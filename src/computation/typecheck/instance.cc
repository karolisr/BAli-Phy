#include "typecheck.H"
#include "kindcheck.H"
#include "haskell/ids.H"

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

Hs::Decls TypeChecker::infer_type_for_default_methods(const Hs::ClassDecl& C)
{
    Hs::Decls decls_out;

    auto class_info = class_env().at(C.name);

    for(auto& decl: C.default_method_decls)
    {
        auto FD = decl.as_<Hs::FunDecl>();
        auto method_name = unloc(FD.v.name);
        auto dm = class_info.default_methods.at(method_name);
        FD.v = dm;

        auto [decl2, name, sig_type] = infer_type_for_single_fundecl_with_sig(FD);
        decls_out.push_back(decl2);
    }

//    std::cerr<<"Default method ops:\n";
//    std::cerr<<decls_out.print();
//    std::cerr<<"\n\n";

    return decls_out;
}

Hs::Binds TypeChecker::infer_type_for_default_methods(const Hs::Decls& decls)
{
    Hs::Binds default_method_decls;
    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        default_method_decls.push_back( infer_type_for_default_methods(*c) );
    }
    return default_method_decls;
}

string get_name_for_typecon(const TypeCon& tycon)
{
    auto n = unloc(tycon.name);

    if (n == "[]")
        return "List";
    else if (n == "->")
        return "Func";
    else if (is_tuple_name(n))
    {
        int m = tuple_arity(n);
        return std::to_string(m)+"Tuple";
    }
    else
        return get_unqualified_name(n);
}

void TypeChecker::check_add_type_instance(const Hs::TypeFamilyInstanceEqn& inst, const optional<string>& associated_class, const substitution_t& instance_subst)
{
    context.push_err_context( ErrorContext()<<"In instance '"<<inst.print()<<"':" );
    auto tf_con = desugar(inst.con);
    
    // 1. Check that the type family exists.
    if (not type_fam_info().count( tf_con ))
        throw err_context_exception()<<"  No type family '"<<inst.con.print()<<"'";

    // 2. Get the type family info
    auto& tf_info = type_fam_info().at(tf_con);

    if (tf_info.associated_class)
    {
        // 3. Check for unassociated instances declared for associated classes
        if (not associated_class)
            throw err_context_exception()<<
                "  Can't declare non-associated type instance for type family '"<<inst.con.print()<<"' associated with class '"<<(*tf_info.associated_class)<<"'";

        // 4. Check for instances associated with the wrong class
        if (*tf_info.associated_class != *associated_class)
            throw err_context_exception()<<
                "  Trying to declare type instance in class '"<<*associated_class<<" for family '"<<inst.con.print()<<"' associated with class '"<<(*tf_info.associated_class)<<"'";

        // 5. Check that arguments corresponding to class parameters are the same as the parameter type for the instance.
        for(int i=0;i<tf_info.args.size();i++)
        {
            auto fam_tv = tf_info.args[i];
            if (instance_subst.count(fam_tv))
            {
                auto expected = instance_subst.at(fam_tv);
                if (not same_type( desugar(inst.args[i]), expected))
                    throw err_context_exception()<<
                        "    argument '"<<inst.args[i]<<"' should match instance parameter '"<<expected<<"'";
            }
        }
    }

    // 6. Check that the type family is not closed
    if (tf_info.closed)
        throw err_context_exception()<<
            "  Can't declare additional type instance for closed type family '"<<inst.con.print()<<"'";

    // 7. Check that the type instance has the right number of arguments
    if (inst.args.size() != tf_info.args.size())
        throw err_context_exception()<<
            "    Expected "<<tf_info.args.size()<<" parameters, but got "<<inst.args.size();

    // 8. The rhs may only mention type vars bound on the lhs.
    set<TypeVar> lhs_tvs;
    for(auto& arg: desugar(inst.args))
    {
        for(auto& tv: free_type_variables(arg))
            lhs_tvs.insert(tv);
    }

    for(auto& tv: free_type_variables(desugar(inst.rhs)))
        if (not lhs_tvs.count(tv))
            throw err_context_exception()<<"  rhs variable '"<<tv.print()<<"' not bound on the lhs.";

    // 9. Kind-check the parameters and result type, and record the free type variables.
    TypeFamEqnInfo eqn{ desugar(inst.args), desugar(inst.rhs), lhs_tvs | ranges::to<vector>()};

    // 9a. Bind the free type vars
    kindchecker_state K( tycon_info() );
    K.push_type_var_scope();
    for(auto& tv: eqn.free_tvs)
    {
        assert(not K.type_var_in_scope(tv));
        tv.kind = K.fresh_kind_var();
        K.bind_type_var(tv, *tv.kind);
    }

    // 9b. Kind-check the type vars
    for(int i=0; i<eqn.args.size(); i++)
        K.kind_check_type_of_kind(eqn.args[i], *tf_info.args[i].kind);
    K.kind_check_type_of_kind(eqn.rhs, tf_info.result_kind);

    // 9c. Record the final kinds for the free type vars
    for(int i=0; i<eqn.args.size(); i++)
        eqn.args[i] = K.zonk_kind_for_type(eqn.args[i]);
    eqn.rhs = K.zonk_kind_for_type(eqn.rhs);

    for(auto& tv: eqn.free_tvs)
        tv.kind = replace_kvar_with_star(K.kind_for_type_var(tv));

    // 10. Add the (~) instance to the instance environment
    Type lhs = make_tyapps(tf_con, eqn.args);
    Type constraint = make_equality_constraint(lhs, eqn.rhs);
    Type inst_type = add_forall_vars(eqn.free_tvs, constraint);

    int eqn_id = FreshVarSource::current_index();
    auto dvar = fresh_dvar(constraint);

    instance_env().insert( {dvar, inst_type} );

    // 11. Make up an equation id -- this is the "evidence" for the type family instance.
    tf_info.equations.insert({eqn_id, eqn});

    context.pop_err_context();
}

pair<Core::Var, Type>
TypeChecker::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl)
{
    context.push_err_context( ErrorContext()<<"In instance '"<<inst_decl.constraint<<"':" );

    // 1. Get class name and parameters for the instance
    auto [class_head, class_args] = decompose_type_apps(desugar(inst_decl.constraint));

    // 2. Look up the class info
    auto tc = class_head.to<TypeCon>();
    if (not tc)
        throw err_context_exception()<<"  "<<class_head<<" is not a type constructor!";

    // Check that this is a class, and not a data or type?
    auto class_name = unloc(tc->name);
    if (not class_env().count(class_name))
        throw err_context_exception()<<"  no class '"<<class_name<<"'!";
    auto class_info = class_env().at(class_name);

    // 3. Check that the instance has the right number of parameters
    int N = class_info.type_vars.size();
    if (class_args.size() != class_info.type_vars.size())
        throw err_context_exception()<<"  should have "<<N<<" parameters, but has "<<class_args.size()<<".";

    // 4. Construct the mapping from original class variables to instance variables
    substitution_t instance_subst;
    for(int i = 0; i < N; i++)
        instance_subst = instance_subst.insert( {class_info.type_vars[i], class_args[i]} );

    // 5. Find the type vars mentioned in the constraint.
    set<TypeVar> type_vars = free_type_variables(desugar(inst_decl.constraint));

    // 6. The class_args must be (i) a variable or (ii) a type constructor applied to simple, distinct type variables.
    string tycon_names;
    for(auto& class_arg: class_args)
    {
        if (class_arg.to<TypeVar>())
        {
            tycon_names += "_";
        }
        else
        {
            auto [a_head, a_args] = decompose_type_apps(class_arg);

            if (auto tc = a_head.to<TypeCon>())
                tycon_names += get_name_for_typecon(*tc);
            else
                throw err_context_exception()<<"  '"<<a_head.print()<<"' is not a type constructor!";

            // Now, tc needs to be a data type constructor!
            // With FlexibleInstances, (i) the arguments do NOT have to be variables and (ii) type synonyms are allowed.
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `class_arg`
    for(auto& tv: free_type_variables(desugar(inst_decl.context.constraints)))
    {
        if (not type_vars.count(tv))
            throw err_context_exception()<<"  Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in the instance declaration";
    }


    // Look at associated type instances
    for(auto& inst: inst_decl.type_inst_decls)
        check_add_type_instance(inst, class_name, instance_subst);

    string dfun_name = "d"+get_unqualified_name(class_info.name)+tycon_names;

    auto dfun = get_fresh_var(dfun_name, true);

    //  -- new -- //
    Type inst_type = add_constraints(desugar(inst_decl.context.constraints), desugar(inst_decl.constraint));
    inst_type = check_constraint( inst_type );  // kind-check the constraint and quantify it.

    context.pop_err_context();

    return {dfun, inst_type};
}


// See Tc/TyCl/Instance.hs
// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
vector<pair<Core::Var,Hs::InstanceDecl>>
TypeChecker::infer_type_for_instances1(const Hs::Decls& decls)
{
    GIE gie_inst;

    vector<pair<Core::Var, Hs::InstanceDecl>> named_instances;

    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto [dfun, inst_type] = infer_type_for_instance1(*I);

            named_instances.push_back({dfun, *I});
            gie_inst.insert( {dfun, inst_type} );
        }

        if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
            check_add_type_instance(*TI, {}, {});
    }

//    std::cerr<<"GIE:\n";
//    for(auto& [method,type]: gie_inst)
//    {
//        std::cerr<<method<<" :: "<<type.print()<<"\n";
//    }
//    std::cerr<<"\n";

    for(auto pred: gie_inst)
        instance_env().insert(pred);

    return named_instances;
}

string get_class_for_constraint(const Type& constraint)
{
    auto [class_head, args] = decompose_type_apps(constraint);
    auto tc = class_head.to<TypeCon>();
    assert(tc);
    return unloc(tc->name);
}

// PROBLEM: we also need to know all the instance types to check the entails.
//          so, that part needs to come after a first pass over all instances...
// PROBLEM: we need to types for functions defined in the module...
//          so, typechecking the method bodies needs to come after typechecking the rest of the module.
// FIXME: What stuff do we want to know from infer_type_for_instance1( )?
//        * the dvar name

// Construct superclass dictionary entries from instance constraints

// Construct member function entries.

/* dfun idvar1:instance_constraint1 ... idvar[N]:instance_constraint[N] =
   let dvar1 = <construct superdict1>
   dvar2 = <construct superdictN>

   in let var1 = <body1>
   varM = <bodyM>
   in <dvar1, ..., dvarN, var1, ..., varM>
*/

map<string, Hs::Matches> TypeChecker::get_instance_methods(const Hs::Decls& decls, const global_value_env& members, const string& class_name) const
{
    std::map<string,Hs::Matches> method_matches;
    for(auto& decl: decls)
    {
        auto& fd = decl.as_<Hs::FunDecl>();
        string method_name = unloc(fd.v.name);

        if (not members.count(method_name))
            throw err_context_exception()<<"'"<<method_name<<"' is not a member of class '"<<class_name<<"'";

        if (method_matches.count(method_name))
            throw err_context_exception()<<"method '"<<method_name<<"' defined twice!";

        method_matches.insert({method_name, fd.matches});
    }

    return method_matches;
}

pair<Hs::Decls, Core::Decl>
TypeChecker::infer_type_for_instance2(const Core::Var& dfun, const Hs::InstanceDecl& inst_decl)
{
    context.push_err_context( ErrorContext()<<"In instance '"<<inst_decl.constraint<<"':" );

    // 1. Get instance head and constraints 

    // This could be Num Int or forall a b.(Ord a, Ord b) => Ord (a,b)
    auto inst_type = instance_env().at(dfun);
    // Instantiate it with rigid type variables.
    auto [wrap_gen, instance_tvs, givens, instance_head] = skolemize(inst_type, false);
    auto [instance_class, instance_args] = decompose_type_apps(instance_head);

    // 2. Get the class info
    auto class_name = get_class_for_constraint(instance_head);
    auto class_info = class_env().at(class_name);

    // 3. Get constrained version of the class
    substitution_t subst;
    for(int i=0; i<class_info.type_vars.size(); i++)
        subst = subst.insert({class_info.type_vars[i], instance_args[i]});

    // 4. Get (constrained) superclass constraints
    auto superclass_constraints = class_info.context.constraints;
    for(auto& superclass_constraint: superclass_constraints)
        superclass_constraint = apply_subst(subst, superclass_constraint);

    // 5. Construct binds_super
    auto wanteds = constraints_to_lie(superclass_constraints);
    auto WC = WantedConstraints(wanteds);
    auto decls_super = entails(givens, WC);
    if (not WC.simple.empty())
        throw err_context_exception()<<"Can't derive superclass constraints "<<print(WC.simple)<<" from instance constraints "<<print(givens)<<"!";

    // 7. make some intermediates

    vector<Hs::Expression> dict_entries;
    for(auto& [var,constraint]: wanteds)
        dict_entries.push_back(var);

    // 7. Construct binds_methods
    Hs::Decls decls;

    map<string, Hs::Matches> method_matches;
    method_matches = get_instance_methods( inst_decl.method_decls, class_info.members, class_name );

    string classdict_name = "d" + get_class_name_from_constraint(instance_head);

    // OK, so lets say that we just do \idvar1 .. idvarn -> let ev_binds = entails( )
    for(const auto& [method_name, method_type]: class_info.members)
    {
        auto op = get_fresh_Var("i"+method_name, true);

        dict_entries.push_back( Core::Apply(make_var(op), vars_from_lie<Core::Exp>(givens)) );

        // forall b. Ix b => a -> b -> b
        Type op_type = remove_top_gen(method_type);
        // forall b. Ix b => [x] -> b -> b
        op_type = apply_subst(subst, op_type);
        // forall x. (C1 x, C2 x) => forall b. Ix b => [x] -> b -> b
        op_type = add_forall_vars(instance_tvs,add_constraints(constraints_from_lie(givens), op_type));

        gve = gve.insert( {unloc(op.name), op_type} );

        optional<Hs::FunDecl> FD;
        if (auto it = method_matches.find(method_name); it != method_matches.end())
        {
            FD = Hs::FunDecl(op, it->second);
        }
        else
        {
            if (not class_info.default_methods.count(method_name))
                throw err_context_exception()<<"instance "<<inst_decl.constraint<<" is missing method '"<<method_name<<"'";

            auto dm_var = class_info.default_methods.at(method_name);

            FD = Hs::simple_decl(op, dm_var);
        }
        auto [decl2, _, __] = infer_type_for_single_fundecl_with_sig(*FD);
        decls.push_back(decl2);
    }

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdict_vars,method_vars>
    auto dict = Core::Tuple(dict_entries);

    if (decls_super.size())
        dict = Core::Let( decls_super, dict );

    dict = wrap_gen(dict);

    context.pop_err_context();

    return {decls, pair(dfun, dict)};
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
pair<Hs::Binds, Core::Decls> TypeChecker::infer_type_for_instances2(const vector<pair<Core::Var, Hs::InstanceDecl>>& named_instances)
{
    Hs::Binds instance_method_decls;
    Core::Decls dfun_decls;

    for(auto& [dfun, instance_decl]: named_instances)
    {
        try
        {
            auto [decls, dfun_decl] = infer_type_for_instance2(dfun, instance_decl);

            instance_method_decls.push_back(decls);
            dfun_decls.push_back(dfun_decl);
        }
        catch (myexception& e)
        {
            string header = "In instance '" + instance_decl.constraint.print() + "' ";
//            if (instance_decl.binds and instance_decl.binds->loc)
//                header += " at " + convertToString(*instance_decl.binds->loc);
            header += ":\n";
            e.prepend(header);
            throw;
        }
    }
//    std::cerr<<"\nInstance ops and dfuns:\n";
//    std::cerr<<instance_decls.print();
//    std::cerr<<"\n\n";

    return {instance_method_decls, dfun_decls};
}

bool TypeChecker::instance_matches(const Type& type1, const Type& type2)
{
    auto [_1, _2, head1] = instantiate(type1);
    auto [_3, _4, head2] = instantiate(type2);
    return maybe_match(head1, head2);
}

bool TypeChecker::more_specific_than(const Type& type1, const Type& type2)
{
    // We can get type1 by constraining type2, so type1 is more specific than type2.
    return instance_matches(type2, type1) and not instance_matches(type1, type2);
}

// 1. An instance looks like (forall as.Q1(as) => Q2(as))
// 2. We have some constraints Q3.
// 3. We instantiate the instance with substitutions [as->gs].
// 4. We'd like to check if Q3 ~ [as->gs]Q2, where only the variables gs are allowed to be unified.
// 5. If we give the unification variables gs a higher level, can we guarantee that only
//    gs will be constrained?
// 6. Actually, I don't think so... Suppose that the instance is (Eq Int) and the constraint is
//    Eq a.
// 7. Unless we actually FORBID unification of variables at any higher level, then this won't work.
// 8. Simply forbidding substitution to a deeper depth won't cut it.

optional<pair<Core::Exp,LIE>> TypeChecker::lookup_instance(const Type& target_constraint)
{
    vector<pair<pair<Core::Exp, LIE>,Type>> matching_instances;

    string target_class = get_class_for_constraint(target_constraint);

    for(auto& [dfun, type]: instance_env() )
    {
        auto instance_class = get_class_for_constraint( remove_top_gen(type));

        if (instance_class != target_class) continue;

        auto [_, wanteds, instance_head] = instantiate(type);

        assert(not constraint_is_hnf(instance_head));

        if (not maybe_match(instance_head, target_constraint)) continue;

        auto dfun_exp = Core::Apply(dfun, vars_from_lie<Core::Exp>(wanteds));

        matching_instances.push_back({{dfun_exp, wanteds}, type});
    }

    if (matching_instances.size() == 0)
        return {}; // No matching instances

    vector<pair<pair<Core::Exp, LIE>,Type>> surviving_instances;

    for(int i=0;i<matching_instances.size();i++)
    {
        auto type_i = matching_instances[i].second;

        bool keep = true;
        for(int j=0;keep and j<matching_instances.size();j++)
        {
            if (i == j) continue;

            auto type_j = matching_instances[j].second;

            if (more_specific_than(type_j, type_i))
                keep = false;
        }

        if (keep)
            surviving_instances.push_back(matching_instances[i]);
    }

    if (surviving_instances.size() > 1)
    {
        auto e = myexception()<<"Too many matching instances for "<<target_constraint<<":\n\n";
        for(auto& [_,type]: surviving_instances)
            e<<"  "<<remove_top_gen(type)<<"\n";
        throw e;
    }

    assert(surviving_instances.size() == 1);

    return surviving_instances[0].first;
}

