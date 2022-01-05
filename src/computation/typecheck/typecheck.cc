#include "typecheck.H"
#include "kindcheck.H"

#include <set>

#include <range/v3/all.hpp>

#include "parser/haskell.H"

#include "immer/map.hpp" // for immer::map

#include "util/set.H"

#include "util/graph.H" // for get_ordered_strong_components( )

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H" // for is_tuple_name( )
#include "computation/operation.H" // for is_non_apply_op( )
#include "computation/module.H"    // for is_qualified_symbol( ), get_module_name( )

#include "unify.H"

#include "alphabetize.H" // for alphabetize_type( ).

namespace views = ranges::views;

using std::vector;
using std::string;
using std::optional;
using std::map;
using std::set;
using std::pair;
using std::shared_ptr;
using std::tuple;

/*
  TODO:
  1. Check that constraints in classes only mention type vars.
  2. Check that constraints in instance heads are of the form Class (Tycon a1 a2 a3..)
  3. Check that constraints in instance contexts satisfy the "paterson conditions"
  4. Construct the list of class -> [tycon] -> instance to record which instances exist
     (like in THIH -- no actual code generation yet)
  5. How are we actually supposed to store the GIE?
  6. Put class methods into global namespace WITH their type -> how?
  7. How do we export stuff?
  8. Make functions to handle class declarations from Figure 11.
     - PROBLEM: How do we pipe in fresh variable names for dictionary extractors?
  9. Make functiosn to handle instance declarations from Figure 12.
     - PROBLEM: How do we pipe in fresh variable names for dictonary functions?
  10. Handle instances in two passes:
     - Can we first the the NAME and TYPE for all the instance variables,
       and second generate the instance dfun bodies?
     - Possibly generating the dfun bodies AFTER the value declarations are done?
     - How do we figure out if the instance contexts can be satisfied in a mutally recursive manner?
  11. Make AST nodes for dictionary abstraction and dictionary application.
     - \(dicts::theta) -> f(dicts)    LambdaDicts = vector<Hs::Var> -> MultiGuardedRHS
     - exp <dicts>                    ApplyDicts  = expression_ref vector<Hs::Var>
     - (superdicts, methods)          Dictionary
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?

  Cleanups:
  1. Implement kinds as Hs::Type
  2. Use Hs::decompose_type_apps( ) to simplify Hs::is_function_type( ),
     like in parser.y:check_kind( ).

 */

/*
  Points about contexts in instances and classes:

  1. Each class declaration must have the form class (C1,C2) => K a1 a2 a3 where
  - CLASS ARGUMENTS must be type variables.
  - CONSTRAINT ARGUMENTS must be type variables, unless FlexibleContexts is enabled.

  2. Each instance declaration must have the form instance (C1,C2) => K (X1 a1 a2) (X2 b1 b2) (X3 c1 c2)
  - CLASS ARGUMENTS must have a single type constructor applied to type variables.
  - CONSTAINTS must satisfy the instance termination rules:

    See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/instances.html#instance-termination

    We can ignore the functional dependencies stuff.  Thus, we just have:

      The Paterson Conditions: for each class constraint (C t1 ... tn) in the context

      1. No type variable has more occurrences in the constraint than in the head
      2. The constraint has fewer constructors and variables (taken together and counting repetitions) than the head
      3. The constraint mentions no type functions. A type function application can in principle expand to a type of arbitrary size, and so are rejected out of hand

  3. We can therefore look up an instance by (K,X1,X2,X3).
  - The more complete form would simply scan ALL instance declarations to find the ONE (or ZERO) matching instances.
  - The slow implementation can extract the constraint from the type.

  Questions about instances:

  Q1. How do we name dictionary extractor & dictionary creator functions?
  A1. Just make up names and record them at the appropriate place.

  Q2. How do we handle mutual recursion between instance methods and value declarations?
  A2. We can process instances before values, but we output instances in the same recursive block as values.

  Q3: Should we translate => to -> during typechecking, OR do we want to do this during desugaring?
  A3: Well.. the typechecker IS making up some variable names... so maybe we do this during type checking?
      Can we tell what GHC is doing?

  Q4: How do we process the instances in the correct order?
  A4: Can we put them into the type groups, and then REVISIT the groups,
      analyzing the instances last in each group?

  Q5. How do we type-check the method bindings, if their types depend on the VALUE bindings?
  A5. We know the types of the instance methods BEFORE we know the function bodies, right?
      So, we should be able to type-check the instance bodies LAST, if we generate the types
      for the value_decls first.

  Q6. How do we check that there are no super-class -> class cycles.

  Q7. Should we generate a special syntax node for dictionary arguments, dictionary applications, etc,
      in order to delay converting => to ->, and converting dictionary applications, etc?
      See Section 4.3:
      - exp <dict>                                       Dictionary application
      - \dicts:theta -> exp                              Dictionary abstraction
      - \\a1..an -> \dicts:theta -> binds in monobinds   Dictionary abstraction (generalized)
      - <dicts,methods>                                  Dictionary record data type
      - \<dicts:theta,methods:GVE> -> exp                Selecting entries from a dictionary
 */

typedef Hs::Type monotype;
typedef Hs::Type overtype;
typedef Hs::Type polytype;
typedef Hs::Type constraint;

// A = out typevar
// T = out monotype
// K = class typecon.  Also a kind of data declaration for dictionaries.

// E = (TCE, TVE, VE = (CVE, GVE, LVE), CE, IE = (GIE, LIE))

// TCE = type constructor environment = tycon -> /\A1 A2 .. An.T

// TVE = type variable environment = tyvar -> A (destination language typevar)

// CE = class environment = class name -> (K,(forall A (context, methods: GVE)))

// IE = instance environment = (GIE, LIE)

// GIE = global instance environment = dfun -> forall A1 A2 .. An. context => K T

// LIE = dvar -> K T

// VE = (CVE, GVE, LVE)
// CVE = constructor value environment = con -> polytype
// GVE = global value environment      = var -> polytype
// LVE = local  value environment      = var -> monotype

typedef value_env constr_env;

// The GIE does NOT allow free type variables.
struct instance_info
{
    // How do we get the kind into the type vars?
    vector<Hs::TypeVar> type_vars;
    Hs::Context context;
    string class_name;
    std::vector<Hs::Type> argument_types;

    string dfun_name;

    // forall <type_vars> . context => class_name argument_types[0] arguments[1] .. argument_types[n01]
    Hs::Type dfun_type() const
    {
        Hs::TypeCon class_con({noloc, class_name}); // whats the kind?
        return Hs::ForallType(type_vars, Hs::ConstrainedType(context, make_tyapps(class_con, argument_types)));
    }
};

global_value_env apply_subst(const substitution_t& s, const value_env& env1)
{
    global_value_env env2;
    for(auto& [x,type]: env1)
        env2 = env2.insert({x, apply_subst(s,type)});
    return env2;
}

// Wait, actually don't we assume that the value decls are divided into self-referencing binding groups, along with explicit signatures?
// We would also need: infix declarations, default declarations, ???
// I guess this is AFTER rename, so declarations have been un-infixed, and we could (theoretically) represent each function as something like [([pat],grhs)]
// SHOULD we actually translate each function to (say) a list of ([pat],ghrs)?  How do we store 
//
// typecheck_module(vector<ClassDecl>, vector<DataDecl>, vector<TypeSyonymDecl>, vector<InstanceDecl>, vector<ValueDecl>)
// {
//    Kindcheck(classdecls, data_decls, type_decls);
//
//
// }

struct typechecker_state
{
    int next_var_index = 1;

    int next_tvar_index = 1;

    constr_env con_info;

    std::string mod_name;

    typechecker_state(const string& s, const constr_env& ce)
        :con_info(ce),
         mod_name(s)
        { }

    Hs::Type bool_type() const { return Hs::TypeCon({noloc,"Data.Bool.True"}); }

    Hs::Type num_type() const { return Hs::TypeCon({noloc,"Num#"}); }

    Hs::Type char_type() const { return Hs::TypeCon({noloc,"Char#"}); }

    pair<Hs::Type, vector<Hs::Type>> constr_types(const Hs::Con&);

    Hs::Var fresh_var(const std::string& s, bool qualified)
    {
        string name = "$"+s+std::to_string(next_var_index);
        if (qualified)
            name = mod_name + "." + name;
        Hs::Var x({noloc, name});
        x.index = next_var_index++;
        return x;
    }

    Hs::Var fresh_var(bool qualified)
    {
        return fresh_var("v", qualified);
    }

    Hs::TypeVar fresh_type_var() {
        Hs::TypeVar tv({noloc, "t"+std::to_string(next_tvar_index)});
        tv.index = next_tvar_index;
        next_tvar_index++;
        return tv;
    }

    Hs::TypeVar named_type_var(const string& name)
    {
        Hs::TypeVar tv({noloc, name+"_"+std::to_string(next_tvar_index)});
        tv.index = next_tvar_index;
        next_tvar_index++;
        return tv;
    }

    Hs::Type instantiate(const Hs::Type& t);

    pair<substitution_t, local_value_env>
    infer_quals_type(const global_value_env& env, const vector<Hs::Qual>& quals);

    pair<substitution_t, local_value_env>
    infer_qual_type(const global_value_env& env, const Hs::Qual& qual);

    pair<substitution_t, local_value_env>
    infer_guard_type(const global_value_env& env, const Hs::Qual& guard);

    pair<Hs::Type, local_value_env>
    infer_pattern_type(const Hs::Pattern& pat);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const expression_ref& exp);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::GuardedRHS&);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::MultiGuardedRHS&);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::MRule&);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::Match&);

    pair<substitution_t, global_value_env>
    infer_type_for_decls(const global_value_env& env, const Hs::Decls& E);

    pair<substitution_t, global_value_env>
    infer_type_for_decls(const global_value_env& env, const Hs::Binds& binds);

    tuple<global_value_env, global_instance_env, class_env, Hs::Binds>
    infer_type_for_classes(const Hs::Decls& decls, const type_con_env& tce);

    tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
    infer_type_for_class(const type_con_env& tce, const Hs::ClassDecl& class_decl);

    // Figure 12
    global_instance_env
    infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in);

    // Figure 12
    global_instance_env
    infer_type_for_instance1(const Hs::InstanceDecl& instance_del, const class_env& ce, const global_instance_env& gie_in);

    // Figure 12
    Hs::Decls
    infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in);

    // Figure 12
    Hs::Decls
    infer_type_for_instance2(const Hs::InstanceDecl& instance_del, const class_env& ce, const global_instance_env& gie_in);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    Hs::Binds get_dicts(const global_instance_env& gie, const local_instance_env& lie1, const local_instance_env& lie2);
};

set<Hs::TypeVar> free_type_variables(const Hs::Type& t);

pair<Hs::Type, vector<Hs::Type>> typechecker_state::constr_types(const Hs::Con& con)
{
    auto& con_name = unloc(con.name);

    if (con_name == ":")
    {
        Hs::Type a = fresh_type_var();
        return {Hs::ListType(a),{a,Hs::ListType(a)}};
    }
    else if (con_name == "[]")
    {
        Hs::Type a = fresh_type_var();
        return {Hs::ListType(a),{a,Hs::ListType(a)}};
    }
    else if (is_tuple_name(con_name))
    {
        int n = tuple_arity(con_name);
        vector<Hs::Type> types;
        for(int i=0;i<n;i++)
            types.push_back(fresh_type_var());
        return {Hs::TupleType(types),types};
    }

    // 1. Find the data type
    if (not con_info.count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;
    auto con_type = instantiate(con_info.at(con_name));
    vector<Hs::Type> field_types;

    while(auto f = is_function_type(con_type))
    {
        auto [t1,t2] = *f;
        field_types.push_back(t1);
        con_type = t2;
    }
    auto object_type = con_type;

    return {object_type, field_types};
}

std::set<Hs::TypeVar> free_type_variables(const Hs::Type& t)
{
    return free_type_VARS(t);
}

std::set<Hs::TypeVar> free_type_variables(const global_value_env& env)
{
    std::set<Hs::TypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_type_variables(type));
    return free;
}

expression_ref generalize(const global_value_env& env, const expression_ref& monotype)
{
    auto ftv1 = free_type_variables(monotype);
    auto ftv2 = free_type_variables(env);
    for(auto tv: ftv2)
        ftv1.erase(tv);

    return Hs::ForallType(ftv1 | ranges::to<vector>, monotype);
}

expression_ref typechecker_state::instantiate(const expression_ref& t)
{
    substitution_t s;
    auto t2 = t;
    while(auto fa = t2.to<Hs::ForallType>())
    {
        for(auto& tv: fa->type_var_binders)
        {
            auto new_tv = fresh_type_var();
            new_tv.kind = tv.kind;
            s = s.insert({tv,new_tv});
        }
        t2 = fa->type;
    }
    return apply_subst(s,t2);
}

pair<substitution_t, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const Hs::Binds& binds)
{
    substitution_t s;
    auto env2 = env;
    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [s1, binders1] = infer_type_for_decls(env2, decls);
        env2 = plus_prefer_right(env2, binders1);
        binders = plus_no_overlap(binders, binders1);
        s = compose(s1, s);
    }
    binders = apply_subst(s, binders);
    return {s, binders};
}

pair<substitution_t,global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const Hs::Decls& decls)
{
    // 1. Add each let-binder to the environment with a fresh type variable
    value_env binder_env;

    vector<pair<Hs::Type,global_value_env>> decl_types;
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            Hs::Type type = fresh_type_var();
            local_value_env lve;
            auto& name = unloc(fd->v.name);
            lve = lve.insert({name,type});
            decl_types.push_back({type, lve});
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto [type, lve] = infer_pattern_type(pd->lhs);
            decl_types.push_back({type,lve});
        }
        auto& [type,lve] = decl_types.back();
        binder_env = plus_no_overlap(binder_env, lve);
    }
    auto env2 = plus_prefer_right(env, binder_env);

    // 2. Infer the types of each of the x[i]
    substitution_t s;
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto& name = unloc(fd->v.name);
            auto lhs_type = env2.at(name);
            auto [s2, rhs_type] = infer_type(env2, fd->match);
            s = compose(s2, compose(unify(lhs_type, rhs_type), s));
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto [lhs_type, lve] = infer_pattern_type(pd->lhs);
            auto [s2, rhs_type] = infer_type(env2, pd->rhs);

            s = compose(s2, compose(unify(lhs_type, rhs_type), s));
        }

        binder_env = apply_subst(s, binder_env);
        env2 = apply_subst(s, env2);
    }

    // 3. Generalize each type over variables not in the *original* environment
    value_env generalized_binder_env;
    for(auto& [var,type]: binder_env)
        generalized_binder_env = generalized_binder_env.insert({var,generalize(env, type)});

    return {s, generalized_binder_env};
}

// Figure 24. Rules for patterns
pair<Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat)
{
    // TAUT-PAT
    if (auto x = pat.to<Hs::Var>())
    {
        Hs::Type type = fresh_type_var();
        local_value_env lve;
        auto& name = unloc(x->name);
        lve = lve.insert({name, type});
	return { type , lve };
    }
    // CONSTR-PAT
    else if (auto con = pat.head().to<Hs::Con>())
    {
        local_value_env lve;
        vector<Hs::Type> types;
        for(auto& pat: pat.copy_sub())
        {
            auto [t1,lve1] = infer_pattern_type(pat);
            types.push_back(t1);
            lve = plus_no_overlap(lve, lve1);
        }
        substitution_t s;
        auto [type,field_types] = constr_types(*con);

        assert(field_types.size() == pat.size());

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
        {
            auto s1 = unify(types[i], field_types[i]);
            s = compose(s1, s);
        }

        lve = apply_subst(s, lve);
        type = apply_subst(s, type);
        return {type, lve};
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto [t,lve] = infer_pattern_type(ap->pattern);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {t,lve};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
        return infer_pattern_type(lp->pattern);
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
        return infer_pattern_type(sp->pattern);
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_type_var();
        return {tv,{}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        local_value_env lve;
        Hs::Type t = fresh_type_var();
        substitution_t s;
        for(auto& element: l->elements)
        {
            auto [t1,lve1] = infer_pattern_type(element);
            auto s1 = unify(t, t1);
            s = compose(s1,s);
            lve = plus_no_overlap(lve, lve1);
        }
        t = apply_subst(s, t);
        lve = apply_subst(s, lve);
        return {t, lve};
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: t->elements)
        {
            auto [t1, lve1] = infer_pattern_type(element);
            types.push_back(t1);
            lve = plus_no_overlap(lve, lve1);
        }
        return {Hs::TupleType(types), lve};
    }
    // ???
    else if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double())
        return {num_type(),{}};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
    
}


// Figure 22. Rules for quals
//
// The implementation is rather... different?
// * the original figure doesn't have let quals.
// * the original figure seems to assume that quals only occur in list comprehensions?

pair<substitution_t,value_env>
typechecker_state::infer_quals_type(const global_value_env& env, const vector<Hs::Qual>& quals)
{
    substitution_t s;
    auto env2 = env;
    local_value_env binders;
    for(auto& qual: quals)
    {
        auto [qual_s, qual_binders] = infer_qual_type(env2, qual);
        s = compose(qual_s, s);
        env2 = plus_prefer_right(env2, qual_binders);
        binders = plus_prefer_right(binders, qual_binders);
    }
    binders = apply_subst(s, binders);
    return {s, binders};
}

pair<substitution_t,value_env>
typechecker_state::infer_qual_type(const global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto [cond_s, cond_type] = infer_type(env, sq->exp);
        auto s2 = unify( cond_type, bool_type() );
        auto s = compose(s2, cond_s);
        return {s, {}};
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        // pat <- exp
        auto [pat_type,lve] = infer_pattern_type(pq->bindpat);
        auto [exp_s,exp_type] = infer_type(env, pq->exp);
        // type(pat) = type(exp)
        auto s3 = unify(Hs::ListType(pat_type), exp_type);
        auto s = compose(s3, exp_s);
        lve = apply_subst(s, lve);
        return {s, lve};
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        return infer_type_for_decls(env, unloc(lq->binds));
    }
    else
        std::abort();
}


pair<substitution_t,value_env>
typechecker_state::infer_guard_type(const global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto [cond_s, cond_type] = infer_type(env, sq->exp);
        auto s2 = unify( cond_type, bool_type() );
        auto s = compose(s2, cond_s);
        return {s, {}};
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        // pat <- exp
        auto [pat_type,lve] = infer_pattern_type(pq->bindpat);
        auto [exp_s,exp_type] = infer_type(env, pq->exp);
        // type(pat) = type(exp)
        auto s3 = unify(pat_type,exp_type);
        auto s = compose(s3, exp_s);
        lve = apply_subst(s, lve);
        return {s, lve};
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        return infer_type_for_decls(env, unloc(lq->binds));
    }
    else
        std::abort();
}


// Figure 25. Rules for match, mrule, and grhs
pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::GuardedRHS& rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty()) return infer_type(env, rhs.body);

    // Fig 25. GUARD
    auto guard = rhs.guards[0];
    auto [s1, env1] = infer_guard_type(env, guard);
    auto env2 = plus_prefer_right(env, env1);

    auto rhs2 = rhs;
    rhs2.guards.erase(rhs2.guards.begin());
    auto [s2,t2] = infer_type(env2, rhs2);
    auto s = compose(s2, s1);

    Hs::Type type = apply_subst(s, t2);
    return {s, type};
}

// Fig 25. GUARD-OR
pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::MultiGuardedRHS& rhs)
{
    substitution_t s;
    Hs::Type type = fresh_type_var();

    auto env2 = env;
    if (rhs.decls)
    {
        auto [s1, binders] = infer_type_for_decls(env, unloc(*rhs.decls));
        env2 = plus_prefer_right(env, binders);
        s = compose(s1, s);
    }

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto [s1,t1] = infer_type(env2, guarded_rhs);
        auto s2 = unify(t1,type);
        s = compose(s2,compose(s1,s));
    }
    type = apply_subst(s, type);
    return {s, type};
};

pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::MRule& rule)
{

    if (rule.patterns.empty())
        return infer_type(env, rule.rhs);
    else
    {
        auto [t1, lve1] = infer_pattern_type(rule.patterns.front());
        auto env2 = plus_no_overlap(env, lve1);

        // Remove the first pattern in the rule
        auto rule2 = rule;
        rule2.patterns.erase(rule2.patterns.begin());

        auto [s, t2] = infer_type(env2, rule2);
        t1 = apply_subst(s, t1);

        Hs::Type type = make_arrow_type(t1,t2);

        return {s, type};
    }
}

pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::Match& m)
{
    substitution_t s;
    Hs::Type result_type = fresh_type_var();

    for(auto& rule: m.rules)
    {
        auto [s1,t1] = infer_type(env, rule);
        auto s2 = unify(result_type, t1);
        s = compose(s2,compose(s1,s));
        result_type = apply_subst(s, result_type);
    }

    return {s,result_type};
}

pair<substitution_t,Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const expression_ref& E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto& x_name = unloc(x->name);
        auto sigma = env.find( x_name );

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x->print()<<"'";

        auto tau = instantiate(*sigma);

        return {{},tau};
    }
    else if (E.is_int() or E.is_double() or E.is_log_double())
        return {{},num_type()};
    else if (E.is_char())
        return {{},char_type()};
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_type_var();
        auto L = *l;
        substitution_t s;
        for(auto& element: L.elements)
        {
            auto [s1, t1] = infer_type(env, element);
            auto s2 = unify(t1, element_type);
            s = compose(s2, compose(s1, s));
        }
        element_type = apply_subst(s, element_type);
        return {s, Hs::ListType(element_type)};
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        substitution_t s;
        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [s1, element_type] = infer_type(env, element);
            s = compose(s1, s);
            element_types.push_back( element_type );
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        result_type = apply_subst(s, result_type);
        return {s, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];
        substitution_t s;

        auto [s1,t1] = infer_type(env,e1);

        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_type_var();

            // This is now done by the previous iteration of the loop!
            // (s1, t1) <- infer env e1
            // auto [s1,t1] = infer_type(env, e1);

            // (s2, t2) <- infer (apply s1 env) e2
            auto [s2,t2] = infer_type(apply_subst(s1,env), e2);

            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            auto s3 = unify (apply_subst(s2,t1), make_arrow_type(t2,tv));

            s1 = compose(s3,compose(s2,s1));
            t1 = apply_subst(s3,tv);
        }

        // This is now done by the setup for the next loop iteration.
        // return {compose(s3,compose(s2,s1)), apply_subst(s3,tv)};
        return {s1,t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto rule = Hs::MRule{lam->args, lam->body};

        return infer_type(env, rule);
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        // 1. Extend environment with types for decls, get any substitutions
        auto [s_decls, env_decls] = infer_type_for_decls(env, unloc(let->binds));
        auto env2 = plus_no_overlap(env_decls, env);

        // 2. Compute type of let body
        auto [s_body, t_body] = infer_type(env2, unloc(let->body));

        // return (s1 `compose` s2, t2)
        return {compose(s_body, s_decls), t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        auto [object_type, field_types] = constr_types(*con);

        substitution_t s;
        auto env2 = env;
        vector<Hs::Type> arg_types;
        for(int i=0; i<E.size(); i++)
        {
            auto [s_i, t_i] = infer_type(env2, E.sub()[i]);
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            auto s2_i = unify( field_types[i], t_i);

            s = compose(compose(s2_i,s_i), s);
            env2 = apply_subst(s_i, env2);
        }

        return {s, apply_subst(s, object_type)};
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        // 1. Determine object type
        auto [s1, object_type] = infer_type(env, case_exp->object);
        auto env2 = apply_subst(s1, env);

        // 2. Determine data type for object from patterns.
        Hs::Match match;
        for(auto& alt: case_exp->alts)
        {
            auto& [pattern, body] = unloc(alt);
            match.rules.push_back(Hs::MRule{{pattern},body});
        }

        auto [s2, match_type] = infer_type(env2, match);

        Hs::Type result_type = fresh_type_var();

        auto s3 = unify( make_arrow_type(object_type,result_type), match_type );

        auto s = compose(s3, compose(s2, s1));

        result_type = apply_subst(s, result_type);

        return {s, result_type};
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto [cond_s, cond_type] = infer_type(env, unloc(if_exp->condition));
        auto [tbranch_s, tbranch_type] = infer_type(env, unloc(if_exp->true_branch));
        auto [fbranch_s, fbranch_type] = infer_type(env, unloc(if_exp->false_branch));

        auto s2 = unify(cond_type, bool_type());
        auto s3 = unify(tbranch_type, fbranch_type);

        auto s = compose(s3, compose(s2, compose(fbranch_s, compose(tbranch_s, cond_s))));

        auto result_type = apply_subst(s, tbranch_type);
        return {s, result_type};
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto [quals_s, quals_binders] = infer_quals_type(env, lcomp->quals);
        auto [exp_s, exp_type] = infer_type(plus_prefer_right(env, quals_binders), lcomp->body);
        auto s = compose(exp_s, quals_s);
        Hs::Type result_type = apply_subst(s, Hs::ListType(exp_type));
        return {s, result_type};
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
    // PROBLEM: the ENUM rules actually take any type t with an Enum t instance.
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});

        // PROBLEM: Do we need to desugar these here, in order to plug in the dictionary?
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_then, t_then] = infer_type(env, l->then);
        auto s2 = unify(t,t_then);
        s = compose(s2, compose(s_then,s));
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_to, t_to] = infer_type(env, l->to);
        auto s2 = unify(t,t_to);
        s = compose(s2, compose(s_to,s));
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_then, t_then] = infer_type(env, l->then);
        auto s2 = unify(t,t_then);
        s = compose(s2, compose(s_then,s));
        t = apply_subst(s,t);

        auto [s_to, t_to] = infer_type(env, l->to);
        auto s3 = unify(t,t_to);
        s = compose(s3, compose(s_to,s));
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    else
        throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

Hs::Type remove_top_level_foralls(Hs::Type t)
{
    while(auto fa = t.to<Hs::ForallType>())
        t = fa->type;
    return t;
}

constr_env get_constructor_info(const Hs::Decls& decls, const type_con_env& tce)
{
    constr_env cve;

    kindchecker_state ks(tce);

    for(auto& decl: decls)
    {
        auto d = decl.to<Hs::DataOrNewtypeDecl>();
        if (not d) continue;

        auto constr_map = ks.type_check_data_type(*d);
        for(auto& [name, type]: constr_map)
            cve = cve.insert({name,type});
    }

    return cve;
}

tuple<global_value_env, global_instance_env, class_env, Hs::Binds> typechecker_state::infer_type_for_classes(const Hs::Decls& decls, const type_con_env& tce)
{
    global_value_env gve;
    global_instance_env gie;
    class_env ce;
    Hs::Binds binds;

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto [gve1, gie1, class_info, class_decls] = infer_type_for_class(tce, *c);

        gve = plus_no_overlap(gve, gve1);
        gie = plus_no_overlap(gie, gie1);
        ce.insert({class_info.name, class_info});
        binds.push_back(class_decls);
    }

    return {gve, gie, ce, binds};
}

Hs::Type type_check_class_method_type(kindchecker_state& K, Hs::Type type, const Hs::Type& constraint)
{
    // 1. Bind type parameters for type declaration
    K.push_type_var_scope();

    std::optional<Hs::Context> context;

    // 2. Find the unconstrained type
    auto unconstrained_type = type;
    if (unconstrained_type.is_a<Hs::ConstrainedType>())
    {
        auto& ct = unconstrained_type.as_<Hs::ConstrainedType>();
        context = ct.context;
        unconstrained_type = ct.type;
    }

    // 3. Find the NEW free type variables
    auto new_ftvs = free_type_VARS(unconstrained_type);
    vector<Hs::TypeVar> to_erase;
    for(auto& type_var: new_ftvs)
        if (K.type_var_in_scope(type_var))
            to_erase.push_back(type_var);
    for(auto& type_var: to_erase)
        new_ftvs.erase(type_var);

    // 4. Bind fresh kind vars to new type variables
    for(auto& ftv: new_ftvs)
    {
        auto a = K.fresh_kind_var();
        K.bind_type_var(ftv,a);
    }

    // 5. Check the context
    if (context)
        K.kind_check_context(*context);

    // 6. Check the unconstrained type and infer kinds.
    K.kind_check_type_of_kind(unconstrained_type, make_kind_star());

    // 7. Bind fresh kind vars to new type variables
    vector<Hs::TypeVar> new_type_vars;
    for(auto& type_var: new_ftvs)
    {
        auto type_var_with_kind = type_var;
        type_var_with_kind.kind = replace_kvar_with_star( K.kind_for_type_var(type_var) );
        new_type_vars.push_back( type_var_with_kind );
    }

    // Don't we need to simplify constraints if we do this?
    type = add_constraints({constraint}, type);

    type = add_forall_vars(new_type_vars, type);
    
    // 6. Unbind type parameters
    K.pop_type_var_scope();

    return type;
}

// OK, so
// * global_value_env    = name         :: forall a: class var => signature (i.e. a-> b -> a)
// * global_instance_env = made-up-name :: forall a: class var => superclass var
// * Hs::Decls           = { name         = \dict -> case dict of (_,_,method,_,_) -> method }
//                       = { made-up-name = \dict -> case dict of (superdict,_,_,_,_) -> superdict }

tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
typechecker_state::infer_type_for_class(const type_con_env& tce, const Hs::ClassDecl& class_decl)
{
    kindchecker_state K(tce);

    auto& name = class_decl.name;

    class_info cinfo;
    cinfo.type_vars = class_decl.type_vars;
    cinfo.name = name;
    cinfo.emitted_name = "class$"+name; // FIXME: only modify name after qualifier?  Just prefix d?
    cinfo.context = class_decl.context;

    // Bind type parameters for class
    K. push_type_var_scope();

    // a. Look up kind for this data type.
    kind k = K. kind_for_type_con(name);  // FIXME -- check that this is a class?

    // b. Put each type variable into the kind.
    vector<Hs::TypeVar> class_typevars;
    for(auto& tv: class_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        K.bind_type_var(tv, ka.k1);

        // record a version of the var with that contains its kind
        auto tv2 = tv;
        tv2.kind = ka.k1;
        class_typevars.push_back(tv2);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kconstraint());

    // d. construct the constraint
    Hs::Type constraint = Hs::TypeCon(Unlocated(class_decl.name));
    for(auto& tv: class_typevars)
        constraint = Hs::TypeApp(constraint, tv);

    // e. handle the class methods
    global_value_env gve;
    if (class_decl.decls)
    {
        for(auto& [name, type]: unloc(*class_decl.decls).signatures)
        {
            Hs::Type method_type = type_check_class_method_type(K, type, constraint);

            method_type = add_forall_vars(class_typevars, method_type);

            gve = gve.insert({name, method_type});
        }
    }

    K.pop_type_var_scope();

    // OK, so now we need to
    //   (a) determine names for dictionary extractors.
    //   (b) determine an order for all the fields.
    //   (c) synthesize field accessors and put them in decls

    global_instance_env gie;
    for(auto& constraint_: cinfo.context.constraints)
    {
        auto get_dict = fresh_var("get_dict", true);
        // Should this be a function arrow?
        Hs::Type type = add_constraints({constraint}, constraint_);
        // Could we be adding too many forall vars?
        type = add_forall_vars(class_typevars, type);
        gie = gie.insert({unloc(get_dict.name), type});
    }
    cinfo.fields = plus_no_overlap(gve, gie);

    Hs::Decls decls;

    vector<Hs::Type> types;
    for(auto& [name,type]: cinfo.fields)
        types.push_back(type);
    Hs::Type dict_type = Hs::TupleType(types);

    int i = 0;
    for(auto& [name,type]: cinfo.fields)
    {
        // body = \dict -> case dict of (_,field,_,_) -> field

        // dict
        Hs::Var dict({noloc,"dict"});
        // field
        Hs::Var field({noloc,"field"});

        // (_,field,_,_)
        vector<Hs::Pattern> pats(cinfo.fields.size(), Hs::WildcardPattern());
        pats[i] = field;

        // (,field,_,_) -> field
        Hs::Alt alt{Hs::Tuple(pats),Hs::SimpleRHS({noloc,field})};

        // case dict of (_,field,_,_) -> field
        Hs::CaseExp case_exp(dict,Hs::Alts({{noloc,alt}}));

        // dict -> case dict of (_,field,_,_) -> field
        Hs::MRule rule{{dict},Hs::SimpleRHS({noloc,case_exp})};
        Hs::Match m{{rule}};

        // f = dict -> case dict of (_,field,_,_) -> field
        Hs::Var f({noloc,name});
        decls.push_back( Hs::FunDecl(f,m) );

        i++;
    }

    return {gve,gie,cinfo,decls};
}

Hs::Type extract_class_constraint(Hs::Type type)
{
    // This should only be called on LIEs
    assert(not type.is_a<Hs::ForallType>());

    if (auto c = type.to<Hs::ConstrainedType>())
        return c->type;
    else
        return type;
}

optional<Hs::Var> contains_constraint(const local_instance_env& lie)
{
    return {};
}

// How does this relate to simplifying constraints?
Hs::Binds typechecker_state::get_dicts(const global_instance_env& gie, const local_instance_env& lie1, const local_instance_env& lie2)
{
    Hs::Binds binds;
    for(auto& [name,constraint]: lie2)
    {
        auto [class_con, args] = decompose_type_apps(extract_class_constraint(constraint));
        for(auto& arg: args)
        {
            // name = dvar
            auto [head, type_args] = decompose_type_apps(arg);

            if (auto dvar = contains_constraint(lie1))
            {
                auto decl = Hs::simple_decl(Hs::Var({noloc,name}), *dvar);
                // binds[0].push_back( decl ) ?
            }
            else if (auto k = head.to<Hs::TypeCon>())
            {
                // Look up instance for K (head a1 a2) in gie -- there should be an instance defined for it.
            }
            else if (auto a = constraint.is_a<Hs::TypeVar>())
            {
                // There might be another assumption in LIE1 (or LIE2?) of the form dvar' :: k' a
                // Then we could extract a as a superclass for k' a.

                // Uh.... Is this basically simplifying the context?
            }
            else
                std::abort();
        }
    }
    return binds;
}

global_instance_env
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl, const class_env& ce, const global_instance_env& gie_in)
{
    auto [class_head, monotypes] = decompose_type_apps(inst_decl.constraint);

    // Premise #1: Look up the info for the class
    optional<class_info> cinfo;
    if (auto tc = class_head.to<Hs::TypeCon>())
    {
        // Check that this is a class, and not a data or type?
        auto class_name = unloc(tc->name);
        if (not ce.count(class_name))
            throw myexception()<<"In instance '"<<inst_decl.constraint<<"': no class '"<<class_name<<"'!";
        cinfo = ce.at(class_name);
    }
    else
        throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<class_head<<" is not a class!";


    // Premise #2: Find the type vars mentioned in the constraint.
    set<Hs::TypeVar> type_vars;
    // Premise #4: the monotype must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& monotype: monotypes)
    {
        auto [a_head, a_args] = decompose_type_apps(monotype);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type!";

        types.push_back(*tc);

        // Add distinct type variables
        for(auto& a_arg: a_args)
        {
            auto tv = a_arg.to<Hs::TypeVar>();

            if (not tv)
                throw myexception()<<"In instance for '"<<inst_decl.constraint<<"' for type '"<<monotype<<"': "<<a_arg<<" is not a type variable!";

            if (type_vars.count(*tv))
                throw myexception()<<"Type variable '"<<tv->print()<<"' occurs twice in constraint '"<<inst_decl.constraint<<"'";

            type_vars.insert(*tv);
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `monotype`
    for(auto& tv: free_type_VARS(inst_decl.context))
    {
        if (not type_vars.count(tv))
            throw myexception()<<"Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in '"<<inst_decl.constraint<<"'";
    }

    auto dfun = fresh_var("dfun");
    Hs::Type inst_type = add_constraints(inst_decl.context.constraints, inst_decl.constraint);
    inst_type = add_forall_vars( free_type_VARS(inst_type) | ranges::to<vector>, inst_type);
    global_instance_env gie;
    gie = gie.insert( { unloc(dfun.name), inst_type } );
    return gie;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
global_instance_env
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in)
{
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto gie = infer_type_for_instance1(*I, ce, gie_in);

            gie_inst = plus_no_overlap(gie_inst, gie);
        }
    }
    return gie_inst;
}

expression_ref tuple_from_value_env(const value_env& venv)
{
    vector<expression_ref> elements;
    for(auto& [name,type]: venv)
        elements.push_back( Hs::Var({noloc, name}) );

    return Hs::Tuple(elements);
}

Hs::Decls
typechecker_state::infer_type_for_instance2(const Hs::InstanceDecl& inst_decl, const class_env& ce, const global_instance_env& gie_in)
{
    auto [class_head, monotypes] = decompose_type_apps(inst_decl.constraint);

    // Premise #1: Look up the info for the class
    optional<class_info> cinfo;
    if (auto tc = class_head.to<Hs::TypeCon>())
    {
        // Check that this is a class, and not a data or type?
        auto class_name = unloc(tc->name);
        if (not ce.count(class_name))
            throw myexception()<<"In instance '"<<inst_decl.constraint<<"': no class '"<<class_name<<"'!";
        cinfo = ce.at(class_name);
    }
    else
        throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<class_head<<" is not a class!";


    // Premise #2: Find the type vars mentioned in the constraint.
    set<Hs::TypeVar> type_vars;
    // Premise #4: the monotype must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& monotype: monotypes)
    {
        auto [a_head, a_args] = decompose_type_apps(monotype);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type!";

        types.push_back(*tc);

        // Add distinct type variables
        for(auto& a_arg: a_args)
        {
            auto tv = a_arg.to<Hs::TypeVar>();

            if (not tv)
                throw myexception()<<"In instance for '"<<inst_decl.constraint<<"' for type '"<<monotype<<"': "<<a_arg<<" is not a type variable!";

            if (type_vars.count(*tv))
                throw myexception()<<"Type variable '"<<tv->print()<<"' occurs twice in constraint '"<<inst_decl.constraint<<"'";

            type_vars.insert(*tv);
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `monotype`
    for(auto& tv: free_type_VARS(inst_decl.context))
    {
        if (not type_vars.count(tv))
            throw myexception()<<"Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in '"<<inst_decl.constraint<<"'";
    }

    // Premise 6: build a local instance environment from the context
    local_instance_env lie;
    int i=0;
    for(auto& constraint: inst_decl.context.constraints)
    {
        string dict_name = "dict"+std::to_string(i+1);
        Hs::Var dict({noloc, dict_name});
        lie = lie.insert({dict_name,constraint});
    }

    // Premise 7:
    local_instance_env lie_super;
    Hs::Binds binds_super = get_dicts(gie_in, lie, lie_super);

    // Premise 8:
    Hs::Binds binds_methods;
    auto dfun = fresh_var("dfun");

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdicts,methods>
    expression_ref dict1 = tuple_from_value_env(cinfo->fields);
    expression_ref dict2 = tuple_from_value_env(lie);

    expression_ref E = Hs::LetExp( {noloc,binds_methods}, {noloc, dict1} );
    E = Hs::LetExp( {noloc,binds_super}, {noloc,E} );
    E = Hs::LambdaExp({dict2}, E);

    Hs::Decls decls ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto decls_ = infer_type_for_instance2(*I, ce, gie_in);

            for(auto& d: decls_)
                out_decls.push_back(d);
        }
    }
    return out_decls;
}

void typecheck( const string& mod_name, const Hs::ModuleDecls& M )
{
    // 1. Check the module's type declarations, and derives a Type Environment TE_T:(TCE_T, CVE_T)
    //    OK, so datatypes produce a
    //    * Type Constructor Environment (TCE) = tycon -> (kind, arity, method of applying the tycon?)
    //    * Constructor Value Environment (CVE)
    //
    // 2. Check the module's class declarations, produce some translated bindings -> binds_C ( GVE_C, CE_C, GIE_C )

    // TCE_T = type con info, part1
    auto tce = get_tycon_info( M.type_decls );
    for(auto& [tycon,ka]: tce)
    {
        auto& [k,arity] = ka;
        std::cerr<<tycon<<" :: "<<k->print()<<"\n";
    }
    std::cerr<<"\n";

    // CVE_T = constructor types :: map<string, polytype> = global_value_env
    auto constr_info = get_constructor_info(M.type_decls, tce);

    for(auto& [con,type]: constr_info)
    {
        std::cerr<<con<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    //   CE_C  = class name -> class info
    typechecker_state state( mod_name, constr_info );
    auto [gve, class_gie, class_info, class_binds] = state.infer_type_for_classes(M.type_decls, tce);
    // GVE_C = {method -> type map} :: map<string, polytype> = global_value_env

    for(auto& [method,type]: gve)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    for(auto& [method,type]: class_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    std::cerr<<class_binds.print()<<"\n";
    std::cerr<<"\n";

    auto inst_gie = state.infer_type_for_instances1(M.type_decls, class_info, class_gie);

    auto gie = plus_no_overlap(class_gie, inst_gie);

    for(auto& [method,type]: inst_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    // 3. E' = (TCE_T, (CVE_T, GVE_C, LVE={}), CE_C, (GIE_C, LIE={}))

    auto [s,env] = state.infer_type_for_decls(gve, M.value_decls);

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<remove_top_level_foralls(alphabetize_type(t))<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";
}

    // GIE_C = functions to extract sub-dictionaries from containing dictionaries?
    // NOT IMPLEMENTED YET.

    // 3. E' = (TCE_T, (CVE_T, GVE_C, {}), CE_C, (GIE_C,{}))
    //
    // 4. Check the module's instance declarations -> monobinds : GIE_I
    //    These are mutually recursive with the value declarations. ?!?
    //
    // 5. Check the module's value declarations.

    // FIXME: Handle instances.

    // Instances: an instance is a function from dictionaries a dictionaries.
    //    instance (A a, B b) => A (b a) is a function of the form \dict_A_a dict_B_b -> dict_A_(b_a)

    // Q: How are instances grouped?
    // A: Each instance needs to be at-or-after all the types/classes referenced,
    //    Do instances depend on other instances?  Maybe this is check in the context...
    //    e.g. instance Eq a => Eq [a] where

    // See equivalents in GHC Rename/Module.hs
    // We are trying to find strongly connected components of
    //  types, type classes, and instances.

    // Shouldn't instances be AFTER everything?
    // * We only have type class instances (ClsInstDecl), but GHC
    //   also has data family instances and type family instances.

    // GHC looks at types and classes first, then adds instances to the SCCs later.


    // 5. Compute types for functions.

    //   Does the type-checker need to augment all bound variables with their type?

    //   Does the type-checker need to add type lambdas?

    //   Does the type-checker need to specify type arguments to type lambdas?

    //   So, let, lambda, and case would need to specify the type

    // 6. Compute types for class default methods?

    // Q: How are default method declarations handled here?
    //    Do they affect type class resolution?
    //    Do we need to do more work on them when handling value decls?
    // A: I think default methods do not affect the type.

    // See function `rnTyClDecls`, which calls `depAnalTyClDecls`.
    // * Dependency analysis on values can be done by name, since instances are not included.
    // * Code is in GHC.Data.Graph.Directed.

    // I don't think we need to look up "parents" during typechecking unless we are promoting data constructors
    // to types or kinds.

    // For values, each value can have a body decl, a fixity decl, and a signature decl.
    // So we can't use the decl itself as the key -- we have to use something like the name.

    // It looks like GHC rename extracts the "free variables" from everything.
    // For example: rnSrcInstDecl operates on ClsInstD, which wraps ClsInstDecl from Hs/Decl.hs

    // FreeVars = OccEnv ID.  See Core/Opt/OccurAnal.hs.

    // Looks like code for determining inlining


