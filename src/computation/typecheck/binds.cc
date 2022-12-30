#include "typecheck.H"
#include "kindcheck.H"
#include "types.H"
#include "match.H" // for tcMatchesFun

#include "rename/rename.H" // for get_indices_for_names( )

#include "util/set.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;
using std::shared_ptr;


template <typename T>
Type quantify(const T& tvs, const Type& monotype)
{
    if (tvs.empty())
        return monotype;
    else
    {
        for(auto& tv: tvs)
            assert(tv.kind);
        return ForallType(tvs | ranges::to<vector>, monotype);
    }
}

global_value_env sig_env(const map<string, Type>& signatures)
{
    global_value_env sig_env;
    for(auto& [name, type]: signatures)
        sig_env = sig_env.insert({name, type});
    return sig_env;
}

Hs::Binds TypeChecker::infer_type_for_binds_top(Hs::Binds binds)
{
    infer_type_for_binds(binds, true);
    return binds;
}

void TypeChecker::infer_type_for_foreign_imports(vector<Hs::ForeignDecl>& foreign_decls)
{
    global_value_env fte;
    for(auto& f: foreign_decls)
    {
        auto type = check_type( desugar(f.type) );
        fte = fte.insert({f.function_name, type});
    }
    gve += fte;
}

void
TypeChecker::infer_type_for_binds(Hs::Binds& binds, bool is_top_level)
{
    global_value_env sigs;
    signature_env sigs2;
    for(auto& [name,type]: binds.signatures)
    {
        auto type2 = check_type(desugar(type));
        sigs = sigs.insert({name,type2});
        sigs2.insert({name,type2});
    }

    add_binders(sigs);

    for(auto& decls: binds)
        decls = infer_type_for_decls(sigs2, decls, is_top_level);
}

value_env remove_sig_binders(value_env binder_env, const signature_env& signatures)
{
    auto no_sig_binder_env = binder_env;
    for(auto& [name,_]: binder_env)
        if (signatures.count(name))
            no_sig_binder_env = no_sig_binder_env.erase(name);
    return no_sig_binder_env;
}

vector<Hs::Decls> split_decls_by_signatures(const Hs::Decls& decls, const map<string, Type>& signatures)
{
    // 1. Map names to indices
    map<string,int> index_for_name = get_indices_for_names(decls);

    // 2. Figure out which indices reference each other
    vector<vector<int>> referenced_decls;
    for(int i=0;i<decls.size();i++)
    {
        vector<int> refs;
        for(auto& name: get_rhs_free_vars(decls[i]))
        {
            auto it = index_for_name.find(name);

            // Skip if this name isn't one of the ids being defined.
            if (it == index_for_name.end()) continue;

            // Skip if this name has a signature
            if (signatures.count(name)) continue;

            refs.push_back(it->second);
        }
        referenced_decls.push_back( std::move(refs) );
    }

    // 3. Compute strongly-connected components and split
    return split_decls(decls, referenced_decls);
}

Hs::Decls
TypeChecker::infer_type_for_decls(const signature_env& signatures, const Hs::Decls& decls, bool is_top_level)
{
    // The signatures for the binders should already be in the environment.

    auto bind_groups = split_decls_by_signatures(decls, signatures);

    Hs::Decls decls2;
    for(auto& group: bind_groups)
    {
        Note ec;
        ec<<"In recursive group:\n";
        for(auto& decl: group)
        {
            if (auto fd = decl.to<Hs::FunDecl>())
                ec<<"    "<<fd->v.print()<<"\n";
            else if (auto pd = decl.to<Hs::PatDecl>())
                ec<<"    "<<pd->lhs.print()<<"\n";
            else
                std::abort();
        }
        context.push_note(ec);

        auto group_decls = infer_type_for_decls_group(signatures, group, is_top_level);

        context.pop_note();
        
        for(auto& decl: group_decls)
            decls2.push_back(decl);
    }
    return decls2;
}

bool single_fundecl_with_sig(const Hs::Decls& decls, const signature_env& signatures)
{
    if (decls.size() != 1) return false;

    auto& decl = decls[0];

    if (not decl.is_a<Hs::FunDecl>()) return false;

    auto& FD = decl.as_<Hs::FunDecl>();

    auto& name = unloc(FD.v.name);

    return signatures.count(name) > 0;
}

expression_ref
rename_from_bindinfo(expression_ref decl, const map<string, Hs::BindInfo>& bind_infos)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        FD.v = rename_var_from_bindinfo(FD.v, bind_infos);
        return FD;
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        unloc(PD.lhs) = rename_pattern_from_bindinfo(unloc(PD.lhs), bind_infos);
        return PD;
    }
    else
        std::abort();
}

Hs::Decls rename_from_bindinfo(Hs::Decls decls,const map<string, Hs::BindInfo>& bind_infos)
{
    for(auto& decl: decls)
        decl = rename_from_bindinfo(decl, bind_infos);
    return decls;
}

Hs::GenBind mkGenBind(const vector<TypeVar>& tvs,
                      const vector<Core::Var>& dict_vars,
                      const shared_ptr<const Core::Decls>& ev_decls,
                      Hs::Decls decls,
                      const map<string, Hs::BindInfo>& bind_infos)
{
    decls = rename_from_bindinfo(decls, bind_infos);
    return Hs::GenBind(tvs, dict_vars, ev_decls, decls, bind_infos);
}

// Why aren't we using `fixed_type_vars`?
// I guess the deferred constraints that do not mention fixed_type_vars are ambiguous?
pair<LIE, LIE>
classify_constraints(bool restricted, const LIE& lie, const set<MetaTypeVar>& qtvs)
{
    if (restricted) return {lie, {}};

    LIE lie_deferred;
    LIE lie_retained;

    for(auto& constraint: lie)
    {
        if (intersects( free_meta_type_variables(constraint.pred), qtvs ))
            lie_retained.push_back(constraint);
        else
            lie_deferred.push_back(constraint);
    }
    return {lie_deferred, lie_retained};
}

pair<LIE, LIE>
classify_constraints(const LIE& lie, const set<TypeVar>& qtvs)
{
    LIE lie_deferred;
    LIE lie_retained;

    for(auto& constraint: lie)
    {
        if (intersects( free_type_variables(constraint.pred), qtvs ))
            lie_retained.push_back(constraint);
        else
            lie_deferred.push_back(constraint);
    }
    return {lie_deferred, lie_retained};
}

/// Compare to checkSigma, which also check for any skolem variables in the wanteds
tuple<expression_ref, ID, Type>
TypeChecker::infer_type_for_single_fundecl_with_sig(Hs::FunDecl FD)
{
    // Q: Are we getting the monotype correct?

    context.push_note( Note()<<"In function '"<<FD.v.print()<<"'" );

    auto& name = unloc(FD.v.name);

    // 1. skolemize the type -> (tvs, givens, rho-type)
    auto polytype = gve.at(name);
    auto [wrap_gen, tvs, givens, rho_type] =
        skolemize_and(polytype,
                      [&](const Type& rho_type, auto& tcs2) {
                          auto ctx = Hs::FunctionContext(unloc(FD.v.name));
                          tcs2.tcMatchesFun( getArity(FD.matches), Check(rho_type),
                                             [&](const vector<Expected>& arg_types, const Expected& result_type) {return [&](auto& tc) {
                                                 tc.tcMatches(ctx, FD.matches, arg_types, result_type);};});
                      }
            );

    // 2. return GenBind with tvs, givens, body
    Hs::Var inner_id = get_fresh_Var(unloc(FD.v.name),false);

    Type monotype = rho_type;

    Hs::BindInfo bind_info(FD.v, inner_id, monotype, polytype, wrap_gen);

    auto decl = mkGenBind( {}, {}, std::make_shared<Core::Decls>(), Hs::Decls({FD}), {{name, bind_info}} );

    context.pop_note();

    return {decl, name, polytype};
}

bool is_restricted(const map<ID, Type>& signatures, const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Hs::PatDecl>())
            return true;
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            // Simple pattern declaration
            if (fd->matches[0].patterns.size() == 0)
            {
                auto& name = unloc(fd->v.name);
                if (not signatures.count(name)) return true;
            }
        }
    }
    return false;
};

tuple<Type, local_value_env>
TypeChecker::infer_lhs_type(expression_ref& decl, const map<string, Type>& signatures)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        // If there was a signature, we would have called infer_type_for_single_fundecl_with_sig
        assert(not signatures.count(unloc(FD.v.name)));

        local_value_env lve;
        auto type = inferPat(lve, FD.v);
        decl = FD;
        return {type, lve};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        local_value_env lve;
        auto type = inferPat( lve, unloc(PD.lhs), signatures);
        decl = PD;
        return {type, lve};
    }
    else
        std::abort();
}

void TypeChecker::infer_rhs_type(expression_ref& decl, const Expected& rhs_type)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        auto ctx = Hs::FunctionContext(unloc(FD.v.name));
        tcMatchesFun( getArity(FD.matches), rhs_type, [&](const auto& arg_types, const auto& result_type) {return [&](auto& tc) {
            tc.tcMatches(ctx, FD.matches, arg_types, result_type);};}
                        );
        decl = FD;
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        tcRho(PD.rhs, rhs_type);
        decl = PD;
    }
    else
        std::abort();
}

tuple< map<string, Hs::Var>, local_value_env > TypeChecker::tc_decls_group_mono(const signature_env& signatures, Hs::Decls& decls)
{
    // 1. Add each let-binder to the environment with a fresh type variable
    local_value_env mono_binder_env;

    std::map<std::string, Hs::Var> mono_ids;

    vector<Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto [type, lve] = infer_lhs_type( decls[i], signatures );

        lhs_types.push_back(type);
        mono_binder_env += lve;
    }

    for(auto& [name, type]: mono_binder_env)
    {
        Hs::Var mono_id = get_fresh_Var(name, false);
        mono_ids.insert({name, mono_id});

        if (not signatures.count(name))
        {
            mono_local_env = mono_local_env.erase(name);
            mono_local_env = mono_local_env.insert({name,{mono_id, type}});
        }
    }

    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        Note n;
        if (auto FD = decls[i].to<Hs::FunDecl>())
            n<<"In function `"<<FD->v.print()<<"`";
        else if (auto PD = decls[i].to<Hs::PatDecl>())
            n<<"In definition of `"<<unloc(PD->lhs).print()<<"`";

        context.push_note(n);

        infer_rhs_type(decls[i], Check(lhs_types[i]));

        context.pop_note();
    }

    return {mono_ids, mono_binder_env};
}

bool type_is_hnf(const Type& type)
{
    auto [head,args] = decompose_type_apps(type);

    head = follow_meta_type_var(head);

    if (head.is_a<TypeVar>())
        return true;
    else if (head.is_a<MetaTypeVar>())
        return true;
    else if (head.is_a<TypeCon>())
        return false;
    else
        std::abort();
}

// OK:     K a, K (a b), K (a [b]), etc. OK
// NOT OK: K [a], K (a,b), etc. NOT OK.
// Question: for multiparameter type classes, how about i.e. `K Int a`?
bool constraint_is_hnf(const Type& constraint)
{
    auto [class_con, args] = decompose_type_apps(constraint);
    for(auto& arg: args)
        if (not type_is_hnf(arg))
            return false;
    return true;
}



/* NOTE: Constraints can reference variables that are in
 *        (i) ALL types in a recursive group
 *       (ii) SOME-BUT-NOT-ALL types
 *      (iii) NO types.
 *
 * For unrestricted bindings, classes (ii) and (iii) need defaults.
 * For restricted bindings, only class (iii) (I think) needs defaults.
 */

// For the COMPLETELY ambiguous constraints, we should be able to just discard the constraints,
//   after generating definitions of their dictionaries.


// This can't return TypeVar's...
set<MetaTypeVar> TypeChecker::injective_vars_for_type(const Type& type) const
{
    if (auto type2 = filled_meta_type_var(type))
        return injective_vars_for_type(*type2);
    else if (auto type2 = is_type_synonym(type))
        return injective_vars_for_type(*type2);
    else if (type.is_a<TypeVar>())
        return {};
    else if (auto mtv = type.to<MetaTypeVar>())
        return {*mtv};
    else if (auto app = is_type_app(type))
    {
        auto& [head,arg] = *app;
        auto mtvs = injective_vars_for_type(head);
        add(mtvs, injective_vars_for_type(arg));
        return mtvs;
    }
    else if (is_type_fam_app(type))
        return {};
    else if (auto forall = type.to<ForallType>())
        return injective_vars_for_type(forall->type);
    else if (auto constrained = type.to<ConstrainedType>())
    {
        auto mtvs = injective_vars_for_type(constrained->type);
        for(auto& pred: constrained->context.constraints)
            add(mtvs, injective_vars_for_type(pred));
        return mtvs;
    }
    else
        std::abort();
}

/*
  (no_quant_preds, maybe_quant_preds) <- select preds based on restricted & overloaded strings

  taus = list of values in mono_env

  level = current level

  psig_tys = []

  co_vars = []
  co_var_tvs = []

  mono_tvs0 = metatypevars in preds that are NOT quantifiable at this level

  mono_tvs1 = mono_tvs0

  non_ip_candidates = candidates

  mono_tvs2 = closeWrtFunDeps candidates monotvs1

  constrained_tvs = closeWrtFunDeps candidates (metatypevars in no_quant_preds),
                    minus those constrained by vars free in the environment,
                    minus those that are not quantifiable at this level


  closeWrtFunDeps = ???                  
*/


set<MetaTypeVar> TypeChecker::find_fixed_tvs(bool restricted, int level, const vector<Type>& preds, const set<MetaTypeVar>& tvs) const
{
    set<MetaTypeVar> fixed;

    for(auto& tv: tvs)
        if (tv.level() <= level)
            fixed.insert(tv);

    if (restricted)
        add(fixed, free_meta_type_variables(preds));

    // If we have alpha[1] ~ [ beta[2] ], then beta should also be considered fixed.
    for(auto& pred: preds)
    {
        if (auto eq = is_equality_pred(pred))
        {
            auto [t1,t2] = *eq;

            if (auto lvl = unfilled_meta_type_var(t1); lvl and *lvl <= level)
            {
                add( fixed, injective_vars_for_type(t2) );
            }
            else if (auto lvl = unfilled_meta_type_var(t2); lvl and *lvl <= level)
            {
                add( fixed, injective_vars_for_type(t1) );
            }
        }
    }

    return fixed;
}

Hs::BindInfo TypeChecker::compute_bind_info(const string& name, const Hs::Var& mono_id, const set<TypeVar>& qtvs,
                                            const Type& monotype, const signature_env& signatures,
                                            const LIE& lie_retained)
{
    set<TypeVar> qtvs_in_this_type = intersection( qtvs, free_type_variables( monotype ) );

    set<TypeVar> qtvs_unused = qtvs - qtvs_in_this_type;

    // Replace any unused typevars with metavariables
    substitution_t s;
    for(auto& tv: qtvs_unused)
    {
        assert(tv.kind);
        auto new_tv = fresh_meta_type_var(unloc(tv.name), *tv.kind);
        s = s.insert({tv, new_tv});
    }
    auto lie_all = apply_subst(s, lie_retained);

    // Get new dict vars for constraints
    for(auto& constraint: lie_all)
        constraint.ev_var = fresh_dvar(constraint.pred);

    // Any constraints that don't mention type vars of this type are ambiguous.
    // We will put them into the environment in hopes that we can default them later.
    auto [lie_unused, lie_used] = classify_constraints( lie_all, qtvs_in_this_type );
    current_wanteds() += lie_unused;

    auto dict_args = dict_vars_from_lie( lie_used );
    auto tup_dict_args = dict_vars_from_lie( lie_all );
    auto wrap = Core::WrapLambda(dict_args) * Core::WrapApply(tup_dict_args);

    auto constraints_used = preds_from_lie(lie_used);
    Type polytype = quantify( qtvs_in_this_type, add_constraints( constraints_used, monotype ) );
    if (signatures.count(name))
    {
        auto sub_polytype = polytype;
        polytype = signatures.at(name);
        wrap = subsumptionCheck(TypeConvertOrigin(), sub_polytype, polytype) * wrap;
    }

    Hs::Var poly_id({noloc,name});

    return {poly_id, mono_id, monotype, polytype, wrap};
}

// II. decideQuantification
//
// 1. decideMonoTyVars == Get global tyvars and grow them using equalities.
//                     If a is fixed, then (a ~ [beta]) fixes beta.
//                     But (a ~ F beta) does not fix beta.
//                     Returns new candidates by clearing all of them if restricted is true.
//
// 2. defaultTyVarsAndSimplify == Promote known-fixed tyvars (to current level from rhs_tclvl)
//                             Default kind & levity vars?
//                             Re-simplify ... to infer multiplicity?
//                             Also... somehow promotes mtvs in candidates to rhs_tclvl?
//                             Return simplified candidates...
//
//
// 3. decideQuantifiedTyVars ==
// - tau_tys = list of zonked monotypes
// - grown_tcvs = type vars of tau_tys + any that are transitively reachable through a candidate pred
//   + this set includes metatypevars, typevars, and coercionvars
// - dv <- candidateQTyVarsOfTypes (candidates ++ tau_tys)
//   + find types in tau_tys OR candidates with a level that is deeper than the current level
//   + classify into type and kinds (a tyvar can be both if we have e.g. forall k (a::k) ...
// - dvs_plus = intersection of dv with grown_tcvs
// - final_qtvs <- quantifyTyVars grown_tcvs
//   + defaultTyVars
//   + discard coercion vars
//   + run skolemiseQuantifiedTyVar on them
//     o  if its a metatyvar, then
//        * replace with skolemtv with {kind = from metatv, tc_level = current_level+1, loc=here, where we are generalizing
//     = if its a skolemtv, then
//        * basically keep it... possibly with a deeper level?
//
// - return final_qtvs
//
// III. emitResidualConstraints
//
// args = level solve_decls mono_binder_env qtvs givens solved_wanteds
//
// Instead of removing the wanteds for the candidates, we construct a new implication constraint with:
// - level = rhs_level
// - skols = qtvs
// - givens = full_theta_vars
// - wanteds = wanteds
// - decls = solve_decls?
// I guess this means that we try to solve the wanteds from the givens?


// Don't quantify equality preds like Int ~ Bool or a ~ [b].
// But we can quantify equality preds like F a [b] = Int
bool TypeChecker::is_quantifiable_pred(const Type& pred, const set<TypeVar>& qtvs) const
{
    if (not intersects(free_type_variables(pred), qtvs)) return false;

    if (auto eq = is_equality_pred(pred))
    {
        auto& [t1,t2] = *eq;
        return (is_type_fam_app(t1) or is_type_fam_app(t2));
    }
    else
        return true;
}

vector<Type>
TypeChecker::get_quantifiable_preds(bool restricted, const vector<Type>& preds, const set<TypeVar>& qtvs) const
{
    if (restricted) return {};

    vector<Type> keep;

    for(auto& pred: preds)
        if (is_quantifiable_pred(pred, qtvs))
            keep.push_back(pred);

    return keep;
}

tuple<set<TypeVar>, LIE, Core::Decls>
TypeChecker::simplify_and_quantify(bool restricted, WantedConstraints& wanteds, const value_env& mono_binder_env)
{
    // 1. Try and solve the wanteds.  (See simplifyInfer)
    auto tcs2 = copy_clear_wanteds(true);
    auto solve_decls = tcs2.entails({},  wanteds );
    int rhs_level = level + 1;
    
    // 2. Float wanteds out of implications if they aren't trapped by (i) given equalities or (ii) type variables
    auto maybe_quant_preds = preds_from_lie(float_wanteds(false,wanteds));
    for(auto& pred: maybe_quant_preds)
        promote(pred, rhs_level);

    // (qtvs, bound_theta) = decideQuantification resolved level rhs_level mono_binder_env quant_pred_candidates
    // bound_theta_vars = get new evidence vars for the preds in bound_theta?

    // emit residual constraints?
    // emitResidualConstraints rhs_level solve_decls mono_binder_env qtvs bound_theta_vars wanteds2
    
    auto tvs_in_any_type = free_meta_type_variables(mono_binder_env);
    auto local_tvs = tvs_in_any_type;
    add( local_tvs, free_meta_type_variables(wanteds.simple) );

    // 4. Figure out which type vars we cannot quantify over.
    auto fixed_tvs = find_fixed_tvs(restricted, level, maybe_quant_preds, local_tvs);

    // 5. After deciding which vars we may NOT quantify over, figure out which ones we CAN quantify over.
    set<MetaTypeVar> qmtvs = tvs_in_any_type - fixed_tvs;

    // 7. Replace quantified meta-typevars with fresh type vars, and promote the other ones.
    set<TypeVar> qtvs;
    for(auto& qmtv: qmtvs)
    {
        TypeVar qtv = FreshVarSource::fresh_rigid_type_var(rhs_level, unloc(qmtv.name), *qmtv.kind);
        qtvs.insert(qtv);
        qmtv.fill(qtv);
    }

    // promote type vars that we are not quantifying over.
    for(auto& tv: local_tvs)
        if (not tv.filled())
            maybe_promote_mtv(tv, level);

//  What do we want to assert here?
//    for(auto& tv: local_tvs)
//        assert(max_level(tv) <= level);

    // For the SOMEWHAT ambiguous constraints, we don't need the defaults to define the recursive group,
    // but we do need the defaults to define individual symbols.

    // 7. Quantify over variables in ANY type that are not fixed -- doesn't depend on defaulting.
    // Never quantify over variables that are only in a LIE -- those must be defaulted.

    // 6. Defer constraints w/o any vars to quantify over
    auto quant_preds = get_quantifiable_preds( restricted, maybe_quant_preds, qtvs );

    // 8. Only the constraints with all fixed tvs are going to be visible outside this declaration group.
    assert(not restricted or quant_preds.empty() );

    // 4. Constrict givens from the preds
    LIE givens;
    for(auto& pred: quant_preds)
        givens.push_back({GivenOrigin(), Given, fresh_dvar(pred), pred, rhs_level});

    return {qtvs, givens, solve_decls};
}

Hs::Decls
TypeChecker::infer_type_for_decls_group(const map<string, Type>& signatures, Hs::Decls decls, bool is_top_level)
{
    if (single_fundecl_with_sig(decls, signatures))
    {
        auto& FD = decls[0].as_<Hs::FunDecl>();

        auto [decl, name, sig_type] = infer_type_for_single_fundecl_with_sig(FD);

        Hs::Decls decls({decl});

        return decls;
    }

    // 1. Type check the decls group with monomorphic types for vars w/o signatures.
    auto tcs2 = copy_clear_wanteds(true);
    auto [mono_ids, mono_binder_env] = tcs2.tc_decls_group_mono(signatures, decls);
    auto wanteds = tcs2.current_wanteds();

    // 2. Check if there are predicates on signatures with the monomorphism restriction..
    bool restricted = is_restricted(signatures, decls) and not is_top_level;
    // TODO: complain here if restricted variable have signatures with constraints?

    // 3. Determine what to quantify over and stuff
    auto [qtvs, givens, solve_decls] = simplify_and_quantify(restricted, wanteds, mono_binder_env);

    auto ev_decls = std::make_shared<Core::Decls>(solve_decls);

    auto imp = std::make_shared<Implication>(level+1, qtvs | ranges::to<vector>, givens, wanteds, ev_decls, context);

    current_wanteds().implications.push_back(imp);

    // 5. Check that we don't have any wanteds with a deeper level
    for(auto& constraint: current_wanteds().simple)
    {
        assert( max_level(constraint.pred) <= level );
    }

    // 6. Compute bind infos
    map<string, Hs::BindInfo> bind_infos;
    for(auto& [name, monotype]: mono_binder_env)
    {
        auto mono_id = mono_ids.at(name);
        auto bind_info = compute_bind_info(name, mono_id, qtvs, monotype, signatures, givens);
        bind_infos.insert({name, bind_info});
    }
    assert(bind_infos.size() >= 1);

    // 7. Record types for binders
    global_value_env poly_binder_env;
    for(auto& [name, bind_info]: bind_infos)
        poly_binder_env = poly_binder_env.insert({name, bind_info.polytype});
    add_binders(poly_binder_env);

    // 8. Construct the quantified declaration to return
    vector< Core::Var > dict_vars = dict_vars_from_lie( givens );
    auto gen_bind = mkGenBind( qtvs | ranges::to<vector>, dict_vars, ev_decls, decls, bind_infos );
    Hs::Decls decls2({ gen_bind });

    return decls2;
}

