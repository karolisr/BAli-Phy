#include "typecheck.H"
#include "kindcheck.H"
#include "solver.H"
#include "unify.H" // for occurs_check

#include "util/set.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

namespace views = ranges::views;

string NonCanonicalPred::print() const
{
    return dvar.print() + " :: " + constraint.print();
}

string CanonicalDictPred::print() const
{
    auto constraint = Hs::make_tyapps(klass, args);
    return dvar.print() + " :: " + constraint.print();
}

string CanonicalEqualityPred::print() const
{
    auto constraint = Hs::make_equality_constraint(a, b);
    return co.print() + " :: " + constraint.print();
}

string Pred::print() const
{
    if (std::holds_alternative<NonCanonicalPred>(*this))
        return std::get<NonCanonicalPred>(*this).print();
    else if (std::holds_alternative<CanonicalDictPred>(*this))
        return std::get<CanonicalDictPred>(*this).print();
    else if (std::holds_alternative<CanonicalEqualityPred>(*this))
        return std::get<CanonicalEqualityPred>(*this).print();
    else
        std::abort();
}

string Predicate::print()  const
{
    string s = (flavor==Given)?"[G] ":"[W] ";
    s += pred.print();
    return s;
}
template <typename T, typename U>
const T* to(const U& u)
{
    if (std::holds_alternative<T>(u))
        return &std::get<T>(u);
    else
        return nullptr;
}

bool type_is_hnf(const Hs::Type& type)
{
    auto [head,args] = Hs::decompose_type_apps(type);

    if (head.is_a<Hs::TypeVar>())
        return true;
    else if (head.is_a<Hs::MetaTypeVar>())
        return true;
    else if (head.is_a<Hs::TypeCon>())
        return false;
    else if (head.is_a<Hs::ListType>())
        return false;
    else if (head.is_a<Hs::TupleType>())
        return false;
    else
        std::abort();
}

// OK:     K a, K (a b), K (a [b]), etc. OK
// NOT OK: K [a], K (a,b), etc. NOT OK.
// Question: for multiparameter type classes, how about i.e. `K Int a`?
bool constraint_is_hnf(const Hs::Type& constraint)
{
    auto [class_con, args] = Hs::decompose_type_apps(constraint);
    for(auto& arg: args)
        if (not type_is_hnf(arg))
            return false;
    return true;
}


pair<Core::Decls, LIE> typechecker_state::toHnf(const Core::Var& dvar, const Hs::Type& constraint)
{
    Core::Decls decls;
    LIE lie;
    if (constraint_is_hnf(constraint))
    {
        lie.push_back({dvar, constraint});
    }
    else
    {
        LIE lie2;

        // 1. find the (single) matching instance or fail.
        // The instance will be of the form
        //     dfun :: forall  a1 a2 a3. (constraint, constraint, constraint) => K (T b1 b2 b3)
        // We need to substitute into this definition to make K (T b1 b2 b3) == constraint.
        auto instance = lookup_instance(constraint);
        if (not instance)
            throw myexception()<<"No instance for '"<<constraint<<"'";

        auto [dfun_exp, wanteds] = *instance;

        // 2. We need to make up new dvar names for all the input constraints.
        // Then we would add to the decls:
        //     dvar_orig = dfun dvar_new1 dvar_new2 dvar_new3
        // And we would get a NEW lie:
        //     dvar_new1 :: constraint1
        //     dvar_new2 :: constraint2
        //     dvar_new3 :: constraint3
        for(auto& [wdvar, wconstraint]: wanteds)
            lie2.push_back({wdvar, wconstraint});


        // 3. Finally, we may need to further simplify the new LIE
        auto [decls2, lie3] = toHnfs(lie2);

        lie = lie3;
        decls = decls2;
        decls.push_back( {dvar, dfun_exp} );
    }
    return {decls,lie};
}

pair<Core::Decls, LIE>
typechecker_state::toHnfs(const LIE& lie_in)
{
    Core::Decls decls_out;
    LIE lie_out;
    for(auto& [name, constraint]: lie_in)
    {
        auto [decls2, lie2] = toHnf(name, constraint);

        decls_out += decls2;
        lie_out += lie2;
    }
    return {decls_out, lie_out};
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<Core::Var, Hs::Type>> typechecker_state::superclass_constraints(const Hs::Type& constraint)
{
    vector<pair<Core::Var, Hs::Type>> constraints;

    auto class_name = get_full_class_name_from_constraint(constraint);

    // Fixme... since we know the class name, can we simplify superclass_extractors???
    for(auto& [dvar, type]: class_env().at(class_name).superclass_extractors)
    {
        // forall a.Klass a => Superklass a
        auto [_, wanteds, superclass_constraint] = instantiate(type);

        assert(constraint_is_hnf(superclass_constraint));

        assert(wanteds.size() == 1);

        auto class_constraint = wanteds[0].second;

        // The premise doesn't match the current class;
        if (not maybe_match(class_constraint, constraint)) continue;

        constraints.push_back( { dvar, superclass_constraint } );
    }

    return constraints;
}

// We are trying to eliminate the *first* argument.
optional<vector<Core::Var>> typechecker_state::is_superclass_of(const Hs::Type& constraint1, const Hs::Type& constraint2)
{
    vector<Core::Var> extractors;
    if (same_type(constraint1, constraint2))
        return extractors;
    else
    {
        // dvar1 :: constraint1 => dvar3 :: constraint3 => dvar2 :: constraint2
        for(auto& [dvar, constraint3]: superclass_constraints(constraint2))
        {
            if (auto extractors2 = is_superclass_of(constraint1, constraint3))
            {
                extractors = std::move(*extractors2);
                extractors.push_back(dvar);
                return extractors;
            }
        }
        return {};
    }
}

optional<Core::Decls> typechecker_state::entails_by_superclass(const pair<Core::Var, Hs::Type>& given, const pair<Core::Var, Hs::Type>& wanted)
{
    auto& [dvar_given, given_constraint] = given;
    auto& [dvar_wanted, wanted_constraint] = wanted;

    if (auto extractors = is_superclass_of(wanted_constraint, given_constraint))
    {
        Core::Exp dict_exp = dvar_given;
        for(auto& extractor: *extractors | views::reverse)
            dict_exp = {extractor, dict_exp};

        // dvar_wanted = extractor[n] extractor[n-1] ... extractor[0] dvar_given
        return Core::Decls( { pair(dvar_wanted, dict_exp) } );
    }
    else
        return {};
}

// 1. constraint solving loop: first process simple constraints, then go under implications.
//    1a. Run the constraint solving loop on the simpled Wanted constraints.
//        - This can create new implication constraints. (how?)
//    1b. Process each implication constraint
//        - Enter an implication.
//        - Save the outer inert set.
//        - Simplify the givens - add them to the inert set when this makes sense.
//        - Solve the inner simple wanteds - also add them to the inert set?
//          - does this have the effect of making simple wanteds affect implication constraints?
//        - Recurse into nested implications.
//        - Restore the saved inert set to what it was before entering the implication.
//        - Then continue processing the next implication constraint.

// 2. The interaction pipeline.
//    2a. Pick an item from the work list.
//    2b. If its not canonical, canonicalize it and goto 2a.
//    2c. If we have an inert reaction, do it and goto 2a.
//    2d. If we have a top-level reaction, do it and goto 2a.
//    2e. If its canonical and there are no possible reactions left, add it to the inert set.


template <typename T>
std::optional<Core::Decls> typechecker_state::entails(const T& givens, const std::pair<Core::Var, Hs::Type>& wanted_pred)
{
    // 1. First check if the wanted pred is a superclass of any of the givens.
    //
    //    A class implies all of its superclasses separately, but a single superclass of K may not imply K.
    for(auto& given: givens)
    {
        if (auto decls = entails_by_superclass(given, wanted_pred))
            return *decls;
    }

    // 2. Then check if there is an instance dfun :: (K1 a, K2 a) => K a
    auto [wanted_dvar, wanted_constraint] = wanted_pred;

    if (auto inst = lookup_instance(wanted_constraint))
    {
        auto [dfun_exp, super_wanteds] = *inst;

        Core::Decls decls;
        decls.push_back( { wanted_dvar, dfun_exp} );

        // If we can get (dvar1 :: K1 a) and (dvar2 :: K2 a) and a dfun so that dvar = dfun dvar1 dvar2
        for(auto& super_wanted: super_wanteds)
        {
            if (auto edecls = entails(givens, super_wanted))
                decls = *edecls + decls;
            else
                return {};
        }

        return decls;
    }

    return {};
}

bool is_canonical(const Predicate& p)
{
    return std::holds_alternative<CanonicalDictPred>(p.pred) or
        std::holds_alternative<CanonicalEqualityPred>(p.pred);
}

std::optional<Reaction> canonicalize_equality(const Core::Var& co_var, ConstraintFlavor flavor, const Hs::Type& t1, const Hs::Type& t2)
{
    // REFL: tau ~ tau
    if (same_type(t1,t2))
        return ReactSuccess({}, {});

    auto [head1,args1] = decompose_type_apps(t1);
    auto [head2,args2] = decompose_type_apps(t2);
    auto h1 = head1.to<Hs::TypeCon>();
    auto h2 = head2.to<Hs::TypeCon>();

    if (h1 and h2)
    {
        // TDEC: T1 x1 y1 z1 = T1 x2 y2 z2
        if (*h1 == *h2 and args1.size() == args2.size())
        {
            vector<Predicate> preds;
            for(int i=0;i<args1.size();i++)
                preds.push_back(Predicate(flavor,NonCanonicalPred(flavor, make_equality_constraint(args1[i],args2[i]))));
            return ReactSuccess({}, preds);
        }
        // FAILDEC: T1 x1 y1 z1 = T2 x2 y2 z2
        else
        {
            return ReactFail();
        }
    }

    if (auto uv1 = t1.to<Hs::MetaTypeVar>())
    {
        if (auto uv2 = t2.to<Hs::MetaTypeVar>(); *uv2 < *uv1)
            return canonicalize_equality(co_var, flavor, t2, t1);

        if (occurs_check(*uv1, t2))
            return ReactFail();
    }
    else if (t2.is_a<Hs::MetaTypeVar>())
    {
        return canonicalize_equality(co_var, flavor, t2, t1);
    }
    else if (auto tv1 = t1.to<Hs::TypeVar>())
    {
        if (auto tv2 = t2.to<Hs::TypeVar>(); *tv2 < *tv1)
            return canonicalize_equality(co_var, flavor, t2, t1);

        if (occurs_check(*tv1, t2))
            return ReactFail();
    }
    else if (t2.is_a<Hs::TypeVar>())
    {
        return canonicalize_equality(co_var, flavor, t2, t1);
    }

    vector<Predicate> preds = {Predicate(flavor,CanonicalEqualityPred(co_var, t1, t2))};
    return ReactSuccess({}, preds);
}

std::optional<Reaction> canonicalize(const Predicate& P)
{
    if (is_canonical(P)) return {};

    Core::Decls decls;
    vector<Predicate> preds;
    auto NCP = std::get<NonCanonicalPred>(P.pred);
    auto flavor = P.flavor;

    if (auto eq = Hs::is_equality_constraint(NCP.constraint))
    {
        auto& [t1, t2] = *eq;
        return canonicalize_equality(NCP.dvar, flavor, t1, t2);
    }
    else
    {
        auto [head,args] = decompose_type_apps(NCP.constraint);
        assert(head.is_a<Hs::TypeCon>());
        auto klass = head.as_<Hs::TypeCon>();
        preds.push_back( Predicate{flavor, CanonicalDictPred{NCP.dvar, klass, args}} );
    }
    return ReactSuccess(decls, preds);
}

std::optional<Reaction> typechecker_state::interact_same(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    if (P1.flavor != P2.flavor) return {};
    auto flavor = P1.flavor;

    auto eq1 = to<CanonicalEqualityPred>(P1.pred);
    auto eq2 = to<CanonicalEqualityPred>(P2.pred);

    auto dict1 = to<CanonicalDictPred>(P1.pred);
    auto dict2 = to<CanonicalDictPred>(P2.pred);

    if (eq2 and not eq1)
        return interact_same(P2,P1);

    else if (eq1 and eq2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = t1a.to<Hs::MetaTypeVar>();
        while (uv1 and uv1->filled())
        {
            t1a = *uv1->filled();
            uv1 = t1a.to<Hs::MetaTypeVar>();
        }
        auto tv1 = t1a.to<Hs::TypeVar>();

        auto [v2, t2a, t2b] = *eq2;
        auto uv2 = t2a.to<Hs::MetaTypeVar>();
        while(uv2 and uv2->filled())
        {
            t2a = *uv2->filled();
            uv2 = t2a.to<Hs::MetaTypeVar>();
        }
        auto tv2 = t2a.to<Hs::TypeVar>();

        // EQSAME: (tv1 ~ X1) + (tv1 ~ X2) -> (tv1 ~ X1) && (X1 ~ X2) 
        if ((tv1 and tv2 and *tv1 == *tv2) or (uv1 and uv2 and *uv1 == *uv2))
        {
            Predicate P3(flavor, NonCanonicalPred(eq2->co, make_equality_constraint(t1b, t2b)));
            return ReactSuccess({}, {P1,P3});
        }
        // EQDIFF: (tv1 ~ X1) + (tv2 ~ X2) -> (tv1 ~ X1) && (tv2 ~ [tv1->X1]X2) if tv1 in ftv(X2)
        else if ((tv1 or uv1) and (tv2 or uv2))
        {
            if (auto t2b_subst = tv1?check_apply_subst({{*tv1, t1b}}, t2b):check_apply_subst({{*uv1, t1b}}, t2b))
            {
                Predicate P3(flavor, NonCanonicalPred(eq2->co, make_equality_constraint(t2a, *t2b_subst)));
                return ReactSuccess({}, {P1, P3});
            }
            else if (auto t1b_subst = tv2?check_apply_subst({{*tv2, t2b}}, t1b):check_apply_subst({{*uv2, t2b}}, t1b))
            {
                Predicate P3(flavor, NonCanonicalPred(eq2->co, make_equality_constraint(t1a, *t1b_subst)));
                return ReactSuccess({}, {P3, P2});
            }
        }
    }
    // EQDICT: (tv1 ~ X1) + (D xs)     -> (tv1 ~ X1) && (D [tv1->X1]xs) if tv1 in ftv(xs)
    else if (eq1 and dict2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = t1a.to<Hs::MetaTypeVar>();
        while (uv1 and uv1->filled())
        {
            t1a = *uv1->filled();
            uv1 = t1a.to<Hs::MetaTypeVar>();
        }
        auto tv1 = t1a.to<Hs::TypeVar>();

        bool changed = false;
        CanonicalDictPred dict2_subst = *dict2;
        for(auto& class_arg: dict2_subst.args)
        {
            if (auto maybe_class_arg = tv1?check_apply_subst({{*tv1,t1b}}, class_arg):check_apply_subst({{*uv1,t1b}},class_arg))
            {
                changed = true;
                class_arg = *maybe_class_arg;
            }
        }
        if (changed)
            return ReactSuccess({},{Predicate(flavor, dict2_subst)});
    }
    else if (dict1 and dict2)
    {
        auto constraint1 = Hs::make_tyapps(dict1->klass,dict1->args);
        auto constraint2 = Hs::make_tyapps(dict2->klass,dict2->args);

        // DDICT:  (D xs)     + (D xs)     -> (D xs)
        if (same_type(constraint1, constraint2))
        {
            Core::Decls decls = {{dict2->dvar, dict1->dvar}};
            return ReactSuccess(decls,{P1});
        }
        // SUPER - not in the paper.
        else if (auto decls = entails_by_superclass({dict1->dvar,constraint1}, {dict2->dvar,constraint2}))
        {
            return ReactSuccess(*decls, {P1});
        }
        // SUPER - not in the paper.
        else if (auto decls = entails_by_superclass({dict2->dvar,constraint2}, {dict1->dvar,constraint1}))
        {
            return ReactSuccess(*decls, {P2});
        }
    }
    // EQFEQ:  (tv1 ~ X1) + (F xs ~ x) -> (tv1 ~ X1) && (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // FEQFEQ: (F xs ~ X1) + (F xs ~ X2) -> (F xs ~ X1) && (X1 ~ X2)
    
    // No reaction
    return {};
}

std::optional<Reaction> typechecker_state::interact_g_w(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    if (P1.flavor == P2.flavor) return {};
    if (P1.flavor != Given) return interact_g_w(P2, P1);

    auto eq1 = to<CanonicalEqualityPred>(P1.pred);
    auto eq2 = to<CanonicalEqualityPred>(P2.pred);

    auto dict1 = to<CanonicalDictPred>(P1.pred);
    auto dict2 = to<CanonicalDictPred>(P2.pred);

    if (eq1 and eq2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = t1a.to<Hs::MetaTypeVar>();
        while (uv1 and uv1->filled())
        {
            t1a = *uv1->filled();
            uv1 = t1a.to<Hs::MetaTypeVar>();
        }
        auto tv1 = t1a.to<Hs::TypeVar>();

        auto [v2, t2a, t2b] = *eq2;
        auto uv2 = t2a.to<Hs::MetaTypeVar>();
        while(uv2 and uv2->filled())
        {
            t2a = *uv2->filled();
            uv2 = t2a.to<Hs::MetaTypeVar>();
        }
        auto tv2 = t2a.to<Hs::TypeVar>();

        // SEQSAME: (tv1 ~ X1) simplifies (tv1 ~ X2) -> (X1 ~ X2) 
        if ((tv1 and tv2 and *tv1 == *tv2) or (uv1 and uv2 and *uv1 == *uv2))
        {
            Predicate P3(Wanted, NonCanonicalPred(eq2->co, make_equality_constraint(t1b, t2b)));
            return ReactSuccess({}, {P1,P3});
        }
        // SEQDIFF: (tv1 ~ X1) simplifies (tv2 ~ X2) -> (tv2 ~ [tv1->X1]X2) if tv1 in ftv(X2)
        else if ((tv1 or uv1) and (tv2 or uv2))
        {
            if (auto t2b_subst = tv1?check_apply_subst({{*tv1, t1b}}, t2b):check_apply_subst({{*uv1, t1b}}, t2b))
            {
                Predicate P3(Wanted, NonCanonicalPred(eq2->co, make_equality_constraint(t2a, *t2b_subst)));
                return ReactSuccess({}, {P1, P3});
            }
        }
    }
    // SEQDICT: (tv1 ~ X1) simplifies (D xs) -> (D [tv1->X1]xs) if tv1 in ftv(xs)
    else if (eq1 and dict2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = t1a.to<Hs::MetaTypeVar>();
        while (uv1 and uv1->filled())
        {
            t1a = *uv1->filled();
            uv1 = t1a.to<Hs::MetaTypeVar>();
        }
        auto tv1 = t1a.to<Hs::TypeVar>();

        bool changed = false;
        CanonicalDictPred dict2_subst = *dict2;
        for(auto& class_arg: dict2_subst.args)
        {
            if (auto maybe_class_arg = tv1?check_apply_subst({{*tv1,t1b}}, class_arg):check_apply_subst({{*uv1,t1b}},class_arg))
            {
                changed = true;
                class_arg = *maybe_class_arg;
            }
        }
        if (changed)
            return ReactSuccess({},{Predicate(Wanted, dict2_subst)});
    }
    else if (dict1 and dict2)
    {
        auto constraint1 = Hs::make_tyapps(dict1->klass,dict1->args);
        auto constraint2 = Hs::make_tyapps(dict2->klass,dict2->args);

        // SDDICTG:  (D xs) simplifies (D xs)     -> empty
        if (same_type(constraint1, constraint2))
        {
            Core::Decls decls = {{dict2->dvar, dict1->dvar}};
            return ReactSuccess(decls,{P1});
        }
        // SSUPER - not in the paper.
        else if (auto decls = entails_by_superclass({dict1->dvar,constraint1}, {dict2->dvar,constraint2}))
        {
            return ReactSuccess(*decls, {P1});
        }
    }
    // SEQFEQ:  (tv1 ~ X1) simplifies (F xs ~ x) -> (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // SFEQFEQ: (F xs ~ X1) simplifies (F xs ~ X2) -> (X1 ~ X2)

    return {};
}


ReactSuccess::ReactSuccess(const Core::Decls& d, ConstraintFlavor f, const std::vector<std::pair<Core::Var, Hs::Type>>& ps)
    :decls(d)
{
    for(auto& [cvar, constraint]: ps)
        predicates.push_back(Predicate(f,NonCanonicalPred(cvar,constraint)));
}


// we need to have three results: No react, React<vector<Predicates>>, ReactFail
std::optional<Reaction> typechecker_state::top_react(const Predicate& P)
{
    assert(is_canonical(P));

    if (auto dict = to<CanonicalDictPred>(P.pred))
    {
        auto [dvar, klass, args] = *dict;
        auto constraint = Hs::make_tyapps(klass,args);

        if (auto inst = lookup_instance(constraint))
        {
            // There should not be instances for given constraints
            if (P.flavor == Given) return ReactFail();

            auto [dfun_exp, super_wanteds] = *inst;

            Core::Decls decls;
            decls.push_back( { dvar, dfun_exp } );

            return ReactSuccess(decls, Wanted, super_wanteds);
        }
    }
    
    return {};
}

bool is_touchable(const Hs::MetaTypeVar&)
{
    return true;
}


    // The simplifier corresponds to the relation |->[simp] from Figure 19 of the OutsideIn(X) paper:
    //    \mathcal{Q}; Q[given] ; alpha[touchable] |->[simp] Q[wanted] ~~> Q[residual]; theta
    // where theta is the substitution -- which we should actually perform here instead of returning as an object.

    // So, we need to to know the set of touchable variables, either through the typechecker_state, or
    // as a function argument.

    // This has a few steps (rule SIMPLES from Figure 19):
    // 1. Run the the rewrite rules until no more are applicable.
    //    We start out with no flattening substitution.
    // 2. Apply the flattening transformation on the wanteds -> Q[rr].
    // 3. \mathcal{E} = the set of (beta ~ tau) or (tau ~ beta) in Q[rr] where
    //    * beta is a touchable unification variable
    //    * beta is not in the free unification variables of tau
    // 4. theta = substitutions (beta -> theta(tau)) from \mathcal{E}, where
    //    * we choose only one substitution for beta if we have multiple equations on beta
    //    * for example, if we have (beta ~ tau1) and (beta ~ tau2), we pick one.
    //    * presumably, we can pick either one -> they would need to agree!
    //    * if they don't, do we do Irresolvable (beta ~ tau1) and Irresolvable (beta ~ tau2)?
    //    * we need to substitute into the taus using the theta that contains the taus!
    //      - that probably works fine if we use meta-vars.
    // 5. Q[r] = Q[rr] - \mathcal{E}
    // 6. Return theta(Q[r])
    //    * we can do this by just performing theta and returning Q[r].

    // The rewrite rules (also Figure 19) include:
    // a. replace a given/wanted by a canonicalized given/wanted (and maybe add a flattening substitution)
    // b. interact a given/wanted Q1 with a given/wanted Q2 and to produce a given/wanted Q3.
    //    - do we REPLACE Q1 and Q2 by Q3, or ADD Q3 to the set?
    // c. react a given Q1 and a wanted Q2 to produce a new wanted Q3 = simplifies(Q1,Q2)
    //    - replace the wanted Q2 by Q3.
    // d. react a given Q1 with axioms in \mathcal{Q} to produce Q2.
    //    - replace Q1 by Q2.
    // e. react a wanted Q1 with axioms in \mathcal{Q} to produce Q2 and new touchable variables beta.
    //    - replace Q1 by Q2 and add in the new touchable variables beta.

pair<Core::Decls, LIE> typechecker_state::simplify(const LIE& givens, const LIE& wanteds)
{
    Core::Decls decls;

    std::vector<Predicate> work_list;
    std::vector<Predicate> inert;
    std::vector<Predicate> failed;

    auto react = [&](const optional<Reaction>& maybe_react, const Predicate& P) -> bool
    {
        if (maybe_react)
        {
            if (auto r = to<ReactSuccess>(*maybe_react))
            {
                auto& new_preds = r->predicates;
                decls += r->decls;
                work_list.insert(work_list.end(), new_preds.begin(), new_preds.end());
            }
            else
                failed.push_back(P);
        }
        return bool(maybe_react);
        
    };
    
    for(auto& [evar, constraint]: givens)
        work_list.push_back({Given, NonCanonicalPred(evar, constraint)});
    for(auto& [evar, constraint]: wanteds)
        work_list.push_back({Wanted, NonCanonicalPred(evar, constraint)});

    while(not work_list.empty())
    {
        auto p = work_list.back(); work_list.pop_back();

        // canonicalize
        if (react(canonicalize(p), p))
            continue;

        // binary interact with other preds with same flavor
        bool reacted = false;
        for(int i=0;i<inert.size() and not reacted;i++)
        {
            reacted = react(interact_same(p, inert[i]), p);
            if (reacted)
            {
                if (i+1 < inert.size())
                    std::swap(inert[i], inert.back());

                inert.pop_back();
            }
        }
        if (reacted) continue;
        
        // binary interact given/wanted
        for(int i=0;i<inert.size() and not reacted;i++)
        {
            reacted = react(interact_g_w(p, inert[i]), p);
            if (reacted)
            {
                if (i+1 < inert.size())
                    std::swap(inert[i], inert.back());

                inert.pop_back();
            }
        }
        if (reacted) continue;

        // top-level reactions
        if (react(top_react(p), p))
            continue;

        // we should only ge this far if there are no reactions.

        inert.push_back(p);
    }

    if (not failed.empty())
        throw myexception()<<"Equations have no solution!";

    // Split inert into substitution and remaining constraints
    LIE residual_wanteds;
    vector<pair<Hs::MetaTypeVar,Hs::Type>> equations;
    for(auto& P: inert)
    {
        assert(is_canonical(P));

        if (P.flavor == Given) continue;

        if (auto eq = to<CanonicalEqualityPred>(P.pred))
        {
            auto [_, t_a, t_b] = *eq;
            auto uv = t_a.to<Hs::MetaTypeVar>();
            while (uv and uv->filled())
            {
                t_a = *uv->filled();
                uv = t_a.to<Hs::MetaTypeVar>();
            }
            if (uv and is_touchable(*uv) and not occurs_check(*uv, t_b))
                equations.push_back({*uv,t_b});
            else
                residual_wanteds.push_back({eq->co, Hs::make_equality_constraint(t_a, t_b)});
        }
        else if (auto dict = to<CanonicalDictPred>(P.pred))
        {
            auto constraint = Hs::make_tyapps(dict->klass, dict->args);
            residual_wanteds.push_back({dict->dvar, constraint});
        }
    }

    // Actually perform the substitution.
    for(auto& [uv,type]: equations)
    {
        if (not uv.filled())
            uv.fill(type);
    }

    return {decls, residual_wanteds};
}


pair<Core::Decls, LIE> typechecker_state::entails(const LIE& givens, const LIE& wanteds)
{
    auto [decls, residual_wanteds] = simplify(givens, wanteds);

    // This should implement |->[solv] from Figure 14 of the OutsideIn(X) paper:
    //   \mathcal{Q}; Q[given]; alpha[touchable] |->[solv] C[wanted] ~~> Q[residual]; theta
    // where theta is a substitution.

    // This has a few steps (rule SOLVE from Figure 14):
    // 1. Run the simplifier (|->[simp]) on the non-implication constraints to produce Q[r]; theta
    // 2. For each implication constraint alpha[i];Q[i] => C[i]
    //    - apply theta to Q[i] and C[i] (which would happen automatically if we use metavars)
    //    - run solve(\mathcal{Q}, Q[given] + Q[r] + Q[i]; alpha[i], C[i]) |->[solv] empty;theta[i]
    //    - the theta[i] shouldn't affect anything outside the implication we are working on.
    // 3. we return Q[r];theta
    //    - interestingly, the implication constraints don't contribute here.

    return {decls, residual_wanteds};
}
