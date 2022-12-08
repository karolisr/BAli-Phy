#include "typecheck.H"
#include "kindcheck.H"
#include "solver.H"

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
    auto constraint = Hs::make_equality_constraint(t1, t2);
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

template <typename T, typename U>
T* to(U& u)
{
    if (std::holds_alternative<T>(u))
        return &std::get<T>(u);
    else
        return nullptr;
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<Core::Var, Hs::Type>> TypeChecker::superclass_constraints(const Hs::Type& constraint)
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
optional<vector<Core::Var>> TypeChecker::is_superclass_of(const Hs::Type& constraint1, const Hs::Type& constraint2)
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

optional<Core::Decls> TypeChecker::entails_by_superclass(const pair<Core::Var, Hs::Type>& given, const pair<Core::Var, Hs::Type>& wanted)
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
std::optional<Core::Decls> TypeChecker::entails(const T& givens, const std::pair<Core::Var, Hs::Type>& wanted_pred)
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

bool cmp_less(const Hs::MetaTypeVar& uv1, const Hs::MetaTypeVar& uv2)
{
    assert(not uv1.filled());
    assert(not uv2.filled());

    if (uv1.level() > uv2.level())
        return true;
    if (uv1.level() < uv2.level())
        return false;

    return (uv1 < uv2);
}


CanonicalEqualityPred CanonicalEqualityPred::flip() const
{
    return {co, t2, t1};
}

std::optional<Predicate> Solver::canonicalize_equality(ConstraintFlavor flavor, CanonicalEqualityPred P)
{
    P.t1 = rewrite(flavor, P.t1);
    P.t2 = rewrite(flavor, P.t2);

    auto uv1 = follow_meta_type_var(P.t1).to<Hs::MetaTypeVar>();
    auto uv2 = follow_meta_type_var(P.t2).to<Hs::MetaTypeVar>();

    // REFL: tau ~ tau
    // NOTE: this does not currently handle foralls or constraints!
    if (same_type(P.t1, P.t2)) return {}; // Solved!

    auto tv1 = P.t1.to<Hs::TypeVar>();
    auto tv2 = P.t2.to<Hs::TypeVar>();

    if (uv1 and uv2)
    {
        if (cmp_less(*uv2,*uv1))
            return canonicalize_equality(flavor, P.flip());
        else
        {
            return {{flavor, P}};
        }
    }
    else if (uv1)
    {
        if (occurs_check(*uv1, P.t2))
        {
            inerts.failed.push_back({flavor,P});
            return {};
        }
        else
        {
            return {{flavor, P}};
        }
    }
    else if (uv2)
    {
        return canonicalize_equality(flavor, P.flip());
    }
    else if (tv1 and tv2)
    {
        if (*tv2 < *tv1)
            return canonicalize_equality(flavor, P.flip());
        else
        {
            return {{flavor, P}};
        }
    }
    else if (tv1)
    {
        if (occurs_check(*tv1, P.t2))
        {
            inerts.failed.push_back({flavor,P});
            return {};
        }
        else
        {
            return {{flavor, P}};
        }
    }
    else if (tv2)
    {
        return canonicalize_equality(flavor, P.flip());
    }
    else
    {
        // Right now the only TypeCon heads are data constructors,
        //   type synonyms, and variables.
        // Unlike GHC, we don't consider representational equality.
        // When we add type families, this will become more complicated.
        // See [Decomposing equality] in Tc/Solver/Canonical.hs
        while(auto s1 = is_type_synonym(P.t1))
            P.t1 = *s1;
        while(auto s2 = is_type_synonym(P.t2))
            P.t2 = *s2;

        if (auto tuple = P.t1.to<Hs::TupleType>())
            P.t1 = canonicalize_type(*tuple);
        else if (auto list = P.t1.to<Hs::ListType>())
            P.t1 = canonicalize_type(*list);

        if (auto tuple = P.t2.to<Hs::TupleType>())
            P.t2 = canonicalize_type(*tuple);
        else if (auto list = P.t2.to<Hs::ListType>())
            P.t2 = canonicalize_type(*list);

        auto [head1,args1] = decompose_type_apps(P.t1);
        auto [head2,args2] = decompose_type_apps(P.t2);

        if (args1.size() != args2.size())
        {
            // Is this too eager?
            inerts.failed.push_back({flavor,P});
            return {};
        }

        auto h1 = head1.to<Hs::TypeCon>();
        auto h2 = head2.to<Hs::TypeCon>();

        vector<Predicate> preds;
        if (h1 and h2)
        {
            if (*h1 == *h2)
            {
                // TDEC: T x1 y1 z1 = T x2 y2 z2
                // Fall through.
            }
            else
            {
                // FAILDEC: T x1 y1 z1 = S x2 y2 z2
                inerts.failed.push_back({flavor,P});
                return {};
            }
        }
        // We will need to do extra stuff here if either of the heads is a type family.
        else
        {
            auto constraint = make_equality_constraint(head1, head2);
            auto dvar = fresh_dvar(constraint);
            work_list.push_back({flavor, NonCanonicalPred(dvar, constraint)});
        }

        // If we've gotten here, the heads are both injective, and might be equal.
        for(int i=0;i<args1.size();i++)
        {
            auto constraint = make_equality_constraint(args1[i], args2[i]);
            auto dvar = fresh_dvar(constraint);
            work_list.push_back({flavor, NonCanonicalPred(dvar, constraint)});
        }

        return {};
    }

    std::abort();
}

bool is_canonical(const Predicate& p)
{
    return std::holds_alternative<CanonicalDictPred>(p.pred) or
        std::holds_alternative<CanonicalEqualityPred>(p.pred);
}


optional<Predicate> Solver::canonicalize_dict(ConstraintFlavor flavor, CanonicalDictPred P)
{
    for(auto& arg: P.args)
        arg = rewrite(flavor, arg);

    return {{flavor, P}};
}

optional<Predicate> Solver::canonicalize(Predicate& P)
{
    if (auto NC = to<NonCanonicalPred>(P.pred))
    {
        auto flavor = P.flavor;

        if (auto eq = Hs::is_equality_constraint(NC->constraint))
        {
            auto& [t1, t2] = *eq;
            return canonicalize_equality(flavor, {NC->dvar, t1, t2});
        }
        else
        {
            auto [head,args] = decompose_type_apps(NC->constraint);
            assert(head.is_a<Hs::TypeCon>());
            auto klass = head.as_<Hs::TypeCon>();
            return canonicalize_dict(P.flavor, CanonicalDictPred{NC->dvar, klass, args});
        }
    }
    else if (auto D = to<CanonicalDictPred>(P.pred))
        return canonicalize_dict(P.flavor, *D);
    else if (auto E = to<CanonicalEqualityPred>(P.pred))
        return canonicalize_equality(P.flavor, *E);
    else
        std::abort();
}

Hs::Type Solver::rewrite(ConstraintFlavor flavor, Hs::Type t) const
{
    for(auto& inert: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs))
    {
        // Don't allow wanteds to rewrite givens
        if (inert.flavor == Wanted and flavor == Given) continue;

        auto eq = to<CanonicalEqualityPred>(inert.pred);
        assert(eq);

        if (auto tv1 = eq->t1.to<Hs::TypeVar>())
        {
            substitution_t s = {{*tv1, eq->t2}};
            t = apply_subst(s, t);
        }
        else if (auto uv1 = follow_meta_type_var(eq->t1).to<Hs::MetaTypeVar>())
        {
            usubstitution_t s = {{*uv1, eq->t2}};
            t = apply_subst(s, t);
        }
    }

    return t;
}

Change Solver::interact(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    // Don't allow wanteds to rewrite givens
    if (P1.flavor == Wanted and P2.flavor == Given) return Unchanged();

    auto eq1 = to<CanonicalEqualityPred>(P1.pred);
    auto eq2 = to<CanonicalEqualityPred>(P2.pred);

    auto dict1 = to<CanonicalDictPred>(P1.pred);
    auto dict2 = to<CanonicalDictPred>(P2.pred);

    if (dict1 and dict2)
    {
        auto constraint1 = Hs::make_tyapps(dict1->klass, dict1->args);
        auto constraint2 = Hs::make_tyapps(dict2->klass, dict2->args);

        // DDICT:  (D xs)     + (D xs)     -> (D xs)
        if (same_type(constraint1, constraint2))
        {
            decls.push_back({dict2->dvar, dict1->dvar});
            return Solved();
        }
        // SUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass({dict1->dvar,constraint1}, {dict2->dvar,constraint2}))
        {
            decls += *sdecls;
            return Solved();
        }
    }
    // EQFEQ:  (tv1 ~ X1) + (F xs ~ x) -> (tv1 ~ X1) && (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // FEQFEQ: (F xs ~ X1) + (F xs ~ X2) -> (F xs ~ X1) && (X1 ~ X2)

    return Unchanged();
}

// we need to have three results: No react, React<vector<Predicates>>, ReactFail
std::optional<Reaction> Solver::top_react(const Predicate& P)
{
    assert(is_canonical(P));

    if (auto dict = to<CanonicalDictPred>(P.pred))
    {
        // We DO get givens like Eq Ordering that have instances.
        // Should we be preventing such things from becoming givens, since we could
        //   derive them from an instance instead?

        // We don't use instances for givens.
        if (P.flavor == Given) return {};

        auto [dvar, klass, args] = *dict;
        auto constraint = Hs::make_tyapps(klass,args);

        if (auto inst = lookup_instance(constraint))
        {
            auto [dfun_exp, super_wanteds] = *inst;

            decls.push_back( { dvar, dfun_exp } );
            for(auto& pred: make_predicates(Wanted, super_wanteds))
                work_list.push_back( pred );

            return ReactSuccess();
        }
    }
    
    return {};
}

bool Solver::is_touchable(const Hs::MetaTypeVar& mtv, const Hs::Type& rhs) const
{
    // We need to have done follow_meta_type_var( ) already.
    assert(not mtv.filled());

    assert(mtv.level() <= level);

    assert(inerts.given_eq_level.value_or(0) < level);

    // 1. Check for intervening given equalities
    bool intervening_given_eqs = inerts.given_eq_level and mtv.level() <= *inerts.given_eq_level;
    if (intervening_given_eqs) return false;

    // 2. Check for skolem escapes
    for(auto& tv: free_type_variables(rhs))
    {
        if (mtv.level() < tv.level())
            return false;
    }

    return true;
}

bool affected_by_mtv(const Pred& P, const Hs::MetaTypeVar& mtv)
{
    if (auto E = to<CanonicalEqualityPred>(P))
        return affected_by_mtv(E->t1, mtv) or affected_by_mtv(E->t2, mtv);
    else if (auto D = to<CanonicalDictPred>(P))
        return affected_by_mtv(D->args, mtv);
    else if (auto NC = to<NonCanonicalPred>(P))
        return affected_by_mtv(NC->constraint, mtv);
    else
        std::abort();
}

bool affected_by_mtv(const Predicate& P, const Hs::MetaTypeVar& mtv)
{
    return affected_by_mtv(P.pred, mtv);
}

void Solver::add_to_work_list(const std::vector<Predicate>& ps)
{
    for(auto& p: ps)
        if (p.flavor == Wanted)
            work_list.push_back(p);

    for(auto& p: ps)
        if (p.flavor == Given)
            work_list.push_back(p);
}

vector<Predicate> kickout_after_unification2(const Hs::MetaTypeVar& mtv, std::vector<Predicate>& preds)
{
    vector<Predicate> work_list;
    // kick-out for inerts that are rewritten by p
    for(int i=0;i<preds.size();i++)
    {
        if (affected_by_mtv(preds[i], mtv))
        {
            // FIXME: put givens last, so that we process them first
            work_list.push_back(preds[i]);
            if (i+1 < preds.size())
                std::swap(preds[i], preds.back());

            preds.pop_back();
            i--;
        }
    }

    return work_list;
}

void Solver::kickout_after_unification(const Hs::MetaTypeVar& mtv)
{
    // kick-out for inerts containing mtv
    kickout_after_unification2(mtv, inerts.tv_eqs);
    kickout_after_unification2(mtv, inerts.mtv_eqs);
    kickout_after_unification2(mtv, inerts.tyfam_eqs);
    kickout_after_unification2(mtv, inerts.dicts);
    kickout_after_unification2(mtv, inerts.irreducible);
}

void Solver::add_inert(const Predicate& p)
{
    if (auto E = to<CanonicalEqualityPred>(p.pred))
    {
        int eq_level = std::max( Hs::max_level(E->t1), Hs::max_level(E->t2));
        if (eq_level < level and p.flavor == Given)
        {
            inerts.given_eq_level = std::max( inerts.given_eq_level.value_or(0), eq_level );
        }
    }

    if (auto E = to<CanonicalEqualityPred>(p.pred))
    {
        auto t1 = Hs::follow_meta_type_var(E->t1);
        if (t1.is_a<Hs::TypeVar>())
            inerts.tv_eqs.push_back(p);
        else if (t1.is_a<Hs::MetaTypeVar>())
            inerts.mtv_eqs.push_back(p);
        else if (false) // type fam application?
            inerts.tyfam_eqs.push_back(p);
        else
            inerts.irreducible.push_back(p);
    }
    else if (to<CanonicalDictPred>(p.pred))
    {
        inerts.dicts.push_back(p);
    }
    else
        std::abort();
}

    // The simplifier corresponds to the relation |->[simp] from Figure 19 of the OutsideIn(X) paper:
    //    \mathcal{Q}; Q[given] ; alpha[touchable] |->[simp] Q[wanted] ~~> Q[residual]; theta
    // where theta is the substitution -- which we should actually perform here instead of returning as an object.

    // So, we need to to know the set of touchable variables, either through the TypeChecker, or
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

bool Solver::can_rewrite(const Predicate& p1, const Predicate& p2) const
{
    auto eq1 = to<CanonicalEqualityPred>(p1.pred);
    if (not eq1) return false;

    auto t1 = follow_meta_type_var(eq1->t1);

    if (auto tv = t1.to<Hs::TypeVar>())
    {
        if (auto dict2 = to<CanonicalDictPred>(p2.pred))
            return contains_tv(dict2->args, *tv);
        else if (auto eq2 = to<CanonicalEqualityPred>(p2.pred))
            return contains_tv(eq2->t1, *tv) or contains_tv(eq2->t2, *tv);
        else
            std::abort();
    }
    else if (auto mtv = t1.to<Hs::MetaTypeVar>())
    {
        if (auto dict2 = to<CanonicalDictPred>(p2.pred))
            return contains_mtv(dict2->args, *mtv);
        else if (auto eq2 = to<CanonicalEqualityPred>(p2.pred))
            return contains_mtv(eq2->t1, *mtv) or contains_mtv(eq2->t2, *mtv);
        else
            std::abort();
    }
    else if (/*auto tycon =*/ t1.to<Hs::TypeCon>())
        return false;
    else if (/*auto app =*/ t1.to<Hs::TypeApp>())
        return false;
    else
        return false;
}

void Solver::kickout_rewritten(const Predicate& p, std::vector<Predicate>& ps)
{
    // kick-out for inerts that are rewritten by p
    for(int i=0;i<ps.size();i++)
    {
        if (can_rewrite(p, ps[i]))
        {
            work_list.push_back(ps[i]);

            if (i+1 < ps.size())
                std::swap(ps[i], ps.back());

            ps.pop_back();
            i--;
            continue;
        }
    }
}

Core::Decls Solver::simplify(const LIE& givens, LIE& wanteds)
{
    if (wanteds.empty()) return {{}, {}};

    for(auto& [evar, constraint]: wanteds)
        work_list.push_back({Wanted, NonCanonicalPred(evar, constraint)});
    // Givens must be processed first!
    for(auto& [evar, constraint]: givens)
        work_list.push_back({Given, NonCanonicalPred(evar, constraint)});

//    std::cerr<<"---------------\n";
//    for(auto& w: work_list)
//        std::cerr<<"  "<<w.print()<<"\n";

    while(not work_list.empty())
    {
        auto p = work_list.back(); work_list.pop_back();

        // canonicalize
        if (auto cp = canonicalize(p))
            p = *cp;
        else
            continue;

        // interact
        bool done = false;
        for(auto& inert: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs, inerts.dicts, inerts.irreducible, inerts.failed))
        {
            auto I = interact(inert, p);
            if (auto C = to<Changed>(I))
            {
                p = C->P;
            }
            else if (to<Solved>(I) or to<NonCanon>(I))
            {
                done = true;
                break;
            }
        }
        if (done) continue;

        // kick-out for inerts that are rewritten by p
        kickout_rewritten(p, inerts.tv_eqs);
        kickout_rewritten(p, inerts.mtv_eqs);
        kickout_rewritten(p, inerts.tyfam_eqs);
        kickout_rewritten(p, inerts.dicts);
        kickout_rewritten(p, inerts.irreducible);
        kickout_rewritten(p, inerts.failed);

        // top-level reactions
        if (top_react(p))
            continue;


        // perform same-level substitutions
        if (auto E = to<CanonicalEqualityPred>(p.pred); E and p.flavor == Wanted)
        {
            auto t1 = Hs::follow_meta_type_var(E->t1);
            if (auto mtv = t1.to<Hs::MetaTypeVar>())
            {
                if (mtv->level() == level)
                {
                    mtv->fill(E->t2);
                    kickout_after_unification(*mtv);
                    continue;
                }
                else if (mtv->level() < level and is_touchable(*mtv, E->t2))
                {
                    promote(E->t2, mtv->level());
                    set_unification_level(mtv->level());
                    mtv->fill(E->t2);
                    kickout_after_unification(*mtv);
                    // Iterating at the level of mtv reconstructs the inert set from scratch, so ...
                    // we don't need to do kick-out at that level?
                    continue;

                    // 3. maybe track which level the constraints are added on?  that way wanteds on a higher
                    //    level become givens automatically...

                    // 4. create a CtLoc object and set it when we first create constraints...

                    // 5. create a TcLclEnv object, and give one to implications also

                    // 6. what to do with ic_given_eqs on implications?
                }
            }
        }

        // we should only get this far if p is closed under rewriting, and unsolved.
        add_inert(p);
    }

    if (not inerts.failed.empty())
    {
        myexception e("Unsolvable equations:\n");
        for(auto& f: inerts.failed)
            e<<"  "<<f.print()<<"\n";
        throw e;
    }

    // Split inert into substitution and remaining constraints
    wanteds.clear();
    for(auto& P: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs, inerts.dicts, inerts.irreducible, inerts.failed))
    {
        assert(is_canonical(P));

        if (P.flavor == Wanted)
        {
            if (auto eq = to<CanonicalEqualityPred>(P.pred))
                wanteds.push_back({eq->co, Hs::make_equality_constraint(eq->t1, eq->t2)});
            else if (auto dict = to<CanonicalDictPred>(P.pred))
            {
                auto constraint = Hs::make_tyapps(dict->klass, dict->args);
                wanteds.push_back({dict->dvar, constraint});
            }
        }
    }

//    std::cerr<<" residual wanteds = \n";
//    for(auto& [v,c]: residual_wanteds)
//        std::cerr<<"  "<<v.print()<<" :  "<<c.print()<<"\n";

    return decls;
}

bool contains_equality_constraints(const LIE& givens)
{
    for(auto& [_,constraint]: givens)
        if (is_equality_constraint(constraint))
            return true;
    return false;
}

Core::Decls TypeChecker::entails(const LIE& givens, WantedConstraints& wanteds)
{
    Core::Decls decls;
    bool update = false;
    do
    {
        // 1. Simplify the simple wanteds.
        Solver solver(*this);
        decls += solver.simplify(givens, wanteds.simple);
        update = false;

        // 2. Handle implications
        vector<shared_ptr<Implication>> wanted_implics;
        LIE new_wanteds;
        std::swap(wanted_implics, wanteds.implications);
        for(auto& implic: wanted_implics)
        {
            // 3. construct sub-givens
            LIE sub_givens = implic->givens;
            sub_givens += givens;
            sub_givens += wanteds.simple;

            // 4. try and sub-wanteds
            auto tcs2 = copy_clear_wanteds();
            tcs2.level = implic->level;
            *implic->evidence_binds += tcs2.entails(sub_givens, implic->wanteds);

            // 5. Promote any level+1 meta-vars and complain about level+1 skolem vars.
            LIE lie_residual_keep;
            if (not contains_equality_constraints(implic->givens))
            {
                for(auto& [var, constraint]: implic->wanteds.simple)
                {
                    promote(constraint, level);

                    if (intersects(free_type_variables(constraint), implic->tvs))
                        lie_residual_keep.push_back({var,constraint});
                    else
                    {
                        // If we've discovered a new simple wanted, then we need to run simplification again.
                        update = true;
                        new_wanteds.push_back({var,constraint});
                    }
                }
                implic->wanteds.simple.clear();
            }

            // 6. Issue collected warnings constraints that couldn't be derived from the givens.
            if (not lie_residual_keep.empty())
                throw myexception()<<"Can't derive constraints '"<<print(lie_residual_keep)<<"' from specified constraints '"<<print(givens)<<"'";

            // 7. Keep implication if not empty.
            if (not implic->wanteds.empty())
                wanteds.implications.push_back( implic );

            // 8. If there was a unification, that affected this level, then we have to iterate.
            if (unification_level() and *unification_level() <= level)
                break;
        }

        wanteds.simple += new_wanteds;

        // If there was a unification, that affected this level, then we have to iterate.
        if (unification_level() and *unification_level() == level)
        {
            update = true;
            clear_unification_level();
        }

    } while(update);

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

    return decls;
}

std::vector<Predicate> make_predicates(ConstraintFlavor f, const std::vector<std::pair<Core::Var, Hs::Type>>& ps)
{
    vector<Predicate> predicates;
    for(auto& [cvar, constraint]: ps)
        predicates.push_back(Predicate(f,NonCanonicalPred(cvar,constraint)));
    return predicates;
}

Solver::Solver(const TypeChecker& tc)
    :TypeChecker(tc)
{
}
