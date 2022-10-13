#include "typecheck.H"
#include "kindcheck.H"

#include "util/set.H"   // for add( , )

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

// Constraints for defaulting must be of the form K a (e.g. Num a) where a is a MetaTypeVar.
optional<Hs::TypeCon> simple_constraint_class_meta(const Hs::Type& constraint)
{
    auto [tycon, args] = Hs::decompose_type_apps(constraint);

    // Only one constrained type.
    if (args.size() != 1) return {};

    // The type is a typevar
    if (not args[0].is_a<Hs::MetaTypeVar>()) return {};

    // The constraint should be a TyCon, not (say) a variable.
    auto tc = tycon.to<Hs::TypeCon>();
    if (not tc) return {};

    return *tc;
}

// The defaulting criteria for an ambiguous type variable v are:
// 1. v appears only in constraints of the form C v , where C is a class
// 2. at least one of these classes is a numeric class, (that is, Num or a subclass of Num)
// 3. all of these classes are defined in the Prelude or a standard library (Figures 6.2–6.3 show the numeric classes, and Figure 6.1 shows the classes defined in the Prelude.)

optional<Core::Decls>
typechecker_state::candidates(const Hs::MetaTypeVar& tv, const LIE& tv_lie)
{
    set<string> num_classes_ = {"Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"};
    set<string> std_classes_ = {"Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"};
    add(std_classes_, num_classes_);

    set<string> num_classes;
    set<string> std_classes;
    for(auto& cls: num_classes_)
        num_classes.insert( find_prelude_tycon_name(cls) );

    for(auto& cls: std_classes_)
        std_classes.insert( find_prelude_tycon_name(cls) );

    bool any_num = false;
    for(auto& [dvar,constraint]: tv_lie)
    {
        // Fail if any of the predicates is not a simple constraint.
        auto tycon = simple_constraint_class_meta(constraint);
        if (not tycon) return {};

        auto& name = unloc(tycon->name);
        if (num_classes.count(name))
            any_num = true;

        // Fail if any of the predicates are not in the standard prelude.
        if (not std_classes.count(name)) return {};
    }

    // Fail if none of the predicates is a numerical constraint
    if (not any_num) return {};

    for(auto& type: defaults() )
    {
        tv.fill(type);
        auto [decls, failed_constraints] = entails({}, tv_lie);
        if (failed_constraints.empty())
            return decls;
        else
            tv.clear();
    }

    return {};
}

pair<LIE, map<Hs::MetaTypeVar, LIE>>
ambiguities(const LIE& lie)
{
    auto ambiguous_tvs = free_meta_type_variables(lie);

    // 1. Record the constraints WITH ambiguous type vars, by type var
    map<Hs::MetaTypeVar, LIE> ambiguities;
    for(auto& ambiguous_tv: ambiguous_tvs)
    {
        LIE lie_for_tv;
        for(auto& [dvar,constraint]: lie)
        {
            if (free_meta_type_variables(constraint).count(ambiguous_tv))
                lie_for_tv.push_back({dvar,constraint});
        }
        if (not lie_for_tv.empty())
            ambiguities.insert({ambiguous_tv, lie_for_tv});
    }

    // 2. Find the constraints WITHOUT ambiguous type vars
    LIE unambiguous_preds;

    for(auto& [dvar, constraint]: lie)
    {
        auto ftvs = free_meta_type_variables(constraint);
        if (not intersects(ftvs, ambiguous_tvs))
            unambiguous_preds.push_back({dvar, constraint});
    }

    return {unambiguous_preds, ambiguities};
}


Core::Decls typechecker_state::default_preds( WantedConstraints& wanted )
{
    Core::Decls decls;
    auto [unambiguous_preds, ambiguous_preds_by_var] = ambiguities( wanted.simple );

    for(auto& [tv, preds]: ambiguous_preds_by_var)
    {
        auto result = candidates(tv, preds);

        if (not result)
        {
            auto e = myexception()<<"Ambiguous type variable '"<<tv<<"' in classes: ";
            for(auto& [dvar,constraint]: preds)
                e<<constraint<<" ";
            throw e;
        }
        auto& decls1 = *result;

        decls += decls1;
    }
    wanted.simple = unambiguous_preds;

    vector<std::shared_ptr<Implication>> keep;
    for(auto& implication: wanted.implications)
    {
        decls += default_preds(implication->wanteds);
        if (not implication->wanteds.empty())
            keep.push_back(implication);
    }
    std::swap( wanted.implications, keep );

    return decls;
}

//////////////////////////////////////////////////////////////////////////////////////////////////

Core::Decls typechecker_state::simplify_and_default_top_level()
{
    auto [top_simplify_decls, new_wanteds] = entails( {}, current_wanteds() );

    collected_wanteds = new_wanteds;

    auto default_decls = default_preds( current_wanteds() );

    if (not current_wanteds().empty())
        throw myexception()<<"Failed to solve wanteds: "<<print(current_wanteds().all_simple());

    // Clear the LIE, which should now be empty.
    current_wanteds() = {};

//    std::cerr<<"GVE (all after defaulting):\n";
//    for(auto& [x,t]: state.gve)
//    {
//        std::cerr<<x<<" :: "<<alphabetize_type(t)<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
//    }
//    std::cerr<<"\n";

    top_simplify_decls += default_decls;

    return top_simplify_decls;
}

