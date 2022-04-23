#include "typecheck.H"
#include "kindcheck.H"
#include "parser/rename.H"

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
  NOTE:
  - Type signatures with constraints are forbidden for variables declared in PatBind's!!!
    This might mean that we can simply instantiate the signatures inside the pattern -- and we 
       can issue an error message if there are any constraints.

  NOTE: It also looks like GHC DOES use 1-way matching -- but it implements it as a bool flag.

  Done: 
  * Check that constraints in classes only mention type vars.
  * Check that constraints in instance heads are of the form Class (Tycon a1 a2 a3..)
  * Switch from substitutions to constraints (i.e. from compose( ) to combine( ) ).
  * Add a substitution to the typechecker_state, instead of returning substitutions from every call.
  * We no longer need to keep substituting into the type.
  * Handle exp :: type expressions.
  * Monomorphism restriction.
  * Defaulting.
  * Make defaulting happen at every binding.
  * Make a version of unification that returns optional instead of throwing.
  * Make a stack of LIEs.
  * Don't substitute into LIEs / LVEs / GVEs until we need to.
  * Handle a :: Num a => Char in (a,b) = ('a',1)
  * Partially handle polymorphic recursion.
  * Split decls into pieces.
  * Handle expression :: type
  * Handle explicit signatures in fundecls / simple-pattern bindings.
  * Handle explicit signatures in pattern bindings.

  TODO:
  0. Double-check logic in Gen/Binds.hs for explicit type signatures...
  0. Change the type of class methods to forall a.C a => (forall b. ctxt => body)
  0. Type-check / kind-check user-written signatures.
  0. Implement explicit types in terms of TypedExp -- match + entails to check predicates
    + GHC/Hs/Binds.hs
    + GHC/Tc/Gen/Binds.hs
    + Basically, I need to read all of Gen/Bind.hs :-(
      - See note [Impedance matching]
  0. Record impedance-matching info in GenBind (and perhaps rename it).
  0. Write more-general impedance-matching code.
  0. Avoid a space leak with polymorphic recursion in cases like factorial.
     Do not create new dictionaries for each call at the same type.
  1. Check that constraints in instance contexts satisfy the "paterson conditions"
  2. How do we export stuff?
  3. Make functions to handle instance declarations from Figure 12.
  4. Handle instances in two passes:
     - Can we first the the NAME and TYPE for all the instance variables,
       and second generate the instance dfun bodies?
     - Possibly generating the dfun bodies AFTER the value declarations are done?
     - How do we figure out if the instance contexts can be satisfied in a mutally recursive manner?
  5. Make AST nodes for dictionary abstraction and dictionary application.
     - \(dicts::theta) -> monobinds in binds
     - exp <dicts>
     - (superdicts, methods)
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?
  7. Implement fromInt and fromRational
  8. Implement literal strings.  Given them type [Char] and turn them into lists during desugaring.
      Do I need to handle LiteralString patterns, then?
  9. Check that there are no cycles in the class hierarchy.
  11. Emit code for instances and check if there are instances for the context.
  13. Handle constraints on constructors.
  14. Remove the constraint from EmptySet
  17. Replace types with type synonyms.
  18. Reject unification of variables, tycons, etc with different kinds.
  19. Add basic error reporting.
  21. Should we consider having infer_type( ) modify expressions in-place instace of 
      returning a new one?
  22. How do we handle things like Prelude.Num, Prelude.Enum, Prelude.fromInt, etc.
      Right now, maybe we can pick a Num / fromInt from the local scope, instead?
      This might require passing some information from the renamer into the typechecker...
  23. Handle literal constant patterns.  We need a Num or Fractional dictionary for
      Int or Double constants.  I guess we need an Eq Char, or Eq [Char] dictionary for
      characters or strings?

  Questions:
  1. How do we handle predicates that make it to the top level?
    - If they are tautological, we can remove them and infer the dictionary.
    - Otherwise we try to default them, if they have a type variable.
  2. Is THIH correct about binding groups in Haskell 2010?
     - It seems that basically all the explicitly-typed things should be at the END.
     - In that case, every explicitly-typed thing would be in its OWN group.
     - We need the explicit types BEFORE we type the thing.
     - So they should be attached to Hs::Binds, not Hs::Decls.
  3. Does normal Haskell avoid context reduction before generalization?
     - choice 2d in "Type classes - an exploration of the design space" suggests
       we should avoid using instances to simplify constraints.
     - but then wouldn't we end up with constraints like Eq [a]?
     - maybe this describes a non-standard extension.

  4. What does entails(x,y) return inside GHC?  If not Binds, then what?  It seems like we
     actually want a formula for all the dvars in y in terms of x.

  5. If the LHS type for a var is more general than its explicit type, how to we do the
     impedance matching?

  6. When a var has an explicit type, on the right hand side, but not on the LHS, how
     do we define the actual exported var?  And how do we ensure that the resulting definition
     is visible on the RHS?  Maybe define the tmp-tuple and the exported var in the same let-rec 
     block?

  7. If we are checking the RHS of a FunDecl with a signature, it seems that we could
     create constraints that refer to type variables from higher scopes, or perhaps
     variables from this scope that do not occur in the type.

     When we check to see that the "wanted" constraints are implied by the "given"
     constraints, we shouldn't need to check these constraints should we?

  Cleanups:
  1. Implement kinds as Hs::Type

  Unification:
  1. If I somehow defer "zonking" to the end, can I avoid manually applying all these
     substitutions?
  2. Can we get better error messages by listing an expected type?
  3. Can we prefix with "heralds" for better error messages?
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

template <typename T>
std::set<T> operator-(const std::set<T>& s1, const std::set<T>& s2)
{
    return minus(s1,s2);
}

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

std::set<Hs::TypeVar> free_type_variables(const Hs::Type& t)
{
    return free_type_VARS(t);
}

std::set<Hs::TypeVar> free_type_variables(const value_env& env)
{
    std::set<Hs::TypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_type_variables(type));
    return free;
}

optional<string> maybe_get_class_name_from_constraint(const Hs::Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);
    if (auto tc = tycon.to<Hs::TypeCon>())
        return get_unqualified_name(unloc(tc->name));
    else
        return {};
}

string get_class_name_from_constraint(const Hs::Type& constraint)
{
    if (auto name = maybe_get_class_name_from_constraint(constraint))
        return *name;
    else
        return "Constraint";
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

local_instance_env& typechecker_state::current_lie() {
    return lie_stack.back();
}

void typechecker_state::add_dvar(const string& name, const Hs::Type& constraint)
{
    auto& lie = current_lie();
    assert(not lie.count(name));
    lie = lie.insert( {name, constraint} );
}

void typechecker_state::add_dvar(const Hs::Var& dvar, const Hs::Type& constraint)
{
    add_dvar(unloc(dvar.name), constraint);
}

Hs::Var typechecker_state::fresh_dvar(const Hs::Type& constraint)
{
    string name = "dvar";
    if (auto cname = maybe_get_class_name_from_constraint(constraint))
        name = "d" + *cname;
    return fresh_var(name, false);
}

Hs::Var typechecker_state::add_dvar(const Hs::Type& constraint)
{
    auto dvar = fresh_dvar(constraint);

    add_dvar(dvar, constraint);

    return dvar;
}

void typechecker_state::push_lie() {
    lie_stack.push_back( {} );
}

local_instance_env typechecker_state::pop_lie()
{
    auto lie = current_lie();
    lie_stack.pop_back();
    return lie;
}

void typechecker_state::pop_and_add_lie()
{
    auto lie = pop_lie();
    current_lie() += lie;
}

typechecker_state::typechecker_state(const string& s, const Module& m, const Hs::ModuleDecls& M, const constr_env& ce)
    :con_info(ce),
     mod_name(s),
     this_mod(m)
{
    push_lie();

    if (M.default_decl)
        defaults = M.default_decl->types;
    else
        defaults = { Hs::TypeCon({noloc,"Int"}), Hs::TypeCon({noloc,"Double"}) };
}

Hs::Var typechecker_state::find_prelude_var(string name) const
{
    if (this_mod.is_declared(name))
        name = this_mod.lookup_symbol(name).name;
    return Hs::Var({noloc, name});
}

string typechecker_state::find_prelude_tycon_name(const string& name) const
{
    if (this_mod.type_is_declared(name))
        return this_mod.lookup_type(name).name;
    else
        return name;
}

Hs::TypeCon typechecker_state::find_prelude_tycon(const string& name) const
{
    auto prelude_name = find_prelude_tycon_name(name);
    return Hs::TypeCon({noloc, prelude_name });
}

Hs::Type typechecker_state::bool_type() const
{
    return find_prelude_tycon("Bool");
}

Hs::Type typechecker_state::char_type() const
{
    return find_prelude_tycon("Char");
}

Hs::Type typechecker_state::enum_class(const Hs::Type& arg) const
{
    auto Enum = find_prelude_tycon("Enum");

    return Hs::TypeApp( Enum, arg);
}

Hs::Type typechecker_state::num_class(const Hs::Type& arg) const
{
    auto Num = find_prelude_tycon("Num");

    return Hs::TypeApp( Num, arg);
}

Hs::Type typechecker_state::fractional_class(const Hs::Type& arg) const
{
    auto Fractional = find_prelude_tycon("Fractional");

    return Hs::TypeApp( Fractional, arg);
}

tuple<Hs::Var, Hs::Type> typechecker_state::fresh_enum_type(bool meta)
{
    Hs::Type a = fresh_type_var(meta);
    Hs::Type enum_a = enum_class(a);
    auto dvar = add_dvar(enum_a);
    return {dvar, a};
}

tuple<Hs::Var, Hs::Type> typechecker_state::fresh_num_type(bool meta)
{
    Hs::Type a = fresh_type_var(meta);
    Hs::Type num_a = num_class(a);
    auto dvar = add_dvar(num_a);
    return {dvar, a};
}

tuple<Hs::Var, Hs::Type> typechecker_state::fresh_fractional_type(bool meta)
{
    Hs::Type a = fresh_type_var(meta);
    Hs::Type fractional_a = fractional_class(a);
    auto dvar = add_dvar(fractional_a);
    return {dvar, a};
}

bool typechecker_state::add_substitution(const substitution_t& s)
{
    if (auto s2 = combine(type_var_to_type, s))
    {
        type_var_to_type = *s2;
        return true;
    }
    else
        return false;
}

bool typechecker_state::add_substitution(const Hs::TypeVar& a, const Hs::Type& type)
{
    if (auto s = try_insert({}, a, type))
        return add_substitution(*s);
    else
        return false;
}

bool typechecker_state::maybe_unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto s = ::maybe_unify(t1, t2))
        return add_substitution(*s);
    else
        return false;
}

void typechecker_state::unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (not maybe_unify(t1,t2))
        throw myexception()<<"Unification failed: "<<apply_current_subst(t1)<<" !~ "<<apply_current_subst(t2);
}

void typechecker_state::unify(const Hs::Type& t1, const Hs::Type& t2, const myexception& e)
{
    if (not maybe_unify(t1,t2))
        throw e;
}

Hs::Var typechecker_state::fresh_var(const std::string& s, bool qualified)
{
    string name = "$"+s+std::to_string(next_var_index);
    if (qualified)
        name = mod_name + "." + name;
    Hs::Var x({noloc, name});
    next_var_index++;
    return x;
}

// "Rigid" type vars come from forall-quantified variables.
// "Wobbly" type vars come from existentially-quantified variables (I think).  We don't have any.
// "Meta" type vars are unification type vars.
Hs::TypeVar typechecker_state::fresh_rigid_type_var() {
    Hs::TypeVar tv({noloc, "t"+std::to_string(next_tvar_index)});
    next_tvar_index++;
    tv.info = Hs::typevar_info::rigid;
    return tv;
}

Hs::TypeVar typechecker_state::fresh_meta_type_var() {
    Hs::TypeVar tv({noloc, "t"+std::to_string(next_tvar_index)});
    next_tvar_index++;
    tv.info = Hs::typevar_info::meta;
    return tv;
}

Hs::TypeVar typechecker_state::fresh_type_var(bool meta) {
    if (meta)
        return fresh_meta_type_var();
    else
        return fresh_rigid_type_var();
}

pair<local_instance_env, map<Hs::TypeVar, local_instance_env>>
ambiguities(const set<Hs::TypeVar>& tvs1, const set<Hs::TypeVar>& tvs2, local_instance_env lie)
{
    // The input lie MUST be substituted to find its free type vars!
    // lie = apply_current_subst(lie);
    auto ambiguous_tvs = free_type_variables(lie) - tvs1 - tvs2;

    // 1. Record the constraints WITH ambiguous type vars, by type var
    map<Hs::TypeVar,local_instance_env> ambiguities;
    for(auto& ambiguous_tv: ambiguous_tvs)
    {
        local_instance_env lie_for_tv;
        for(auto& [dvar,constraint]: lie)
        {
            if (free_type_variables(constraint).count(ambiguous_tv))
                lie_for_tv = lie_for_tv.insert({dvar,constraint});
        }
        if (not lie_for_tv.empty())
            ambiguities.insert({ambiguous_tv, lie_for_tv});
    }

    // 2. Find the constraints WITHOUT ambiguous type vars
    local_instance_env unambiguous_preds;

    for(auto& [dvar, constraint]: lie)
    {
        auto ftvs = free_type_variables(constraint);
        if (not intersects(ftvs, ambiguous_tvs))
            unambiguous_preds = unambiguous_preds.insert({dvar, constraint});
    }

    return {unambiguous_preds, ambiguities};
}


// Constraints for defaulting must be of the form K a (e.g. Num a)
optional<Hs::TypeCon> simple_constraint_class(const Hs::Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);

    // Only one constrained type.
    if (args.size() != 1) return {};

    // The type is a typevar
    if (not args[0].is_a<Hs::TypeVar>()) return {};

    // The constraint should be a TyCon, not (say) a variable.
    auto tc = tycon.to<Hs::TypeCon>();
    if (not tc) return {};

    return *tc;
}

// The defaulting criteria for an ambiguous type variable v are:
// 1. v appears only in constraints of the form C v , where C is a class
// 2. at least one of these classes is a numeric class, (that is, Num or a subclass of Num)
// 3. all of these classes are defined in the Prelude or a standard library (Figures 6.2–6.3 show the numeric classes, and Figure 6.1 shows the classes defined in the Prelude.)

optional<tuple<substitution_t, Hs::Binds>>
typechecker_state::candidates(const Hs::TypeVar& tv, const local_instance_env& tv_lie)
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
        auto tycon = simple_constraint_class(constraint);
        if (not tycon) return {};

        auto& name = unloc(tycon->name);
        if (num_classes.count(name))
            any_num = true;

        // Fail if any of the predicates are not in the standard prelude.
        if (not std_classes.count(name)) return {};
    }

    // Fail if none of the predicates is a numerical constraint
    if (not any_num) return {};

    for(auto& type: defaults)
    {
        substitution_t s;
        s = s.insert({tv, type});
        if (auto binds = entails({}, apply_subst(s, tv_lie)))
            return pair(s, *binds);
    }

    return {};
}

Hs::Binds typechecker_state::default_subst()
{
    auto [s, binds, unambiguous_preds] = default_preds({}, {}, current_lie());
    assert(unambiguous_preds.empty());

    // Record the substitution, since it can affect types.
    bool ok = add_substitution(s);
    assert(ok);

    // Clear the LIE, which should now be empty.
    current_lie() = {};

    return binds;
}

set<Hs::TypeVar> free_type_variables(const Hs::Type& t);

pair<Hs::Type, vector<Hs::Type>> typechecker_state::constr_types(const Hs::Con& con)
{
    auto& con_name = unloc(con.name);

    if (con_name == ":")
    {
        Hs::Type a = fresh_meta_type_var();
        return {Hs::ListType(a),{a,Hs::ListType(a)}};
    }
    else if (con_name == "[]")
    {
        Hs::Type a = fresh_meta_type_var();
        return {Hs::ListType(a),{}};
    }
    else if (is_tuple_name(con_name))
    {
        int n = tuple_arity(con_name);
        vector<Hs::Type> types;
        for(int i=0;i<n;i++)
            types.push_back(fresh_meta_type_var());
        return {Hs::TupleType(types),types};
    }

    // 1. Find the data type
    if (not con_info.count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;
    auto [_, constraints, con_type] = instantiate(con_info.at(con_name));
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

value_env add_constraints(const std::vector<Haskell::Type>& constraints, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, add_constraints(constraints, monotype)} );
    return env2;
}

template <typename T>
Hs::Type quantify(const T& tvs, const Hs::Type& monotype)
{
    if (tvs.empty())
        return monotype;
    else
        return Hs::ForallType(tvs | ranges::to<vector>, monotype);
}

template <typename T>
value_env quantify(const T& tvs, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, quantify(tvs, monotype)} );
    return env2;
}

Hs::Type generalize(const global_value_env& env, const Hs::Type& monotype)
{
    auto ftv1 = free_type_variables(monotype);
    auto ftv2 = free_type_variables(env);
    for(auto tv: ftv2)
        ftv1.erase(tv);

    return Hs::ForallType(ftv1 | ranges::to<vector>, monotype);
}

tuple<vector<Hs::TypeVar>, vector<Hs::Type>, Hs::Type> typechecker_state::instantiate(const Hs::Type& t, bool meta)
{
    // 1. Handle foralls
    vector<Hs::TypeVar> tvs;
    substitution_t s;
    auto type = t;
    while(auto fa = type.to<Hs::ForallType>())
    {
        for(auto& tv: fa->type_var_binders)
        {
            auto new_tv = fresh_type_var(meta);
            new_tv.kind = tv.kind;
            s = s.insert({tv,new_tv});

            tvs.push_back(new_tv);
        }
        type = fa->type;
    }
    type = apply_subst(s,type);

    // 2. Handle constraints
    vector<Hs::Type> constraints;
    if (auto ct = type.to<Hs::ConstrainedType>())
    {
        constraints = ct->context.constraints;
        type = ct->type;
    }
    return {tvs, constraints,type};
}

vector<Hs::Type> constraints_from_lie(const local_instance_env& lie)
{
    vector<Hs::Type> constraints;
    for(auto& [_, constraint]: lie)
        constraints.push_back(constraint);
    return constraints;
}

vector<Hs::Var> vars_from_lie(const local_instance_env& lie)
{
    vector<Hs::Var> dict_vars;
    for(auto& [name, constraint]: lie)
    {
        Hs::Var dict_var({noloc,name});
        dict_var.type = constraint;
        dict_vars.push_back( dict_var );
    }
    return dict_vars;
}

vector<Hs::Var> vars_from_lie(const vector<pair<Hs::Var, Hs::Type>>& lie)
{
    vector<Hs::Var> vars;
    for(auto& [var, constraint]: lie)
        vars.push_back( var );
    return vars;
}

global_value_env typechecker_state::sig_env(const map<string, Hs::Type>& signatures)
{
    global_value_env sig_env;
    for(auto& [name, type]: signatures)
    {
        // FIXME! We need to translate type synonyms
        Hs::Type qualified_type = add_forall_vars(free_type_variables(type) | ranges::to<vector>, type);
        sig_env = sig_env.insert({name, qualified_type});
    }
    return sig_env;
}

tuple<Hs::Binds, global_value_env>
typechecker_state::infer_type_for_binds(const global_value_env& env, Hs::Binds binds)
{
    auto env2 = plus_prefer_right(env, sig_env(binds.signatures));

    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [decls1, binders1] = infer_type_for_decls(env2, binds.signatures, decls);
        decls = decls1;
        // We could remove the binders with sigs
        env2 = plus_prefer_right(env2, binders1);
        binders += binders1;
    }

    return {binds, binders};
}

bool type_is_hnf(const Hs::Type& type)
{
    auto [head,args] = decompose_type_apps(type);

    if (head.is_a<Hs::TypeVar>())
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
    auto [class_con, args] = decompose_type_apps(constraint);
    for(auto& arg: args)
        if (not type_is_hnf(arg))
            return false;
    return true;
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

optional<pair<Hs::Var,vector<Hs::Type>>> typechecker_state::lookup_instance(const Hs::Type& constraint)
{
    for(auto& [name, type]: gie)
    {
        auto [_, instance_constraints, instance_head] = instantiate(type);

        // Skip if this is not an instance.
        if (constraint_is_hnf(instance_head)) continue;

        auto s = ::maybe_match(instance_head, constraint);

        // This instance doesn't match.
        if (not s) continue;

        for(auto& instance_constraint: instance_constraints)
            instance_constraint = apply_subst(*s, instance_constraint);

        auto dfun = Hs::Var({noloc, name});

        return {{dfun, instance_constraints}};
    }
    return {};
}

pair<Hs::Binds,local_instance_env> typechecker_state::toHnf(const string& name, const Hs::Type& constraint)
{
    Hs::Binds binds;
    local_instance_env lie;
    if (constraint_is_hnf(constraint))
    {
        lie = lie.insert({name, constraint});
    }
    else
    {
        local_instance_env lie2;

        // 1. find the (single) matching instance or fail.
        // The instance will be of the form
        //     dfun :: forall  a1 a2 a3. (constraint, constraint, constraint) => K (T b1 b2 b3)
        // We need to substitute into this definition to make K (T b1 b2 b3) == constraint.
        auto instance = lookup_instance(constraint);
        if (not instance)
            throw myexception()<<"No instance for '"<<constraint<<"'";

        auto [dfun,new_constraints] = *instance;

        // 2. We need to make up new dvar names for all the input constraints.
        // Then we would add to the decls:
        //     dvar_orig = dfun dvar_new1 dvar_new2 dvar_new3
        // And we would get a NEW lie:
        //     dvar_new1 :: constraint1
        //     dvar_new2 :: constraint2
        //     dvar_new3 :: constraint3
        expression_ref rhs = dfun;
        for(auto& new_constraint: new_constraints)
        {
            auto dvar = fresh_var("dvar", false);
            lie2 = lie2.insert({unloc(dvar.name), new_constraint});
            rhs = {rhs,dvar};
        }

        Hs::Var dvar({noloc, name});

        Hs::Decls decls;
        decls.push_back(Hs::simple_decl(dvar, rhs));

        // 3. Finally, we may need to further simplify the new LIE
        auto [binds2, lie3] = toHnfs(lie2);

        lie = lie3;
        binds = binds2;
        binds.push_back(decls);
    }
    return {binds,lie};
}

pair<Hs::Binds, local_instance_env>
typechecker_state::toHnfs(const local_instance_env& lie_in)
{
    Hs::Binds binds_out;
    local_instance_env lie_out;
    for(auto& [name, constraint]: lie_in)
    {
        auto [binds2, lie2] = toHnf(name, constraint);
        for(auto& bind: binds2)
            binds_out.push_back(bind);
        lie_out += lie2;
    }
    return {binds_out, lie_out};
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<string, Hs::Type>> typechecker_state::superclass_constraints(const Hs::Type& constraint)
{
    vector<pair<string, Hs::Type>> constraints;

    for(auto& [name, type]: gie)
    {
        // Klass a => Superklass a
        auto [_, class_constraints, superclass_constraint] = instantiate(type, false);

        // Skip if this is not a method of extracting superclass dictionaries
        if (not constraint_is_hnf(superclass_constraint)) continue;

        assert(class_constraints.size() == 1);

        auto class_constraint = class_constraints[0];
        auto s = ::maybe_match(class_constraint, constraint);

        // The premise doesn't match the current class;
        if (not s) continue;

        superclass_constraint = apply_subst(*s, superclass_constraint);

        constraints.push_back( { name, superclass_constraint } );
    }

    return constraints;
}

// We are trying to eliminate the *first* argument.
optional<vector<string>> typechecker_state::is_superclass_of(const Hs::Type& constraint1, const Hs::Type& constraint2)
{
    vector<string> extractors;
    if (same_type(constraint1, constraint2))
        return extractors;
    else
    {
        // dvar1 :: constraint1 => dvar3 :: constraint3 => dvar2 :: constraint2
        for(auto& [name, constraint3]: superclass_constraints(constraint2))
        {
            if (auto extractors2 = is_superclass_of(constraint1, constraint3))
            {
                extractors = std::move(*extractors2);
                extractors.push_back(name);
                return extractors;
            }
        }
        return {};
    }
}

optional<Hs::Binds> typechecker_state::entails_by_superclass(const pair<string, Hs::Type>& to_keep, const pair<string, Hs::Type>& to_remove)
{
    auto& [dvar_to_keep_name, constraint_to_keep] = to_keep;
    auto& [dvar_to_remove_name, constraint_to_remove] = to_remove;

    if (auto extractors = is_superclass_of(constraint_to_remove, constraint_to_keep))
    {
        Hs::Var dvar_to_keep({noloc, dvar_to_keep_name});
        Hs::Var dvar_to_remove({noloc, dvar_to_remove_name});

        Hs::Exp dict_exp = dvar_to_keep;
        for(auto& extractor: *extractors | views::reverse)
        {
            Hs::Var get_dict({noloc, extractor});
            dict_exp = {get_dict, dict_exp};
        }

        Hs::Decls decls;
        // dvar_to_remove = extractor[n] extractor[n-1] ... extractor[0] dvar_to_keep
        decls.push_back( Hs::simple_decl(dvar_to_remove, dict_exp) );
        Hs::Binds binds;
        binds.push_back(decls);
        return binds;
    }
    else
        return {};
}

vector<pair<Hs::Var,Hs::Type>> typechecker_state::constraints_to_lie(const vector<Hs::Type>& constraints)
{
    vector<pair<Hs::Var, Hs::Type>> ordered_lie;
    for(auto& constraint:constraints)
    {
        auto dvar = fresh_var("dvar", false);
        dvar.type = constraint;
        ordered_lie.push_back({dvar, constraint});
    }
    return ordered_lie;
}

local_instance_env unordered_lie(const vector<pair<Hs::Var, Hs::Type>>& lie1)
{
    local_instance_env lie2;
    for(auto& [var,constraint]: lie1)
        lie2 = lie2.insert({unloc(var.name), constraint});
    return lie2;
}

// How does this relate to simplifying constraints?
optional<Hs::Binds> typechecker_state::entails(const local_instance_env& lie1, const local_instance_env& lie2)
{
    Hs::Binds binds;
    for(auto& constraint: lie2)
    {
        auto binds1 = entails(lie1, constraint);
        if (not binds1)
            return {};
        ranges::insert(binds, binds.begin(), *binds1);
    }
    return binds;
}

pair<Hs::Binds, local_instance_env> typechecker_state::simplify(const local_instance_env& lie)
{
    Hs::Binds binds_out;
    local_instance_env lie_out;
    vector<pair<string,Hs::Type>> lie_vec;
    for(auto& entry: lie)
       lie_vec.push_back(entry);
    vector<pair<string,Hs::Type>> checked;

    for(int i=0;i<lie_vec.size();i++)
    {
        auto& pred = lie_vec[i];
        auto preds = views::concat(lie_vec | views::drop(i+1), checked);
        if (auto new_binds = entails(preds, pred))
            ranges::insert(binds_out, binds_out.begin(), *new_binds);
        else
            checked.push_back(lie_vec[i]);
    }

    for(auto& var_equals_constraint: checked)
        lie_out = lie_out.insert(var_equals_constraint);

    return {binds_out, lie_out};
}

pair<Hs::Binds, local_instance_env> typechecker_state::reduce(const local_instance_env& lie)
{
    auto [binds1, lie1] = toHnfs(lie);

    auto [binds2, lie2] = simplify(lie1);

    auto binds = binds2;
    for(auto& bind: binds1)
        binds.push_back(bind);

    return {binds, lie2};
}

Hs::Binds typechecker_state::reduce_current_lie()
{
    auto& lie = current_lie();

    lie = apply_current_subst( lie );

    auto [binds, new_lie] = reduce( lie );

    lie = new_lie;

    return binds;
}


tuple<substitution_t, Hs::Binds, local_instance_env>
typechecker_state::default_preds( const set<Hs::TypeVar>& fixed_tvs,
                                  const set<Hs::TypeVar>& referenced_tvs,
                                  const local_instance_env& lie)
{
    substitution_t s;
    Hs::Binds binds;
    auto [unambiguous_preds, ambiguous_preds_by_var] = ambiguities(fixed_tvs, referenced_tvs, lie);

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
        auto& [s1, binds1] = *result;

        auto tmp = combine(s, s1);
        assert(tmp);
        s = *tmp; // These constraints should be on separate variables, and should not interact.

        // Each binds should be independent of the others, so order should not matter.
        ranges::insert(binds, binds.end(), binds1);
    }

    return {s, binds, unambiguous_preds};
}

// Why aren't we using `fixed_type_vars`?
// I guess the deferred constraints that do not mention fixed_type_vars are ambiguous?
pair<local_instance_env, local_instance_env>
classify_constraints(const local_instance_env& lie,
                     const set<Hs::TypeVar>& fixed_type_vars)
{
    local_instance_env lie_deferred;
    local_instance_env lie_retained;

    for(auto& [name, constraint]: lie)
    {
        auto constraint_type_vars = free_type_VARS(constraint);

        // Does the constraint contain any ambiguous vars?
        bool all_fixed = true;
        for(auto& type_var: constraint_type_vars)
            if (not fixed_type_vars.count(type_var))
                all_fixed = false;

        if (all_fixed)
            lie_deferred = lie_deferred.insert({name,constraint});
        else
            lie_retained = lie_retained.insert({name,constraint});
    }
    return {lie_deferred, lie_retained};
}

bool is_restricted(const map<string, Hs::Type>& signatures, const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Hs::PatDecl>())
            return true;
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            // Simple pattern declaration
            if (fd->match.rules[0].patterns.size() == 0)
            {
                auto& name = unloc(fd->v.name);
                if (not signatures.count(name)) return true;
            }
        }
    }
    return false;
};

tuple<Hs::Var, Hs::Type, local_value_env>
typechecker_state::infer_lhs_var_type(Hs::Var v)
{
    auto& name = unloc(v.name);

    Hs::Type type = fresh_meta_type_var();
    v.type = type;

    // Check that this is a NEW name.
    local_value_env lve;
    lve = lve.insert({name,type});
    return {v, type, lve};
}

tuple<expression_ref, Hs::Type, local_value_env>
typechecker_state::infer_lhs_type(const expression_ref& decl, const map<string, Hs::Type>& signatures)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        // If there was a signature, we would have called infer_type_for_single_fundecl_with_sig
        assert(not signatures.count(unloc(FD.v.name)));

        auto [v2, type, lve] = infer_lhs_var_type(FD.v);
        FD.v.type = type;
        return {FD, type, lve};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [lhs, type, lve] = infer_pattern_type(PD.lhs, signatures);
        PD.lhs = lhs;
        return {PD, type, lve};
    }
    else
        std::abort();
}

tuple<expression_ref, Hs::Type>
typechecker_state::infer_rhs_type(const global_value_env& env, const expression_ref& decl)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        auto [match, rhs_type] = infer_type(env, FD.match);
        FD.match = match;

        return {FD, rhs_type};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [rhs, rhs_type] = infer_type(env, PD.rhs);
        PD.rhs = rhs;

        return {PD, rhs_type};
    }
    else
        std::abort();
}

vector<Hs::Decls> split_decls_by_signatures(const Hs::Decls& decls, const map<string, Hs::Type>& signatures)
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

bool single_fundecl_with_sig(const Hs::Decls& decls, const signature_env& signatures)
{
    if (decls.size() != 1) return false;

    auto& decl = decls[0];

    if (not decl.is_a<Hs::FunDecl>()) return false;

    auto& FD = decl.as_<Hs::FunDecl>();

    auto& name = unloc(FD.v.name);

    return signatures.count(name) > 0;
}

value_env remove_sig_binders(value_env binder_env, const signature_env& signatures)
{
    auto no_sig_binder_env = binder_env;
    for(auto& [name,_]: binder_env)
        if (signatures.count(name))
            no_sig_binder_env = no_sig_binder_env.erase(name);
    return no_sig_binder_env;
}

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const signature_env& signatures, const Hs::Decls& decls)
{
    // The signatures for the binders should already be in the environment.

    auto bind_groups = split_decls_by_signatures(decls, signatures);

    auto env2 = env;
    Hs::Decls decls2;
    local_value_env binders;
    for(auto& group: bind_groups)
    {
        auto [group_decls, group_binders] = infer_type_for_decls_groups(env2, signatures, group);

        for(auto& decl: group_decls)
            decls2.push_back(decl);

        binders += group_binders;

        env2 += remove_sig_binders(group_binders, signatures);
    }
    return {decls2, binders};
}

tuple<expression_ref, string, Hs::Type>
typechecker_state::infer_type_for_single_fundecl_with_sig(const global_value_env& env, Hs::FunDecl FD)
{
    auto& name = unloc(FD.v.name);

    auto sig_type = env.at(name);

    // OK, so what we want to do is:

    // 1. instantiate the type -> (tvs, givens, rho-type)
    auto [tvs, given_constraints, given_type] = instantiate(sig_type, false);
    auto ordered_lie_given = constraints_to_lie(given_constraints);
    auto lie_given = unordered_lie(ordered_lie_given);
    
    // 2. typecheck the rhs -> (rhs_type, wanted, body)
    push_lie();
    auto [decl2, most_general_type] = infer_rhs_type(env, FD);
    auto lie_wanted = pop_lie();

    // 3. alpha[i] in most_general_type but not in env
    auto ftv_mgt = free_type_variables(most_general_type) - free_type_variables(env);
    // FIXME -- what if the instantiated type contains variables that are free in the environment?

    // 4. match(given_type <= most_general_type)
    unify(most_general_type, given_type);

    // 5. check if the given => wanted ~ EvBinds
    lie_wanted = apply_current_subst( lie_wanted );
    auto evbinds = entails(lie_given, lie_wanted);
    if (not evbinds)
        throw myexception()<<"Can't derive constraints '"<<print(lie_wanted)<<"' from specified constraints '"<<print(lie_given)<<"'";

    // 5. return GenBind with tvs, givens, body
    auto dict_vars = vars_from_lie( ordered_lie_given );

    Hs::Decls decls;
    decls.push_back(decl2);

    auto decl = Hs::GenBind( tvs, dict_vars, *evbinds, decls );
    return {decl, name, sig_type};
}

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls_groups(const global_value_env& env, const map<string, Hs::Type>& signatures, Hs::Decls decls)
{
    if (single_fundecl_with_sig(decls, signatures))
    {
        auto& FD = decls[0].as_<Hs::FunDecl>();

        auto [decl, name, sig_type] = infer_type_for_single_fundecl_with_sig(env, FD);

        Hs::Decls decls({decl});

        global_value_env binders;;
        binders = binders.insert({name, sig_type});
        return {decls, binders};
    }

// How & when do we complain if there are predicates on signatures with the monomorphism restriction?

    push_lie();

    // 1. Add each let-binder to the environment with a fresh type variable
    value_env binder_env;

    vector<Hs::Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, type, lve] = infer_lhs_type( decls[i], signatures );
        decls[i] = decl;

        binder_env += lve;
        lhs_types.push_back(type);
    }

    auto env2 = env + remove_sig_binders(binder_env, signatures);

    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, rhs_type] = infer_rhs_type(env2, decls[i]);
        decls[i] = decl;

        unify(lhs_types[i], rhs_type);
    }

    // We need to substitute before looking for free type variables!
    // We also need to substitute before we quantify below.
    binder_env = apply_current_subst(binder_env);

    auto fixed_tvs = free_type_variables(env);
    set<Hs::TypeVar> tvs_in_any_type;  // type variables in ANY of the definitions
    set<Hs::TypeVar> tvs_in_all_types;  // type variables in ALL of the definitions
    {
        // FIXME - should we be looping over binder vars, or over definitions?
        optional<set<Hs::TypeVar>> tvs_in_all_types_;
        for(auto& [_, type]: binder_env)
        {
            auto tvs = free_type_variables(type);
            add(tvs_in_any_type, tvs);
            if (tvs_in_all_types_)
                tvs_in_all_types_ = intersection(*tvs_in_all_types_, tvs);
            else
                tvs_in_all_types_ = tvs;
        }
        assert(tvs_in_all_types_);
        tvs_in_all_types = *tvs_in_all_types_;
    }

    // OK, we've got to do defaulting before we consider what variables to quantify over.

    vector< Hs::Var > dict_vars;

    // A. First, REDUCE the lie by
    //    (i)  converting to Hnf
    //     -- when do we do this?  Always?
    //    (ii) representing some constraints in terms of others.
    // This also substitutes into the current LIE, which we need to do 
    //    before finding free type vars in the LIE below.
    Hs::Binds binds = reduce_current_lie();

    // B. Second, extract the "retained" predicates can be added without causing abiguity.
    auto [lie_deferred, lie_retained] = classify_constraints( current_lie(), fixed_tvs );

    /* NOTE: Constraints can reference variables that are in
     *        (i) ALL types in a recursive group
     *       (ii) SOME-BUT-NOT-ALL types
     *      (iii) NO types.
     *
     * For unrestricted bindings, classes (ii) and (iii) need defaults.
     * For restricted bindings, only class (iii) (I think) needs defaults.
     */


    // FIXME: return {dvar = expression} as a mapping, instead of a sequence of binds?
    // If we want to substitute an expression for an argument in the wrapper,
    
    // For the COMPLETELY ambiguous constraints, we should be able to just discard the constraints,
    //   after generating definitions of their 
    auto [s1, binds1, lie_not_completely_ambiguous] = default_preds( fixed_tvs, tvs_in_any_type, lie_retained );
    binds = binds1 + binds;

    set<Hs::TypeVar> qtvs = tvs_in_any_type - fixed_tvs;

    if (is_restricted(signatures, decls))
    {
        // 1. Remove defaulted constraints from LIE?
        current_lie() = lie_deferred + lie_not_completely_ambiguous;

        // 2. Quantify only over variables that are "unconstrained" (not part of the LIE)
        // -- after defaulting!

        // NOTE: in theory, we should be able to subtract just ftvs(lie_retained_not_defaulted),
        //       since lie_deferred should contain only fixed type variables that have already been
        //       removed from qtvs.
        qtvs = qtvs - free_type_variables(current_lie());

        // 3. We have already substituted for types above.
        binder_env = quantify( qtvs, binder_env );
    }
    else
    {
        // For the SOMEWHAT ambiguous constraints, we don't need the defaults to define the recursive group,
        // but we do need the defaults to define individual symbols.

        // 1. Quantify over variables in ANY type that are not fixed -- doesn't depend on defaulting.
        // Never quantify over variables that are only in a LIE -- those must be defaulted.

        // 2. Only the constraints with all fixed tvs are going to be visible outside this declaration group.
        current_lie() = lie_deferred;

        dict_vars = vars_from_lie( lie_not_completely_ambiguous );

        global_value_env binder_env2;
        for(auto& [name,type]: binder_env)
        {
            auto tvs_in_this_type = free_type_variables(type);

            // Default any constraints that do not occur in THIS type.
            auto [s2, binds2, lie_for_this_type] = default_preds( fixed_tvs, tvs_in_this_type, lie_not_completely_ambiguous );

            Hs::Type constrained_type = add_constraints( constraints_from_lie(lie_for_this_type), type );

            // Only quantify over type variables that occur in THIS type.
            Hs::Type qualified_type = quantify( tvs_in_this_type, constrained_type );

            binder_env2 = binder_env2.insert( {name, qualified_type} );
        }
        binder_env = binder_env2;
    }

    Hs::Decls decls2 = decls;
    if (qtvs.size() or binds.size() or dict_vars.size())
    {
        decls2 = {};
        decls2.push_back( Hs::GenBind( qtvs | ranges::to<vector>, dict_vars, binds, decls ) );
    }

    pop_and_add_lie();

    return {decls2, binder_env};
}

// Figure 24. Rules for patterns
tuple<Hs::Pattern, Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        auto V = *v;
        auto& name = unloc(V.name);
        local_value_env lve;
        Hs::Type type;
        if (sigs.count(name))
        {
            auto sig_type = sigs.at(name);
            auto [tvs, constraints, monotype] = instantiate(sig_type);
            if (constraints.size())
                throw myexception()<<"variable '"<<name<<"' cannot have constrained type '"<<sig_type<<"' due to monomorphism restriction";
            type = monotype;
        }
        else
        {
            type = fresh_meta_type_var();
        }
        V.type = type;
        lve = lve.insert({name, type});
	return { V, type , lve };
    }
    // CONSTR-PAT
    else if (auto con = pat.head().to<Hs::Con>())
    {
        local_value_env lve;
        local_instance_env lie;
        vector<Hs::Type> types;
        auto pats = pat.copy_sub();

        for(auto& pat: pats)
        {
            auto [p, t1, lve1] = infer_pattern_type(pat, sigs);
            pat = p;
            types.push_back(t1);
            lve += lve1;
        }
        substitution_t s;
        auto [type,field_types] = constr_types(*con);

        assert(field_types.size() == pat.size());

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
            unify(types[i], field_types[i]);

        return { pat, type, lve };
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto [pat, t, lve] = infer_pattern_type(ap->pattern, sigs);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {pat, t, lve};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(lp->pattern, sigs);
        return {Hs::LazyPattern(p), t, lve};
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(sp->pattern, sigs);
        return {Hs::StrictPattern(p), t, lve};
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_meta_type_var();
        return {pat, tv, {}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        local_value_env lve;
        Hs::Type t = fresh_meta_type_var();
        for(auto& element: L.elements)
        {
            auto [p1, t1, lve1] = infer_pattern_type(element, sigs);
            element = p1;

            unify(t, t1);
            lve += lve1;
        }

        return {L, Hs::ListType(t), lve};
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        auto T = *t;
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: T.elements)
        {
            auto [p, t1, lve1] = infer_pattern_type(element, sigs);
            element = p;
            types.push_back(t1);
            lve += lve1;
        }
        return {T, Hs::TupleType(types), lve};
    }
    // ???
    else if (pat.is_int())
    {
        auto [dvar, type] = fresh_num_type();
        return {pat, type, {}};
    }
    else if (pat.is_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        return {pat, type, {}};
    }
    else if (pat.is_char())
    {
        return {pat, char_type(), {}};
    }
    else if (false) // Literal string
    {
        return {pat, Hs::ListType(char_type()), {}};
    }
    else if (pat.is_log_double())
        throw myexception()<<"log_double literatal should be impossible: '"<<pat<<"'!";
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}


// Figure 22. Rules for quals
//
// The implementation is rather... different?
// * the original figure doesn't have let quals.
// * the original figure seems to assume that quals only occur in list comprehensions?

tuple<vector<Hs::Qual>, value_env>
typechecker_state::infer_quals_type(const global_value_env& env, vector<Hs::Qual> quals)
{
    auto env2 = env;
    local_value_env binders;
    for(auto& qual: quals)
    {
        auto [qual_exp, qual_binders] = infer_qual_type(env2, qual);

        qual = qual_exp;
        env2 = plus_prefer_right(env2, qual_binders);
        binders = plus_prefer_right(binders, qual_binders);
    }
    return {quals, binders};
}

tuple<Hs::Qual, value_env>
typechecker_state::infer_qual_type(const global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = exp;
        unify( cond_type, bool_type() );
        return {SQ, {}};
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;

        // type(pat) = type(exp)
        unify(Hs::ListType(pat_type), exp_type);

        return {PQ, lve};
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [binds, t] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {LQ, t};
    }
    else
        std::abort();
}


tuple<Hs::Qual, value_env>
typechecker_state::infer_guard_type(const global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [cond_exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = cond_exp;
        unify( cond_type, bool_type() );
        return {SQ, {}};
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;
        
        // type(pat) = type(exp)
        unify(pat_type,exp_type);

        return {PQ, lve};
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [binds, t] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {LQ, t};
    }
    else
        std::abort();
}


// Figure 25. Rules for match, mrule, and grhs
tuple<Hs::GuardedRHS, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::GuardedRHS rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty())
    {
        auto [body, type] = infer_type(env, rhs.body);
        rhs.body = body;
        return {rhs, type};
    }

    // Fig 25. GUARD
    auto guard = rhs.guards[0];
    auto [guard1, env1] = infer_guard_type(env, guard);
    auto env2 = plus_prefer_right(env, env1);

    rhs.guards.erase(rhs.guards.begin());
    auto [rhs2, t2] = infer_type(env2, rhs);
    
    rhs2.guards.insert(rhs2.guards.begin(), guard1);

    Hs::Type type = t2;
    return {rhs2, type};
}

// Fig 25. GUARD-OR
tuple<Hs::MultiGuardedRHS, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MultiGuardedRHS rhs)
{
    substitution_t s;
    Hs::Type type = fresh_meta_type_var();

    auto env2 = env;
    if (rhs.decls)
    {
        auto [decls1, binders] = infer_type_for_binds(env, unloc(*rhs.decls));
        unloc(*rhs.decls) = decls1;
        env2 = plus_prefer_right(env, binders);
    }

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto [guarded_rhs2, t1] = infer_type(env2, guarded_rhs);
        guarded_rhs = guarded_rhs2;
        unify(t1,type);
    }

    return {rhs, type};
};

tuple<Hs::MRule, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MRule rule)
{

    if (rule.patterns.empty())
    {
        auto [rhs, type] = infer_type(env, rule.rhs);
        rule.rhs = rhs;
        return {rule, type};
    }
    else
    {
        auto [pat, t1, lve1] = infer_pattern_type(rule.patterns.front());
        auto env2 = plus_prefer_right(env, lve1);

        // Remove the first pattern in the rule
        rule.patterns.erase(rule.patterns.begin());

        auto [rule2, t2] = infer_type(env2, rule);

        rule2.patterns.insert(rule2.patterns.begin(), pat);

        Hs::Type type = make_arrow_type(t1,t2);

        return {rule2, type};
    }
}

tuple<Hs::Match, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::Match m)
{
    Hs::Type result_type = fresh_meta_type_var();

    for(auto& rule: m.rules)
    {
        auto [rule1, t1] = infer_type(env, rule);
        rule = rule1;
        unify(result_type, t1);
    }

    return {m, result_type};
}



tuple<expression_ref, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, expression_ref E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto& x_name = unloc(x->name);
        auto sigma = env.find( x_name );

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x->print()<<"'";

        auto [_, constraints, type] = instantiate(*sigma);

        for(auto& constraint: constraints)
        {
            auto dvar = add_dvar(constraint);
            E = {E, dvar};
        }

        return {E, type};
    }
    else if (E.is_int())
    {
        auto [dvar, type] = fresh_num_type();
        E = { find_prelude_var("fromInteger"), dvar, E };
        return { E, type };
    }
    else if (E.is_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        E = { find_prelude_var("fromRational"), dvar, E };
        return { E, type };
    }
    else if (E.is_char())
    {
        return { E, char_type() };
    }
    else if (E.is_log_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        return { E, type };
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        // So, ( e :: tau ) should be equivalent to ( let x :: tau ; x = e in x )
        // according to the 2010 report.

        // Example: (\x -> x) :: Num a => a -> a
        // In this example, we should rewrite this to \dNum -> \x -> x

        // texp->exp;
        // texp->type

        // FIXME: We should actually reuse the code from infer_type_for_single_fundecl_with_sig

        auto x = fresh_var("tmp", false);
        Hs::Decls decls;
        decls.push_back(simple_decl(x,texp->exp));
        Hs::Binds binds;
        binds.signatures.insert({unloc(x.name), texp->type});
        binds.push_back(decls);
        expression_ref E2 = Hs::LetExp({noloc,binds},{noloc,x});

        return infer_type(env, E2);

        /* 

        // 1. instantiate the type -> (tvs, givens, rho-type)
        auto [tvs, given_constraints, given_type] = instantiate(texp->type, false);
        auto ordered_lie_given = constraints_to_lie(given_constraints);
        auto lie_given = unordered_lie(ordered_lie_given);

        // 2. Get the most general type
        push_lie();
        auto [exp, most_general_type] = infer_type(env, texp->exp);
        auto lie_wanted = pop_lie();

        // 3. alpha[i] in most_general_type but not in env
        auto ftv_mgt = free_type_variables(most_general_type) - free_type_variables(env);
        // FIXME -- what if the instantiated type contains variables that are free in the environment?
        
        // 4. Constrain the most general type to the given type with MATCH
        auto e = myexception()<<"Type '"<<texp->type<<"' does not match type '"<<most_general_type<<"' of expression '"<<texp->exp<<"'";
        unify(most_general_type, given_type, e);

        // 5. check if the given => wanted ~ EvBinds
        lie_wanted = apply_current_subst(lie_wanted);
        auto evbinds = entails(lie_given, lie_wanted);
        if (not evbinds)
            throw myexception()<<"Can't derive constraints '"<<print(lie_wanted)<<"' from specified constraints '"<<print(lie_given)<<"'";

        // 6. create lambda arguments from the list of constraint vars, in order
        vector<expression_ref> dvars;
        for(auto& [dvar, constraint]: ordered_lie_given)
            dvars.push_back(dvar);

        current_lie() += lie_given;

        return { Hs::LambdaExp(dvars, Hs::LetExp({noloc,*evbinds}, {noloc,exp})), given_type };
        */
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_meta_type_var();
        auto L = *l;
        for(auto& element: L.elements)
        {
            auto [element1, t1] = infer_type(env, element);
            element = element1;
            unify(t1, element_type);
        }
        return { L, Hs::ListType(element_type) };
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [element1, element_type] = infer_type(env, element);
            element = element1;
            element_types.push_back( element_type );
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        return {T, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];

        auto [f, t1] = infer_type(env,e1);

        vector<expression_ref> args;
        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_meta_type_var();

            auto [arg2, t2] = infer_type(env, e2);
            args.push_back(arg2);

            unify (t1, make_arrow_type(t2,tv));

            t1 = tv;
        }
        E = apply_expression(f, args);

        return {E, t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto rule = Hs::MRule{Lam.args, Lam.body};
        auto [rule2, t] = infer_type(env, rule);
        Lam.args = rule.patterns;
        Lam.body = rule.rhs;
        return {Lam, t};
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;

        // 1. Extend environment with types for decls, get any substitutions
        auto [binds, binders] = infer_type_for_binds(env, unloc(Let.binds));
        unloc(Let.binds) = binds;
        auto env2 = plus_prefer_right(env, binders);

        // 2. Compute type of let body
        auto [body, t_body] = infer_type(env2, unloc(Let.body));
        unloc(Let.body) = body;

        // return (s1 `compose` s2, t2)
        return {Let, t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        auto [type, field_types] = constr_types(*con);

        vector<Hs::Type> arg_types;
        vector<Hs::Exp> args = E.copy_sub();
        for(int i=0; i < args.size(); i++)
        {
            auto& arg = args[i];
            auto [arg_i, t_i] = infer_type(env, arg);
            arg = arg_i;
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            unify( field_types[i], t_i);
        }
        E = expression_ref(*con, args);
        
        return { E, type };
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;

        // 1. Determine object type
        auto [object, object_type] = infer_type(env, Case.object);
        Case.object = object;
        
        // 2. Determine data type for object from patterns.
        Hs::Match match;
        for(auto& alt: Case.alts)
        {
            auto& [pattern, body] = unloc(alt);
            match.rules.push_back(Hs::MRule{{pattern},body});
        }

        auto [match2, match_type] = infer_type(env, match);

        for(int i=0;i<Case.alts.size();i++)
        {
            unloc(Case.alts[i]) = {match2.rules[i].patterns[0], match2.rules[i].rhs};
        }

        Hs::Type result_type = fresh_meta_type_var();

        unify( make_arrow_type(object_type,result_type), match_type );

        return { Case, result_type };
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto If = *if_exp;
        auto [cond, cond_type ] = infer_type(env, unloc(If.condition));
        auto [tbranch, tbranch_type] = infer_type(env, unloc(If.true_branch));
        auto [fbranch, fbranch_type] = infer_type(env, unloc(If.false_branch));
        unloc(If.condition) = If;
        unloc(If.true_branch) = tbranch;
        unloc(If.false_branch) = fbranch;

        unify(cond_type, bool_type());
        unify(tbranch_type, fbranch_type);

        return {If, tbranch_type};
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto [quals, quals_binders] = infer_quals_type(env, LComp.quals);
        auto [body, exp_type] = infer_type(plus_prefer_right(env, quals_binders), LComp.body);
        LComp.quals = quals;
        LComp.body = body;

        Hs::Type result_type = Hs::ListType(exp_type);

        return { LComp, result_type };
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();

        // PROBLEM: Do we need to desugar these here, in order to plug in the dictionary?
        auto [from, t_from] = infer_type(env, L.from);
        L.from = from;
        unify(t,t_from);

        return {L, Hs::ListType(t) };
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, L.from);
        unify(t,t_from);

        auto [then, t_then] = infer_type(env, L.then);
        L.from = from;
        L.then = then;
        
        return {L, Hs::ListType(t)};
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, l->from);
        unify(t,t_from);

        auto [to, t_to] = infer_type(env, l->to);
        L.from = from;
        L.to = to;
        
        return {L, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, L.from);
        unify(t,t_from);

        auto [then, t_then] = infer_type(env, L.then);
        unify(t,t_then);

        auto [to, t_to] = infer_type(env, l->to);
        unify(t,t_to);

        L.from = from;
        L.then = then;
        L.to = to;

        return {L, Hs::ListType(t)};
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

        gve += gve1;
        gie += gie1;
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
    if (class_decl.binds)
    {
        for(auto& [name, type]: unloc(*class_decl.binds).signatures)
        {
            Hs::Type method_type = type_check_class_method_type(K, type, constraint);

            method_type = apply_current_subst(method_type);
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
        string cname1 = get_class_name_from_constraint(constraint_);
        string cname2 = get_class_name_from_constraint(constraint);
        auto get_dict = fresh_var(cname1+"From"+cname2, true);
        // Should this be a function arrow?
        Hs::Type type = add_constraints({constraint}, constraint_);
        // Could we be adding too many forall vars?
        type = apply_current_subst(type);
        type = add_forall_vars(class_typevars, type);
        gie = gie.insert({unloc(get_dict.name), type});
    }
    cinfo.fields = gve + gie;

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

global_instance_env
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl, const class_env& ce)
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
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type constructor!";

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

    auto dfun = fresh_var("dfun", true);
    Hs::Type inst_type = add_constraints(inst_decl.context.constraints, inst_decl.constraint);
    inst_type = apply_current_subst(inst_type);
    inst_type = add_forall_vars( free_type_VARS(inst_type) | ranges::to<vector>, inst_type);
    global_instance_env gie_out;
    gie_out = gie_out.insert( { unloc(dfun.name), inst_type } );
    return gie_out;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
global_instance_env
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce)
{
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto gie1 = infer_type_for_instance1(*I, ce);

            gie_inst += gie1;
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
typechecker_state::infer_type_for_instance2(const Hs::InstanceDecl& inst_decl, const class_env& ce)
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
    auto binds_super = entails(lie, lie_super);
    if (not binds_super)
        throw myexception()<<"Missing instances!";

    // Premise 8:
    Hs::Binds binds_methods;
    auto dfun = fresh_var("dfun", true);

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdicts,methods>
    expression_ref dict1 = tuple_from_value_env(cinfo->fields);
    expression_ref dict2 = tuple_from_value_env(lie);

    expression_ref E = Hs::LetExp( {noloc,binds_methods}, {noloc, dict1} );
    E = Hs::LetExp( {noloc,*binds_super}, {noloc,E} );
    E = Hs::LambdaExp({dict2}, E);

    Hs::Decls decls ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto decls_ = infer_type_for_instance2(*I, ce);

            for(auto& d: decls_)
                out_decls.push_back(d);
        }
    }
    return out_decls;
}

Hs::ModuleDecls typecheck( const string& mod_name, const Module& m, Hs::ModuleDecls M )
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
    typechecker_state state( mod_name, m, M, constr_info );
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

    state.gie = class_gie;
    auto inst_gie = state.infer_type_for_instances1(M.type_decls, class_info);

    state.gie += inst_gie;

    for(auto& [method,type]: inst_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    // 3. E' = (TCE_T, (CVE_T, GVE_C, LVE={}), CE_C, (GIE_C, LIE={}))

    auto [value_decls, env] = state.infer_type_for_binds(gve, M.value_decls);
    M.value_decls = value_decls;

    auto simpl_binds = state.reduce_current_lie();

    ranges::insert(simpl_binds, simpl_binds.end(), M.value_decls);
    M.value_decls = simpl_binds;

    auto default_binds = state.default_subst();
    env = state.apply_current_subst(env);
    ranges::insert(M.value_decls, M.value_decls.begin(), default_binds);

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<alphabetize_type(t)<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";

    std::cerr<<M.value_decls.print();
    std::cerr<<"\n\n";
    return M;
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


/*
 Examples #1:
(i,j) = (\x ->x, \y -> y)

i_mono :: a -> a
j_mono :: b -> b 

i = /\a. i' (@a) Any
j = /\b. j' Any (@b)


AbsBinds [p_a2MR, p_a2MW] []
  {Exports: [i <= i_a2ML
               wrap: /\(@ p_a2N4). <> @ p_a2N4 @ GHC.Types.Any,
             j <= j_a2MN
               wrap: /\(@ p_a2Nd). <> @ GHC.Types.Any @ p_a2Nd]
   Exported types: i :: forall p. p -> p
                   [LclId]
                   j :: forall p. p -> p
                   [LclId]
   Binds: (i_a2ML, j_a2MN) = (\ x_a1Zd -> x_a1Zd, \ y_a1Ze -> y_a1Ze)
   Evidence: [EvBinds{}]}

 Example #2:

i :: Int -> Int
(i,j) = (\x ->x, \y -> y)

i = case tup Any of (i,j) -> i
j = /\a.case tup a of (i,j) -> j

AbsBinds [p_a2MV] []
  {Exports: [i <= i_a2MK
               wrap: <> @ GHC.Types.Any,
             j <= j_a2MR
               wrap: <>]
   Exported types: i :: Int -> Int
                   [LclId]
                   j :: forall p. p -> p
                   [LclId]
   Binds: (i_a2MK, j_a2MR) = (\ x_a1Z9 -> x_a1Z9, \ y_a1Za -> y_a1Za)
   Evidence: [EvBinds{}]}

 */
