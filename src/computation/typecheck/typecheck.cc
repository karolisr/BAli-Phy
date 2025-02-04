#include "typecheck.H"
#include "kindcheck.H"

#include <set>

#include <range/v3/all.hpp>

#include "haskell/haskell.H"

#include "immer/map.hpp" // for immer::map

#include "util/set.H"
#include "util/string/join.H"

#include "util/graph.H" // for get_ordered_strong_components( )

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H" // for is_tuple_name( )
#include "computation/operation.H" // for is_non_apply_op( )
#include "haskell/ids.H"

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
  * Add a substitution to the TypeChecker, instead of returning substitutions from every call.
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
  * Kind-check explicit type signatures.
  * Change the type of class methods to forall a.C a => (forall b. ctxt => body)
  * Typecheck default instance methods in class declarations.
    + Out of line, take a (C a) argument.
    + Typecheck as FunDecl with signature forall a. C a => method_type
  * Typecheck instance methods instance declarations.
    + Out of line, take instance constraint dictionaries as arguments.
    + Substitute into class variables to get method type.
    + Typecheck as FunDecl with signature forall tvs.(instance constraints) => method_type
  * Handle different defaults for each entry of a GenBind by recording impedance matching info on BindInfo
  * Mark instances, default methods, and superclass extractors as exported.
  * Export ALL typechecker state between modules.
  * Make a version of the string-to-[Char] routine that makes sense type-wise.
  * Always generalize at top level.
  * Rewrite builtin decls to look like `foreign import mycall "lib:fname" name :: type`
  * Typecheck do-expression, left-sections, and right sections.
  * Typecheck syntax operations directly: fromInteger, fromRational, (>>=), (>>), enumFrom, etc...
  * Record impedance-matching info in GenBind, but only for passing dictionaries.
  * Implement literals, including literal strings.
  * Replace let-bound vars w/o signatures with local monomorphic ids.
  * Handle type synonyms.
  * assert in try_insert that we are inserting a tau type with no foralls.
  * Make a different struct type for MetaTypeVar.
  * Make a constructor expression that must be fully applied
  * add tcRho( ).
  * skolemize in single_fundecl_with_sig
  * make a version of instantiation that invents and applied dictionaries by creating a wrapper.
  * make wrappers operate on Core
  * separate Core::Apply, Core::Let, Core::Lambda, Core::Tuple
  * add a wrapper to checkSigma( ).
  * record a result wrapper on ApplyExp
  * Don't desugar Core::Decls, just the Hs::Binds.  But pass them both to the optimizer.
  * Implement type classes Num, Enum, and Ix.
  * Make MetaTypeVar's point directly to their target.

  0. Speed up: add type variable levels.
    - where do we increment the level?
      + let-bindings
      + lambda-bindings?
  0. Speed up: don't import EVERYTHING
    - Also put the stuff from other modules in a different environment that we know has no meta-type-variables.
  0. Speed up: make types into a ref-counted type?
  0. Add Functor instance for Ix i => Array i.
     - We need to make another type (Array# e) that is indexed by Int.
     - We can then declare Array i e = Array i i (Array# e)
     - The Array then contains its own bounds.
     - The downside is that we need to subtract 0 from every index before we look it up.
  0. merge rename_pattern_from_bindinfo into tcPat( ) -> pass in (LetCtxt signatures (\id->monoid))
  0. remove desugar_pattern( )
  0. Make a quals class.
  0. Make a Pattern class.
     - we would have to re-implement infix expression handling -> infix pattern handling
     - does GHC do that?
  0. Make environments not just take a string ... make them take a Var/Con or a (String,Int) pair?
  0. Make an Expression class, similar to the Type class, that allows the object to be modified.
     - It can hold things in a cow_ptr
     - We probably need to also make a Pattern class
     - We probably need to make an ExpressionOrPattern class that we parse to, and then fix up.
  0. Cleanup: convert ListType, TupleType, to applied TypeCon earlier.
     - We need to alter TypeApp printing, then!
     - How about StrictLazyType?  Is this part of the type, or part of the declaration?
  0. Cleanup: eliminate dependencies on expression_ref:
     - Make Pattern into a Type that doesn't depend on expression_ref.
       - Make a LitPattern that compares Int, Double, String, Char by equality.
  0. Cleanup: move generalization code out of binds.cc to generalize.cc?
  0. Should/Can we get default_method_decls and superclass extractor decls from a class decl?
  0. Exporting and importing types and instances between modules:
     - Should we have separate maps for variables and constructors?
       - This includes classes, methods, data types, constructors, type synonyms, etc.
     - The small_decls won't have symbols though...  maybe we need annotated type info for that?
     - How do you export instances?
  0. Update importing/exporting of constructors and class methods -- in rename???
  0. Typecheck Rec, MDo, etc.
    * Rec is currently desugared in rename.
    * MDo is ignored.
  1. Reject unification of variables, tycons, etc with different kinds.
     - Ensure that all ForallType binders have kinds.
     - Assign kinds to all TypeCons.... OR look it up in the symbol table when we need to!
  2. Process type signatures for ambiguity (?)
  3. Efficient handling of recursion between instance method and dictionaries.
       + For example, if we have i==@1 = \(x:xs) (y:ys) -> (x == y) && (xs == ys) then
         it would be nice to make the call for (xs == ys) call i==@1 directly, instead of
         allocating an Eq [a] dictionary and extracting its (==) element.
       + We should end up with something like (==) (dEqList dEqa) xs ys where
         dEqList dEqa = (i==@1 dEqa, i=\@2 dEq a).
       + If we could inline BOTH (==) and dEqList, then I think we'd get the result that we want.
       + Maybe we could inform the inliner that i==@1 is an method selector, and dEqList is a dfun?
       + And we want to do this regardless of how many times the method selector and the dfun appear?
       + GHC does something else...  see DFunUnfolding?
  4. Avoid a space leak with polymorphic recursion in cases like factorial.
    + When we generate a polymorphic call, sometimes the call is at the same type as the monomorphic call.
    + In those cases, we want to replace the polymorphic call with a monomorphic call... how?
  5. Record impedance-matching info for types on GenBind
     - Need to pass type/dict arguments in correct order for (forall a. C1 a => forall b. C2 b => a -> b -> a)
     - Instantiated type would otherwise be treated as (forall a b. (C1 a, C2 b) => a -> b -> a)
     - For defaulted defaulted types and defaulted dictionaries.
  0. Punt all defaulting to top level?
  6. Check that constraints in instance contexts satisfy the "paterson conditions"
  7. Handle export subspecs Datatype(constructor...) and Class(method...)
  8. AST nodes for type and evidence bindings?
  9. Factor generalization code out of typecheck/binds.cc
  10. Check that there are no cycles in the class hierarchy.
  11. Allow dictionaries as constructor entries.
     - How do we treat such entries as givens when we handle case alternatives?
     - Handle constraints on constructors and data types.
     - Remove the constraint from EmptySet
        - I guess we remove type-level constraints if they would be ambiguous.
        - I guess we COMPLAIN about user-specified constructor-level constraints if they would be ambiguous.
  0. Use UniqueString = {string,optional<int>} ids after rename.
    * Ideally, allow comparing just the integers.
    * If the integers are the same, the strings had better be the same too!
    * Should we record original names during rename?  What does GHC do?
    * Currently, I guess the ids must be globally unique within an entire program.
    * Later, if we allow separate compilation, we could allow ids to be unique within a module.
  11. Annotate let, lambda, and case binders with variable's type.
  12. Should we make AST for: 
     - \(dicts::theta) -> monobinds in binds    -> This is GenBinds
     - exp <dicts>
     - (superdicts, methods)
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?
  18. Add basic error reporting.
  20. Handle literal constant patterns.  
      - we need to typecheck something like (==) (fromInteger x :: a) (case_object :: b)
      - then we would report the type b
      - this requires an Eq dictionary for the (==) and a Num dictionary (eventually) for the integer
      - we don't create an actual case_object expression, just unify the type of (==) with (a -> b -> c)
        and extract the b.
      - should we use an operator == for strings as well?
      - I guess (case x of 1 -> y; _ -> rest) changes to (case (x == 1) of True -> y; _ -> rest)
        + this is NOT a very efficient switch statement!
      - what if I add a switch x of {1 -> x, 2 -> y, etc.} operation for cases where x :: Int#?
      - we know that you can do (<) comparison on Int#...

  Optimization:
  - If we have ((case x of alts) y z), should we sink the arguments?
    This could expose a case-of-case, for example.

  - If we have ((case x of alts) y z), can we float out the case?
    It seems that we are not doing so...

  - See note [ClassOp/DFun selection] in Tc/TyCl/Instance.hs
    + We often see op2 (df d1 d2)
    + dfuns (df) and class ops (op2) NEVER inline
    + always define dfuns in stylized form: df d1 d2 = MkD ($cop d1 d2) ($cop2 d1 d2) ...
    + df inlines to the magical form DFunUnfolding [$cop1, $cop2, ...]
    + Use GHC.Core.Unfold.exprIsConApp_maybe inlines DFunUnfoldings? ... how?
    + Each class op has a BuiltinRule that extracts the right piece whenever the argument is a ConApp..
    + Make 'df' "CONLIKE" so that shared uses still match:
      let d = df d1 d2
      in ...(op2 d) ... (op1 d) ...

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
  1. Implement kinds as Type

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
  A3: We should do it during translation to "Core".

  Q4: How do we process the instances in the correct order?
  A4: Can we put them into the type groups, and then REVISIT the groups,
      analyzing the instances last in each group?

  Q5. How do we type-check the method bindings, if their types depend on the VALUE bindings?
  A5. We know the types of the instance methods BEFORE we know the function bodies, right?
      So, we should be able to type-check the instance bodies LAST, if we generate the types
      for the value_decls first.
  A5. First record the method types, then type-check values -- including values for the method bindings!
      Then create values for the instances.

  Q6. How do we check that there are no super-class -> class cycles.
  A5. Just record all classes that occur in the constraints of each class as a directed edge, and look for cycles.

  Q7. Should we generate a special syntax node for dictionary arguments, dictionary applications, etc,
      in order to delay converting => to ->, and converting dictionary applications, etc?
      See Section 4.3:
      - exp <dict>                                       Dictionary application
      - \dicts:theta -> exp                              Dictionary abstraction
      - \\a1..an -> \dicts:theta -> binds in monobinds   Dictionary abstraction (generalized)
      - <dicts,methods>                                  Dictionary record data type
      - \<dicts:theta,methods:GVE> -> exp                Selecting entries from a dictionary
 */

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

const ClassInfo* TypeChecker::info_for_class(const std::string& name) const
{
    auto T = this_mod().lookup_resolved_type(name);
    assert(T);

    if (auto C = T->is_class())
        return C->info.get();
    else
        return nullptr;
}

const TypeSynonymInfo* TypeChecker::info_for_type_synonym(const std::string& name) const
{
    auto T = this_mod().lookup_resolved_type(name);
    assert(T);
    if (auto S = T->is_type_syn())
        return S->info.get();
    else
        return nullptr;
}

const TypeFamInfo* TypeChecker::info_for_type_fam(const std::string& name) const
{
    auto T = this_mod().lookup_resolved_type(name);
    assert(T);
    if (auto F = T->is_type_fam())
        return F->info.get();
    else
        return nullptr;
}

int TypeChecker::type_con_arity(const TypeCon& tc) const
{
    auto T = this_mod().lookup_resolved_type(unloc(tc.name));
    if (not T)
        throw note_exception()<<"Can't find type con '"<<tc<<"'";
    assert(T->arity);
    return (*T->arity);
}

bool TypeChecker::type_con_is_type_fam(const TypeCon& tc) const
{
    return info_for_type_fam(unloc(tc.name));
}

bool TypeChecker::type_con_is_type_syn(const TypeCon& tc) const
{
    return info_for_type_synonym(unloc(tc.name));
}

bool TypeChecker::type_con_is_type_class(const TypeCon& tc) const
{
    return info_for_class(unloc(tc.name));
}

bool TypeChecker::type_con_must_be_saturated(const TypeCon& tc) const
{
    return type_con_is_type_fam(tc) or type_con_is_type_syn(tc);
}

std::optional<std::tuple<Type, Type>> TypeChecker::is_type_app(Type t) const
{
    // 1. Follow meta-type-vars
    t = follow_meta_type_var(t);

    // 2. If there's no TypeApp, we are done.
    auto app = t.to<TypeApp>();
    if (not app) return {};

    // 3. Get the head and arg types
    auto fun = app->head;
    auto arg = app->arg;

    // 4. Find the head and the number of args it has
    t = follow_meta_type_var(fun);
    int n_args = 0;
    while(auto app2 = t.to<TypeApp>())
    {
        t = follow_meta_type_var(app2->head);
        n_args++;
    }

    // 5. Avoid eating an argument from a saturated typecon that must remain saturated
    if (auto tc = t.to<TypeCon>())
    {
        if (type_con_must_be_saturated(*tc) and n_args < type_con_arity(*tc))
            return {};
    }
    
    // 6. Return the head and arg types
    return {{fun, arg}};
}

std::optional<std::tuple<TypeCon,std::vector<Type>>> TypeChecker::is_type_fam_app(const Type& t) const
{
    auto tcapp = is_type_con_app(t);
    if (not tcapp) return {};
    auto& [tc,args] = *tcapp;

    if (type_con_is_type_fam(tc) and args.size() == type_con_arity(tc))
        return tcapp;
    else
        return {};
}

std::optional<std::tuple<TypeCon,std::vector<Type>>> TypeChecker::is_type_class_app(const Type& t) const
{
    auto tcapp = is_type_con_app(t);
    if (not tcapp) return {};
    auto& [tc,args] = *tcapp;

    if (type_con_is_type_class(tc) and args.size() == type_con_arity(tc))
        return tcapp;
    else
        return {};
}

TypeVar unification_env::fresh_tyvar(const std::optional<Kind>& kind) const
{
    int level = 0;
    TypeVar ftv(level, {noloc,"utv"+std::to_string(next_index)});
    ftv.index = next_index++;
    ftv.kind = kind;
    return ftv;
}

global_tc_state::global_tc_state(Module& m)
    :this_mod(m)
{ }


std::optional<int> TypeChecker::unification_level() const
{
    return global_state->unification_level;
}

void  TypeChecker::set_unification_level(int l)
{
    if (not unification_level() or *unification_level() > l)
        global_state->unification_level = l;
}

void  TypeChecker::clear_unification_level()
{
    global_state->unification_level = {};
}

Type TypeChecker::expTypeToType(const Expected& E)
{
    if (auto I = E.infer())
        return inferResultToType(*I);
    else
        return E.check_type();
}

Type TypeChecker::inferResultToType(Infer& I)
{
    if (auto T = I.type())
    {
        ensure_monotype(*T);
        return *T;
    }
    else
    {
        // This can now only be a monotype
        auto tv = FreshVarSource::fresh_meta_type_var( I.level(), "m", {} ); // unknown kind!
        I.set_type(tv);
        return tv;
    }
}

Expected TypeChecker::newInfer()
{
    return Infer(level());
}

void TypeChecker::fillInfer(const Type& type, Infer& I)
{
    if (auto result_type = I.type())
    {
        if (level() != I.level()) ensure_monotype(*result_type);

        unify(type, *result_type);
    }
    else
    {
        auto promoted_type = promote_type(I.level(), type);

        I.set_type( promoted_type );
    }
}

void TypeChecker::ensure_monotype(const Type& type)
{
    if (not is_rho_type(type))
        throw note_exception()<<"ensure_monotype: "<<type<<" is not a rho type!";

    if (true) // a tau type
        ;
    else
    {
        auto mtv = fresh_meta_type_var("mono", {});  // unknown kind!
        unify(type, mtv);
    }
}

Type TypeChecker::promote_type(int dest_level, const Type& type)
{
    if (level() == dest_level)
        return type;
    else
    {
        Type promoted_type = FreshVarSource::fresh_meta_type_var(dest_level, "hole", {}); // unknown kind!

        // This wanted equality isn't "visible"?
        unify(promoted_type, type);

        return promoted_type;
    }
}

void TypeChecker::set_expected_type(const Expected& E, const Type& type)
{
    if (auto I = E.infer())
        fillInfer(type, *I);
    else
        unify(type, E.check_type());
}

void TypeChecker::get_tycon_info(const Hs::TypeFamilyDecl& F)
{
    auto con = desugar(F.con);
    auto kind = F.kind();
    if (kind_sigs().count(con))
    {
        kind = kind_sigs().at(con);

        // Complain if the declaration has kind annotation
        if (F.has_kind_notes())
        {
            push_note( Note() <<"In type family `"<<con.print()<<"`");
            push_source_span(*con.name.loc);
            record_error( Note() << "Kind annotations in declaration not allowed with a kind signature");
            pop_source_span();
            pop_note();
        }

        // Complain if kind signature allow to few arguments.
        if (num_args_for_kind(kind) < F.arity())
        {
            push_source_span(*con.name.loc);
            record_error( Note() << "Kind signature for type family `"<<con.print()<<"` only allows "<<num_args_for_kind(kind)<<", but declaration has "<<F.arity());
            kind = F.kind();
            pop_source_span();
        }
    }
    auto T = this_mod().lookup_local_type(unloc(F.con).name);
    assert(T);
    T->kind = kind;
    // We could probably do this earlier...
    T->arity = (int)F.args.size();
}

void TypeChecker::get_kind_sigs(const Hs::Decls& type_decls)
{
    for(auto& [_,type_decl]: type_decls)
    {
        if (auto SK = type_decl.to<Hs::KindSigDecl>())
        {
            for(auto& hs_tycon: SK->tycons)
            {
                auto tycon = desugar(hs_tycon);
                push_source_span(*tycon.name.loc);

                if (kind_sigs().count(tycon))
                    record_error( Note()<<"Second kind signature for `"<<unloc(tycon.name)<<"`" );
                else
                    kind_sigs().insert({tycon,SK->kind});

                pop_source_span();
            }
        }
    }
}

void TypeChecker::get_tycon_info(const Hs::Decls& type_decls)
{
    // First get standalone kind signatures
    get_kind_sigs(type_decls);

    TypeConEnv new_tycons;

    for(auto& [_,type_decl]: type_decls)
    {
        if (auto F = type_decl.to<Hs::TypeFamilyDecl>())
            get_tycon_info(*F);
        else if (auto C = type_decl.to<Hs::ClassDecl>())
        {
            for(auto& F: C->type_fam_decls)
                get_tycon_info(F);
        }
    }

    auto type_decl_groups = find_type_groups(type_decls);

    // Compute kinds for type/class constructors.
    for(auto& type_decl_group: type_decl_groups)
    {
        kindchecker_state K( this_mod() );

        auto new_tycons = K.infer_kinds(type_decl_group);

        for(auto& [tycon,ka]: new_tycons)
        {
            auto& [k,arity] = ka;
            auto T = this_mod().lookup_local_type(tycon);
            T->kind = k;
            T->arity = arity;
        }
    }

//    for(auto& [tycon,ka]: new_tycons)
//    {
//        auto& [k,arity] = ka;
//        std::cerr<<tycon<<" :: "<<k.print()<<"\n";
//    }
//    std::cerr<<"\n";
}

// The GIE does NOT allow free type variables.
struct instance_info
{
    // How do we get the kind into the type vars?
    vector<TypeVar> type_vars;
    Context context;
    ID class_name;
    std::vector<Type> argument_types;

    ID dfun_name;

    // forall <type_vars> . context => class_name argument_types[0] arguments[1] .. argument_types[n01]
    Type dfun_type() const
    {
        TypeCon class_con({noloc, class_name}); // whats the kind?
        return ForallType(type_vars, ConstrainedType(context, make_tyapps(class_con, argument_types)));
    }
};

LIE apply_subst(const substitution_t& s, const LIE& env1)
{
    LIE env2;
    for(auto constraint: env1)
    {
        constraint.pred = apply_subst(s, constraint.pred);
        env2.push_back(constraint);
    }
    return env2;
}

optional<ID> maybe_get_class_name_from_constraint(const Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);
    if (auto tc = tycon.to<TypeCon>())
        return get_unqualified_name(unloc(tc->name));
    else
        return {};
}

ID get_full_class_name_from_constraint(const Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);
    if (auto tc = tycon.to<TypeCon>())
        return unloc(tc->name);
    else
        throw myexception()<<"Can't get class name for constraint '"<<constraint<<"'";
}

ID get_class_name_from_constraint(const Type& constraint)
{
    if (auto name = maybe_get_class_name_from_constraint(constraint))
        return *name;
    else
        return "Constraint";
}

MetaTypeVar TypeChecker::fresh_meta_type_var(const std::string& name, const Kind& k)
{
    return FreshVarSource::fresh_meta_type_var(level(), name, k);
}

MetaTypeVar TypeChecker::fresh_meta_type_var(const Kind& k)
{
    return FreshVarSource::fresh_meta_type_var(level(), k);
}

TypeVar TypeChecker::fresh_rigid_type_var(const std::string& name, const Kind& k)
{
    return FreshVarSource::fresh_rigid_type_var(level(), name, k);
}

TypeVar TypeChecker::fresh_rigid_type_var(const Kind& k)
{
    return FreshVarSource::fresh_rigid_type_var(level(), k);
}

// Wait, actually don't we assume that the value decls are divided into self-referencing binding groups, along with explicit signatures?
// We would also need: infix declarations, default declarations, ???
// I guess this is AFTER rename, so declarations have been un-infixed, and we could (theoretically) represent each function as something like [([pat],grhs)]
// SHOULD we actually translate each function to (say) a list of ([pat],ghrs)?  How do we store 
//
// typecheck_module(vector<ClassDecl>, vector<DataDecl>, vector<TypeSynonymDecl>, vector<InstanceDecl>, vector<ValueDecl>)
// {
//    Kindcheck(classdecls, data_decls, type_decls);
//
//
// }

const TypeSynonymInfo* TypeChecker::maybe_find_type_synonym(const Type& type) const
{
    if (auto tycon = type.to<TypeCon>())
        return info_for_type_synonym(unloc(tycon->name));
    else
        return nullptr;
}

std::optional<Type> TypeChecker::expand_type_synonym(const Type& type) const
{
    if (auto t2 = filled_meta_type_var(type))
        return expand_type_synonym(*t2);

    if (type.is_a<TypeCon>() or type.is_a<TypeApp>())
    {
        auto [head, args] = decompose_type_apps(type);

        if (auto tsyn = maybe_find_type_synonym(head))
            return tsyn->expand(args);
    }

    return {};
}

Type TypeChecker::expand_all_type_synonyms(Type type) const
{
    while(auto type2 = expand_type_synonym(type))
        type = *type2;

    if (auto t2 = filled_meta_type_var(type))
        return expand_all_type_synonyms(*t2);

    if (auto tapp = type.to<TypeApp>())
    {
        auto head = expand_type_synonym(tapp->head);
        auto arg  = expand_type_synonym(tapp->arg);

        if (head and arg)
            type = TypeApp(*head, *arg);
        else if (head)
            type = TypeApp(*head, tapp->arg);
        else if (arg)
            type = TypeApp(tapp->head, *arg);
    }

    return type;
}

Type TypeChecker::look_thru(const Type& t) const
{
    if (auto mtv = t.to<MetaTypeVar>())
    {
        if (auto t2 = mtv->filled())
            return look_thru(*t2);
        else
            return t;
    }
    else if (auto s = expand_type_synonym(t))
        return look_thru(*s);
    else
        return t;
}

Type TypeChecker::check_type(const Type& type, kindchecker_state& K) const
{
    // So, currently, we
    // (1) infer kinds for all the free variables.
    // (2) then add foralls for the free variables.
    // Should we be doing synonym substitution FIRST?

    return K.kind_and_type_check_type( type );
}

Type TypeChecker::check_type(const Type& type) const
{
    // This should be rather wasteful... can we use a reference?
    kindchecker_state K( this_mod() );

    return check_type(type, K);
}

Type TypeChecker::check_constraint(const Type& type) const
{
    // This should be rather wasteful... can we use a reference?
    kindchecker_state K( this_mod() );

    return K.kind_and_type_check_constraint( type );

}

std::bitset<8> set_occurs_check_maybe(std::bitset<8> result)
{
    if (result.test(occurs_definitely_bit))
    {
        result.reset(occurs_definitely_bit);
        result.set(occurs_maybe_bit);
    }
    return result;
}

bool has_occurs_check(std::bitset<8> result)
{
    return result.test(occurs_definitely_bit) or result.test(occurs_maybe_bit);
}

std::bitset<8> TypeChecker::check_type_equality(const Type& lhs, const Type& rhs) const
{
    if (auto tt = filled_meta_type_var(rhs))
        return check_type_equality(lhs, *tt);
    else if (auto mtv = rhs.to<MetaTypeVar>())
    {
        if (lhs == *mtv)
            return occurs_definitely_result;
        else
            return ok_result;
    }
    else if (auto tv = rhs.to<TypeVar>())
    {
        if (lhs == *tv)
            return occurs_definitely_result;
        else
            return ok_result;
    }
    else if (auto app = is_type_app(rhs))
    {
        auto& [fun,arg] = *app;
        return check_type_equality(lhs, fun) | check_type_equality(lhs, arg);
    }
    else if (auto tfam = is_type_fam_app(rhs))
    {
        if (same_type(lhs,rhs))
            return occurs_definitely_result;
        else
        {
            auto [_,args] = *tfam;
            auto result = type_family_result;
            for(auto& arg: args)
                result |= check_type_equality(lhs,arg);
            return set_occurs_check_maybe(result);
        }
    }
    // We can record that type synonyms either do or do not expand to have
    // (i) type families
    // (ii) foralls and constraints
    else if (auto tsyn = expand_type_synonym(rhs))
    {
        return check_type_equality(lhs, *tsyn);
    }
    else if (rhs.is_a<TypeCon>())
    {
        return ok_result;
    }
    else if (auto forall = rhs.to<ForallType>())
    {
        return check_type_equality(lhs, forall->type) | impredicative_result;
    }
    else if (auto con = rhs.to<ConstrainedType>())
    {
        auto result = check_type_equality(lhs, con->type) | impredicative_result;
        for(auto& constraint: con->context.constraints)
            result |= check_type_equality(lhs, constraint);
        return result;
    }
    else if (auto st = rhs.to<StrictType>())
    {
        return check_type_equality(lhs, st->type);
    }
    else if (auto lt = rhs.to<LazyType>())
    {
        return check_type_equality(lhs, lt->type);
    }
    else
        std::abort();
}

TypeChecker TypeChecker::copy_clear_wanteds(bool bump_level) const
{
    auto tc2 = *this;
    tc2.current_wanteds() = {};
    if (bump_level)
        tc2.inc_level();
    return tc2;
}

void TypeChecker::promote_mtv(const MetaTypeVar& mtv, int new_level)
{
    assert(mtv.level() > new_level);
    auto mtv2 = FreshVarSource::fresh_meta_type_var( new_level, unloc(mtv.name), *mtv.kind);
    mtv.fill( mtv2 );
}

bool TypeChecker::maybe_promote_mtv(const MetaTypeVar& mtv, int new_level)
{
    if (mtv.level() > new_level)
    {
        promote_mtv(mtv, new_level);
        return true;
    }
    else
        return false;
}

void TypeChecker::promote(Type type, int new_level)
{
    for(auto& mtv: free_meta_type_variables(type))
        maybe_promote_mtv(mtv, new_level);

    // check for skolem_escape
    if (max_level(type) > new_level)
    {
        throw myexception(print_note())<<"skolem-escape in '"<<type<<"':\n  cannot promote to level "<<new_level<<" because of type variables on level "<<max_level(type);
    }
}

void TypeChecker::add_binders(const local_value_env& binders)
{
    poly_env() = plus_prefer_right( poly_env(), binders );
}

TypeChecker
TypeChecker::copy_add_binders(const local_value_env& binders) const
{
    auto new_state = copy_clear_wanteds();
    new_state.add_binders( binders );
    return new_state;
}

WantedConstraints& TypeChecker::current_wanteds()
{
    return collected_wanteds;
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

string class_arg_name(const Type& class_arg)
{
    auto [a_head, _] = decompose_type_apps(class_arg);

    if (auto tc = a_head.to<TypeCon>())
        return get_name_for_typecon(*tc);
    else
        return "_";
}

Core::Var TypeChecker::fresh_dvar(const Type& pred, bool qualified)
{
    auto [class_head, class_args] = decompose_type_apps(pred);
    string name = "dvar";
    if (auto tc = class_head.to<TypeCon>())
    {
        // 1. Get constraint class
        name = get_unqualified_name(unloc(tc->name));
        if (name == "~")
            name = "co";
        else
            name = "d" + name;

        // 2. The class_args must be (i) a variable or (ii) a type constructor applied to simple, distinct type variables.
        for(auto& class_arg: class_args)
            name += class_arg_name(class_arg);
    }
    auto dvar = get_fresh_var(name, qualified);
    dvar.type_ = std::make_shared<Type>(pred);
    return dvar;
}

Core::Var TypeChecker::add_wanted(const ConstraintOrigin& origin, const Type& pred)
{
//    if (context()->locs.empty())
//        std::cerr<<"No location -- add wanted!";

    auto dvar = fresh_dvar(pred);

    current_wanteds().simple.push_back( {origin, Wanted, dvar, pred, context()} );

    return dvar;
}

TypeChecker::TypeChecker(FreshVarState& fvs, const string& s, Module& m)
    :FreshVarSource(fvs, s), local_state(new TypeCheckerContext)
{
    global_state = std::make_shared<global_tc_state>(m);
}

Hs::Var TypeChecker::find_prelude_var(string name) const
{
    if (this_mod().is_declared(name))
        name = this_mod().lookup_symbol(name)->name;
    return Hs::Var(name);
}

ID TypeChecker::find_prelude_tycon_name(const string& name) const
{
    if (this_mod().type_is_declared(name))
        return this_mod().lookup_type(name)->name;
    else
        return name;
}

TypeCon TypeChecker::find_prelude_tycon(const string& name) const
{
    auto prelude_name = find_prelude_tycon_name(name);
    return TypeCon({noloc, prelude_name });
}

Type TypeChecker::bool_type() const
{
    return find_prelude_tycon("Bool");
}

Type TypeChecker::char_type() const
{
    return find_prelude_tycon("Char");
}

Type TypeChecker::int_type() const
{
    return find_prelude_tycon("Int");
}

Type TypeChecker::integer_type() const
{
    return find_prelude_tycon("Integer");
}

Type TypeChecker::double_type() const
{
    return find_prelude_tycon("Double");
}

bool TypeChecker::add_substitution(const MetaTypeVar& a, const Type& type)
{
    return try_insert(a, type);
}


void TypeChecker::unify(const Type& t1, const Type& t2)
{
    unify_solve_(UnifyOrigin{t1,t2}, t1, t2);
}

void TypeChecker::unify(const Type& t1, const Type& t2, const ConstraintOrigin& orig)
{
    unify_solve_(orig, t1, t2);
}

optional<bsubstitution_t> TypeChecker::maybe_unify(const Type& t1, const Type& t2) const
{
    unification_env env;
    bsubstitution_t s;
    if (maybe_unify_(true, env, t1, t2, s))
        return s;
    else
        return {};
}

optional<bsubstitution_t> TypeChecker::maybe_match(const Type& t1, const Type& t2) const
{
    unification_env env;
    bsubstitution_t s;
    if (maybe_unify_(false, env, t1, t2, s))
        return s;
    else
        return {};
}

bsubstitution_t TypeChecker::match(const Type& t1, const Type& t2, const myexception& e) const
{
    auto subst = maybe_match(t1,t2);
    if (not subst)
        throw e;
    else
        return *subst;
}

bsubstitution_t TypeChecker::match(const Type& t1, const Type& t2) const
{
    auto e = myexception()<<"match failed: "<<t1<<" !~ "<<t2;
    return match(t1,t2,e);
}


std::pair<Type, Type> TypeChecker::unify_function(const Type& t)
{
    assert(is_rho_type(t));
    if (auto arg_and_result = is_function_type(t))
        return *arg_and_result;
    else
    {
        auto a = fresh_meta_type_var( kind_type() );
        auto b = fresh_meta_type_var( kind_type() );
        auto fun_t = make_arrow_type(a,b);
        unify(t, fun_t, UnifyOrigin{t, fun_t});
        return {a,b};
    }
}

std::pair<Type, Type> TypeChecker::unify_function(const Type& t, const ConstraintOrigin& orig)
{
    assert(is_rho_type(t));
    if (auto arg_and_result = is_function_type(t))
        return *arg_and_result;
    else
    {
        auto a = fresh_meta_type_var( kind_type() );
        auto b = fresh_meta_type_var( kind_type() );
        unify(t, make_arrow_type(a,b), orig);
        return {a,b};
    }
}


std::tuple<Type, Type, Type> TypeChecker::unify_two_arg_function(const Type& t)
{
    auto [a, partial_result] = unify_function(t);
    auto [b, result] = unify_function(partial_result);
    return {a, b, result};
}


DataConInfo TypeChecker::constructor_info(const Hs::Con& con)
{
    if (auto info = this_mod().constructor_info(con.name))
        return *info;
    else
        throw note_exception()<<"Unrecognized constructor: "<<con.name;
}


value_env add_constraints(const std::vector<Type>& preds, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, add_constraints(preds, monotype)} );
    return env2;
}

// OK, so this returns something of type exp_sigma
Core::wrapper TypeChecker::checkSigma(Hs::LExp& E, const SigmaType& sigma_type)
{
    if (E.loc) push_source_span(*E.loc);

    // 1. skolemize the type
    auto [wrap_gen, tvs, givens, rho_type] =
        skolemize_and(sigma_type,
                      [&](const Type& rho_type, auto& tcs2) {
                          tcs2.tcRho(E, Check(rho_type));
                      }
            );

    if (E.loc) pop_source_span();

    // 2. modify E, which is of type rho_type, to be of type sigma_type
    return wrap_gen;
}

// The idea is that we need an e2, but we have a t1.
// So, if we can make an e2 from the t1, then we are good.
// The wrapper is evidence that we can, and also converts a value of type t1 to a value of type e2.
Core::wrapper TypeChecker::subsumptionCheck(const ConstraintOrigin& origin, const Type& t1, const Expected& e2)
{
    if (auto t2 = e2.read_type_maybe())
        return subsumptionCheck(origin, t1, *t2);
    else
    {
        auto I = e2.infer();
        fillInfer(t1, *I);
        return Core::WrapId;
    }
}

Core::wrapper TypeChecker::subsumptionCheck(const ConstraintOrigin& origin, const Type& t1, const Type& t2)
{
    /*
      If we can make y :: t2 out of x :: t1 then it is OK.  (without doing eta reduction, according to QL).
      We return a wrapper that perform this conversion.

      Q: How do we handle dictionary constraints?

      A: When doing this conversion, we are allowed to make up dictionary variables for any constraints that
      don't contain skolem variables, and emit them into the current LIE.

      A: It is very much possible for t2 to be a simple meta-type-variable, in which case ALL
      wanted constraints will be emitted into the current LIE.

      Example 1. If t1 = (forall a b . a -> b) and t2 = (forall c. c -> c) then
           y = x @c @c

      Example 2. If t1 = (forall a. Eq a => a)  and t2 = (forall b. Ord b => b), then
           y = /\b.\dictOrd -> x b (eqFromOrd dictOrd)

      Example 3. If t1 = (forall a. Eq a => a) and t2 = Int then
           y = x Int dictEqInt
      where entails checks that there is actually an instance for (Eq Int)?

      Example 4. If t1 = (forall a. Eq a => a -> a) and t2 = b (meta type var) then
           b ~ a -> a
      and constraints1 = {Eq a}.  Currently this fails, because we can't infer Eq a.
      However, perhaps we should add Eq a to the environment?
    */

    auto [wrap_gen, tvs2, givens, type2, wrap_apply]
        = skolemize_and_result<Core::wrapper>
        (
            t2,
            [&](const Type& rho_type, TypeChecker& tcs2)
            {
                auto [wrap_apply, type1] = tcs2.instantiate_emit(origin, t1);
                tcs2.unify(type1, rho_type);
                return wrap_apply;
            }
        );

    // For example, \darg1 -> \let darg2 = compute darg1 in function darg1 darg2
    return wrap_gen * wrap_apply;
}

std::tuple<Core::wrapper, Type>
TypeChecker::instantiate_emit(const ConstraintOrigin& origin, const Type& polytype)
{
    auto [_, wanteds, rho_type] = instantiate(origin, polytype);

    current_wanteds() += wanteds;

    auto dict_args = dict_vars_from_lie( wanteds );

    return {Core::WrapApply(dict_args), rho_type};
}

Core::wrapper
TypeChecker::instantiateSigma(const ConstraintOrigin& origin, const Type& polytype, const Expected& exp_type)
{
    if (auto I = exp_type.infer())
    {
        auto [wrap, rho_type] = instantiate_emit(origin, polytype);
        fillInfer(rho_type, *I);
        return wrap;
    }
    else
    {
        // why would this be a rho?
        // assert(is_rho_type(exp_type.check_type()));
        return subsumptionCheck(origin, polytype, exp_type.check_type());
    }
}
// FIXME:: Wrappers:
//
// Note that instantiate collapses any structure in leading foralls/contexts.
//
//    forall a. Eq a => forall b. Ord b => a -> b -> a --> [a,b]  + [Eq a, Ord b] + (a -> b -> a)
//
// So we may need a wrapper to create a
//
//    forall a. Eq a => forall b. Ord b => type
//
// from
//
//    forall a b. (Eq a, Ord b) => type
//
// Maybe something like:
//
//    /\(a::*).\dict (d1::Eq a) -> /\(b::*).\dict (d2::Ord b) -> <RESULT> a b d1 d2
//
// This would take the code for <RESULT> and add the other code around it.
//
// Now, actually, we may NOT need this until add type /\s, because the dictionary arguments should be in the right order.

substitution_t TypeChecker::get_subst_for_tv_binders(const vector<TypeVar>& type_var_binders)
{
    substitution_t s;
    for(auto& tv: type_var_binders)
    {
        assert(tv.kind);
        auto new_tv = fresh_meta_type_var(unloc(tv.name), *tv.kind);
        s = s.insert({tv,new_tv});
    }
    return s;
}

substitution_t TypeChecker::fresh_tv_binders(vector<TypeVar>& type_var_binders)
{
    substitution_t s;
    for(auto& tv: type_var_binders)
    {
        assert(tv.kind);
        auto new_tv = fresh_other_type_var(unloc(tv.name), *tv.kind);
        s = s.insert({tv,new_tv});
        tv = new_tv;
    }
    return s;
}

vector<MetaTypeVar> new_meta_type_vars(const substitution_t& s)
{
    vector<MetaTypeVar> tvs;
    for(auto& [tv,new_tv]: s)
        tvs.push_back(new_tv.as_<MetaTypeVar>());
    return tvs;
}

tuple<vector<MetaTypeVar>, LIE, Type> TypeChecker::instantiate(const ConstraintOrigin& origin, const Type& t)
{
    // 1. Handle foralls
    vector<MetaTypeVar> tvs;
    LIE wanteds;
    Type type = t;

    if (auto fa = type.to<ForallType>())
    {
        auto s = get_subst_for_tv_binders(fa->type_var_binders);
        tvs = new_meta_type_vars(s);
        type = apply_subst(s, fa->type);
    }

    // 2. Handle constraints
    if (auto ct = type.to<ConstrainedType>())
    {
        wanteds = preds_to_constraints(origin, Wanted, ct->context.constraints);
        type = ct->type;
    }

    // 3. Handle the exposed type being a polytype
    if (not tvs.empty() or not wanteds.empty())
    {
        auto [tvs2, wanteds2, type2] = instantiate(origin, type);

        for(auto& tv2: tvs2)
            tvs.push_back(tv2);

        for(auto& wanted: wanteds2)
            wanteds.push_back(wanted);

        type = type2;
    }

    return {tvs, wanteds, type};
}

tuple<Core::wrapper, vector<TypeVar>, LIE, Type> TypeChecker::skolemize(const Type& polytype, bool skolem)
{
    // 1. Handle foralls
    
    if (auto fa = polytype.to<ForallType>())
    {
        vector<TypeVar> tvs;

        substitution_t s;
        for(auto& tv: fa->type_var_binders)
        {
            assert(tv.kind);
            TypeVar new_tv;
            if (skolem)
                new_tv = fresh_rigid_type_var(unloc(tv.name), *tv.kind);
            else
                new_tv = fresh_other_type_var(unloc(tv.name), *tv.kind);
            s = s.insert({tv,new_tv});

            tvs.push_back(new_tv);
        }

        auto type = apply_subst(s, fa->type);

        auto [wrap2, tvs2, givens2, type2] = skolemize(type, skolem);

        // Compute tvs from local tvs followed by tvs of sub-type.
        for(auto& tv2: tvs2)
            tvs.push_back(tv2);

        // auto wrap = Core::WrapLambdaTypes(fa->type_var_binders) * wrap2;
        auto wrap = wrap2;

        return {wrap, tvs, givens2, type2};
    }

    // 2. Handle constraints
    else if (auto ct = polytype.to<ConstrainedType>())
    {
        // Compute givens from local givens followed by givens of sub-type.
        auto givens = preds_to_constraints(GivenOrigin(), Given, ct->context.constraints);
        auto wrap1 = Core::WrapLambda( dict_vars_from_lie( givens ) );

        auto [wrap2, tvs2, givens2, type2] = skolemize(ct->type, skolem);

        // Append the inner givens to the list
        for(auto& given:  givens2)
            givens.push_back(given);

        return {wrap1 * wrap2, tvs2, givens, type2};
    }

    // 3. If the type has no foralls and no constraints, then it is just a rho-type.
    else
        return {Core::WrapId, {}, {}, polytype};
}

std::tuple<Core::wrapper, std::vector<TypeVar>, LIE, Type>
TypeChecker::skolemize_and(const Type& polytype, const tc_action<Type>& nested_action)
{
    // 1. Skolemize the type at level+1
    inc_level();
    auto [wrap, tvs, givens, rho_type] = skolemize(polytype, true);
    dec_level();

    // 2. Perform the action, maybe creating an implication.
    // c++20 should allow us to capture rho-type, but clang is broken until probably clang-16.
    auto ev_decls = maybe_implication(tvs, givens, [&,&rho_type=rho_type](auto& tc) {nested_action(rho_type, tc);});

    // 3. Combine the wrappers
    return {wrap * Core::WrapLet(ev_decls), tvs, givens, rho_type};
}

shared_ptr<const Core::Decls>
TypeChecker::maybe_implication(const std::vector<TypeVar>& tvs, const LIE& givens, const tc_action<>& nested_action)
{
    auto ev_decls = std::make_shared<Core::Decls>();

    bool need_implication = not (tvs.empty() and givens.empty());

    auto tcs2 = copy_clear_wanteds(need_implication);

    nested_action(tcs2);

    auto wanteds = tcs2.current_wanteds();

    if (not wanteds.empty())
    {
        if (need_implication)
        {
            auto imp = std::make_shared<Implication>(level()+1, tvs, givens, wanteds, ev_decls, context());
            current_wanteds().implications.push_back( imp );
        }
        else
            current_wanteds() += wanteds;
    }

    return ev_decls;
}

LIE TypeChecker::preds_to_constraints(const ConstraintOrigin& origin, ConstraintFlavor flavor, const vector<Type>& constraints)
{
    LIE ordered_lie;
    for(auto& constraint: constraints)
    {
//        if (context()->locs.empty())
//            std::cerr<<"No location - preds to constraints!";

        auto dvar = fresh_dvar(constraint);
        ordered_lie.push_back({origin, flavor, dvar, constraint, context()});
    }
    return ordered_lie;
}

Type remove_top_level_foralls(Type t)
{
    while(auto fa = t.to<ForallType>())
        t = fa->type;
    return t;
}

void TypeChecker::get_constructor_info(const Hs::Decls& decls)
{
    kindchecker_state ks( this_mod() );

    for(auto& [_,decl]: decls)
    {
        auto d = decl.to<Hs::DataOrNewtypeDecl>();
        if (not d) continue;

        for(auto& [name,con_info]: ks.type_check_data_type(*this, *d))
        {
            auto C = this_mod().lookup_local_symbol(name);
            assert(C);
            assert(not C->con_info);
            C->con_info = std::make_shared<DataConInfo>(con_info);
        }
    }

//     for(auto& [con,type]: con_env())
//     {
//         std::cerr<<con<<" :: "<<type.print()<<"\n";
//     }
//     std::cerr<<"\n";
}

Kind result_kind_for_type_vars(vector<Hs::LTypeVar>& type_vars, Kind k)
{
    for(auto& tv: type_vars)
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // record a version of the var with that contains its kind
        unloc(tv).kind = ka->arg_kind;

        // set up the next iteration
        k = ka->result_kind;
    }
    // This is the result kind.
    return k;
}

Hs::Decls TypeChecker::add_type_var_kinds(Hs::Decls type_decls)
{
    for(auto& [_,type_decl]: type_decls)
    {
        if (type_decl.is_a<Hs::DataOrNewtypeDecl>())
        {
            auto D = type_decl.as_<Hs::DataOrNewtypeDecl>();
            auto kind = this_mod().lookup_local_type(unloc(D.name))->kind;
            assert(kind);
            result_kind_for_type_vars( D.type_vars, kind);
            type_decl = D;
        }
        else if (type_decl.is_a<Hs::ClassDecl>())
        {
            auto C = type_decl.as_<Hs::ClassDecl>();
            auto kind = this_mod().lookup_local_type(unloc(C.name))->kind;
            assert(kind);
            result_kind_for_type_vars( C.type_vars, kind);
            type_decl = C;
        }
        else if (type_decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto T = type_decl.as_<Hs::TypeSynonymDecl>();
            auto kind = this_mod().lookup_local_type(unloc(T.name))->kind;
            assert(kind);
            result_kind_for_type_vars( T.type_vars, kind);
            type_decl = T;
        }
    }

    return type_decls;
}

pair<Hs::Binds,Core::Decls> typechecker_result::all_binds() const
{
    Hs::Binds all = value_decls;
    all.signatures = {};

    ranges::insert(all, all.end(), default_method_decls);
    ranges::insert(all, all.end(), instance_method_decls);
    ranges::insert(all, all.end(), class_binds);

//    std::cerr<<"Haskell decls:\n";
//    std::cerr<<all.print();

    Core::Decls all2 = top_simplify_decls;
    all2 += dfun_decls;

//    std::cerr<<"\n\nCore decls:\n";
//    std::cerr<<print_cdecls(all2);
//    std::cerr<<"\n\n";

    return {all, all2};
}

typechecker_result Module::typecheck( FreshVarState& fresh_vars, Hs::ModuleDecls M )
{
    // 1. Check the module's type declarations, and derives a Type Environment TE_T:(TCE_T, CVE_T)
    //    OK, so datatypes produce a
    //    * Type Constructor Environment (TCE) = tycon -> (kind, arity, method of applying the tycon?)
    //    * Constructor Value Environment (CVE)
    //
    // 2. Check the module's class declarations, produce some translated bindings -> binds_C ( GVE_C, CE_C, GIE_C )
    //
    // 3. We need to import/export:
    //    - TCE_T: TypeConEnv& tce                type -> kind
    //    - CVE_T: DataConEnv& state.con_info       constructor id -> type
    //    - GIE_C: global_instance_env state.gie    ??
    //      * superclass extractors?
    //      * instance functions to create dictionaries...
    //      * 
    //    - GVE_C: global_value_environment gve     id -> type (for class methods)
    //    - CE_C: class_info                        id -> class_info

    //    - GVE:   global_value_environment env     id -> type (for other values)
    //    - class_binds                             id -> body        = unused?
    //
    // 4. Should imports/export only affect what NAMES are in scope, or also things like the instance environment?

    auto tc_state = std::make_shared<TypeChecker>( fresh_vars, name, *this);

    // 1. Get the types for defaulting.
    tc_state->get_defaults( M );

    // 2. Find the kind and arity of type constructors declared in this module ( TCE_T = type con info, part1 )
    tc_state->get_tycon_info( M.type_decls );

    // 3. Annotate tyvars in types with their kind.
    // Do we need this, could we do it in infer_type_for_classes/synonyms/data?
    M.type_decls = tc_state->add_type_var_kinds( M.type_decls );

    // 4. Get type synonyms
    tc_state->get_type_synonyms(M.type_decls);

    // 5. Get type families and type family instances
    tc_state->get_type_families(M.type_decls);

    // 6. Get types for value constructors  (CVE_T = constructor types)
    tc_state->get_constructor_info(M.type_decls);

    // 7. Get types and values for class method selectors and superclass selectors (CE_C  = class name -> class info)
    auto class_binds = tc_state->infer_type_for_classes(M.type_decls);

    // 8. Get types and names for instances (pass 1)
    auto named_instances = tc_state->infer_type_for_instances1(M.type_decls);

    // 9. Get types for foreign imports
    tc_state->infer_type_for_foreign_imports(M.foreign_decls);

    // 10. Typecheck value decls
    auto value_decls = tc_state->infer_type_for_binds_top(M.value_decls);

    // 11. Typecheck default methods
    auto dm_decls = tc_state->infer_type_for_default_methods(M.type_decls);

    // 12. Typecheck instance methods and generate dfuns (pass 2)
    auto [instance_method_binds, dfun_decls] = tc_state->infer_type_for_instances2(named_instances);

    // 13. Default top-level ambiguous type vars.
    auto top_simplify_decls = tc_state->simplify_and_default_top_level();

    // 14. Record types on the value symbol table
    for(auto& [var,type]: tc_state->poly_env())
    {
        auto V = lookup_local_symbol(var.name);
        assert(V->symbol_type != constructor_symbol);
        V->type = type;
    }

    // 15. Print messages sorted by location.
    show_messages(file, std::cerr, tc_state->messages());

    // If we throw an exception later, the stuff printed to cerr will be printed again.
    // Should we be printing to out_screen instead?
    exit_on_error(tc_state->messages());

    tc_state->messages().clear();

    Core::Decls dfun_decls2;
    for(auto& [var,wrap,rhs]: dfun_decls)
        dfun_decls2.push_back({var,wrap(rhs)});

    return {class_binds, value_decls, dm_decls, instance_method_binds, dfun_decls2, top_simplify_decls};
}

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
