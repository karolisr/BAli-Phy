#include "parse.H"
#include "computation/module.H"
#include <deque>
#include <set>
#include <tuple>
#include <utility>
#include "io.H"
#include "models/parameters.H"
#include "computation/loader.H"
#include "computation/expression/expression.H"
#include "computation/expression/AST_node.H"
#include "computation/expression/apply.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/lambda.H"
#include "computation/expression/var.H"
#include "computation/expression/constructor.H"
#include "desugar.H"

using std::string;
using std::vector;
using std::set;
using std::deque;
using std::pair;

//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html

// See list in computation/loader.C
//

expression_ref infix_parse(const Module& m, const set<string>& bound, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
expression_ref infix_parse_neg(const Module& m, const set<string>& bound, const symbol_info& op1, deque<expression_ref>& T)
{
    assert(not T.empty());

    expression_ref E1 = T.front();
    T.pop_front();

    // We are starting with a Neg
    if (E1.head() == AST_node("neg"))
    {
	if (op1.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

	E1 = infix_parse_neg(m, bound, symbol_info("-",variable_symbol, 2,6,left_fix), T);

	return infix_parse(m, bound, op1, {AST_node("id","negate"),E1}, T);
    }
    // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
    else
	return infix_parse(m, bound, op1, E1, T);
}

symbol_info get_op_sym(const Module& m, const set<string>& bound, const expression_ref& O)
{
    if (not is_AST(O, "id"))
	throw myexception()<<"Can't use expression '"<<O.print()<<"' as infix operator.";

    symbol_info op_sym;
    auto name = O.as_<AST_node>().value;

    if (bound.count(name))
    {
	// We assume that fixity for operators at non-global scope is unknown.
	// FIXME: we should record precedence and fixity for locally-bound variables.
	//        should this be merged with the global scope somehow?
	op_sym.name = name;
	op_sym.precedence = 9;
	op_sym.fixity = left_fix;
    }
    else if (m.is_declared( name ) )
	op_sym = m.get_operator( name );
    else
	throw myexception()<<"Using unknown operator '"<<name<<"' as infix operator.";

    return op_sym;
}

/// Expression is of the form ... op1 E1 [op2 ...]. Get right operand of op1.
expression_ref infix_parse(const Module& m, const set<string>& bound, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T)
{
    if (T.empty())
	return E1;

    expression_ref op2_E = T.front();
    symbol_info op2 = get_op_sym(m, bound, op2_E);

    // illegal expressions
    if (op1.precedence == op2.precedence and (op1.fixity != op2.fixity or op1.fixity == non_fix))
	throw myexception()<<"Must use parenthesis to order operators '"<<op1.name<<"' and '"<<op2.name<<"'";

    // left association: ... op1 E1) op2 ...
    if (op1.precedence > op2.precedence or (op1.precedence == op2.precedence and op1.fixity == left_fix))
	return E1;

    // right association: .. op1 (E1 op2 {...E3...}) ...
    else
    {
	T.pop_front();
	expression_ref E3 = infix_parse_neg(m, bound, op2, T);

	expression_ref E1_op2_E3 = {op2_E, E1, E3};

	return infix_parse(m, bound, op1, E1_op2_E3, T);
    }
}

expression_ref desugar_infix(const Module& m, const set<string>& bound, const vector<expression_ref>& T)
{
    deque<expression_ref> T2;
    T2.insert(T2.begin(), T.begin(), T.end());

    return infix_parse_neg(m, bound, {"",variable_symbol,2,-1,non_fix}, T2);
}

set<string> find_all_ids(const expression_ref& E)
{
    if (is_AST(E,"id"))
	return {E.as_<AST_node>().value};

    if (E.is_atomic()) return set<string>();

    set<string> bound;
    for(const auto& e:E.sub())
	add(bound, find_all_ids(e));

    return bound;
}

set<string> find_bound_vars(const expression_ref& E)
{
    if (E.is_expression())
    {
	set<string> bound;
	for(const auto& e:E.sub())
	    add(bound, find_bound_vars(e));
	return bound;

    }
    else if (is_AST(E,"id"))
    {
	auto& value = E.as_<AST_node>().value;
	if (not is_haskell_con_name(value))
	    return {value};
    }
    return {};
}

string get_func_name(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));
    auto& lhs = decl.sub()[0];
    assert(is_AST(lhs,"id"));
    return lhs.head().as_<AST_node>().value;
}

string desugar_get_func_name(const expression_ref& decl)
{
    auto& lhs = decl.sub()[0];
    return lhs.head().as_<var>().name;
}

bool is_pattern_binding(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));
    return is_haskell_con_name(get_func_name(decl));
}

bool is_function_binding(const expression_ref& decl)
{
    return not is_pattern_binding(decl);
}

bool is_irrefutable_pat(const expression_ref& E)
{
    std::abort();
    return (E.size() == 1) and is_AST(E.sub()[0], "apat_var");
}

set<string> get_pattern_bound_vars(const expression_ref& decl)
{
    std::abort();
    assert(is_AST(decl,"Decl"));

    auto& lhs = decl.sub()[0];

    return find_bound_vars(lhs);
}

vector<expression_ref> get_patterns(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));

    expression_ref lhs = decl.sub()[0];
    assert(lhs.head().is_a<var>());

    return lhs.sub();
}

expression_ref get_body(const expression_ref& decl)
{
    expression_ref rhs = decl.sub()[1];
    assert(is_AST(rhs,"rhs"));
    assert(rhs.size() == 1);
    return rhs.sub()[0];
}

vector<expression_ref> parse_fundecls(const vector<expression_ref>& v)
{
    // Now we go through and translate groups of FunDecls.
    vector<expression_ref> decls;
    for(int i=0;i<v.size();i++)
    {
	auto& decl = v[i];

	// This is a declaration, but not a type we're handling here.
	if (not is_AST(decl,"Decl"))
	{
	    decls.push_back( decl );
	    continue;
	}

	auto& lhs = decl.sub()[0];

	// If its just a variable with no args, don't call def_function because ... its complicated?
	if (lhs.is_a<var>())
	{
	    decls.push_back(decl.head() + lhs + decl.sub()[1].sub()[0]);
	    continue;
	}

	auto& f = lhs.head();

	// Skip pattern bindings
	if (f.is_a<constructor>())
	{
	    decls.push_back(decl);
	    continue;
	}

	vector<vector<expression_ref> > patterns;
	vector<expression_ref> bodies;
	string name = f.as_<var>().name;
	patterns.push_back( get_patterns(decl) );
	bodies.push_back( get_body(decl) );

	for(int j=i+1;j<v.size();j++)
	{
	    if (not is_AST(v[j],"Decl")) break;
	    auto& j_lhs = v[j].sub()[0];
	    auto& j_f   = j_lhs.head();
	    if (j_f.is_a<constructor>()) break;

	    if (j_f.as_<var>().name != name) break;

	    patterns.push_back( get_patterns(v[j]) );
	    bodies.push_back( get_body(v[j]) );

	    if (patterns.back().size() != patterns.front().size())
		throw myexception()<<"Function '"<<name<<"' has different numbers of arguments!";
	}
	decls.push_back(AST_node("Decl") + var(name) + def_function(patterns,bodies) );

	// skip the other bindings for this function
	i += (patterns.size()-1);
    }
    return decls;
}

expression_ref get_fresh_id(const string& s, const expression_ref& /* E */)
{
    return AST_node("id",s);
}

expression_ref shift_list(vector<expression_ref>& v)
{
    if (not v.size()) return {};

    auto head = v[0];
    for(int i=0;i<v.size()-1;i++)
	v[i] = v[i+1];
    v.pop_back();
    return head;
}

// The issue here is to rewrite @ f x y -> f x y
expression_ref unapply(const expression_ref& E)
{
    if (not is_apply(E.head())) return E;

    auto args = E.sub();
    auto head = shift_list(args);
    if (head.size())
    {
	// The head is an apply expression that's been un-applied!
	assert(is_AST(head,"id"));
	for(auto& arg: args)
	    head = head + arg;
	return head;
    }
    else
	return expression_ref{head,std::move(args)};
}

/*
 * We probably want to move away from using dummies to represent patterns.
 * - Dummies can't represent e.g. irrefutable patterns.
 */

// What would be involved in moving the renamer to a kind of phase 2?
// How do we get the exported symbols before we do the desugaring that depends on imports?

// rename_infix does:
// (i) precedence handling for infix expressions
// (ii) rewrites @ f x y -> f x y for decls
// (iii) rewrites @ C x y -> C x y for patterns

expression_ref rename_infix(const Module& m, const expression_ref& E, bool remove_apply)
{
    if (not E.is_expression()) return E;

    assert(E.size());
    auto v = E.sub();

    if (is_AST(E,"Decl"))
    {
	assert(not remove_apply);
	/* lhs */
	v[0] = rename_infix(m, v[0], true);
	/* rhs */
	v[1] = rename_infix(m, v[1], false);
	assert(is_AST(v[0],"id"));
    }
    else if (is_AST(E,"alt"))
    {
	assert(not remove_apply);
	/* pat */
	v[0] = rename_infix(m, v[0], true);
	/* body */
	v[1] = rename_infix(m, v[1], false);
    }
    else if (is_AST(E,"Lambda"))
    {
	assert(not remove_apply);
	auto v = E.sub();
	for(int i=0;i<v.size()-1;i++)
	    v[i] = rename_infix(m, v[i], true);
	v.back() = rename_infix(m, v.back(), false);
    }
    else if (is_AST(E,"PatQual"))
    {
	assert(not remove_apply);
	/* pat */
	v[0] = rename_infix(m, v[0], true);
	/* body */
	v[1] = rename_infix(m, v[1], false);
    }
    else
    {
	for(auto& e: v)
	    e = rename_infix(m, e, remove_apply);
    }

    // If this is an infix expression, handle precedence.
    if (is_AST(E,"infixexp"))
    {
	auto E2 = desugar_infix(m, {}, v);
	if (remove_apply)
	    E2 = unapply(E2);
	return E2;
    }

    assert(v.size());
    expression_ref E2{E.head(),v};
    if (remove_apply)
	return unapply(E2);
    else
	return E2;
}

expression_ref rename(const Module& m, const expression_ref& E)
{
    return rename(m,E,set<string>());
}


std::set<string> rename_pattern(const Module& m, expression_ref& pat, bool top = false)
{
    // 0. Handle WildCardPattern
    if (is_AST(pat,"WildcardPattern"))
    {
	pat = var(-1);
	return {};
    }

    // 1. Normalize pattern from (i) @ X y -> X y, (ii) X -> X, and (ii) y -> y
    //    Maybe do this in rename_infix?
    assert(not is_apply(pat.head()));
    expression_ref head = pat.head();
    vector<expression_ref> args;
    if (pat.size())
	args = pat.sub();

    // 2. Get the identifier name for head
    assert(is_AST(head,"id"));
    auto id = pat.head().as_<AST_node>().value;

    // 3. Handle if identifier is a variable
    if (not is_haskell_con_name(id))
    {
	if (args.size()) throw myexception()<<"Pattern "<<pat<<" doesn't start with a constructor!";
	if (is_qualified_symbol(id)) throw myexception()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!";
	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	// FIXME - since we are creating an ID here, we should give it a unique id!
	pat = var(id);
	return {id};
    }
    
    // 4. Find constructor name if identifier is a constructor
    if (not m.is_declared(id))
	throw myexception()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!";

    const symbol_info& S = m.lookup_symbol(id);
    if (S.symbol_type != constructor_symbol)
	throw myexception()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!";

    if (S.arity != args.size())
	throw myexception()<<"Constructor '"<<id<<"' arity "<<S.arity<<" doesn't match pattern '"<<pat<<"'!";

    head = constructor(S.name, S.arity);

    // 5. Rename arguments and accumulate bound variables
    set<string> bound;
    // Rename the arguments
    for(auto& e: args)
    {
	auto bound_here =  rename_pattern(m, e, top);
	// FIXME - check that we're not using any variables twice
	add(bound, bound_here);
    }

    // 6. Construct the renamed pattern
    if (args.size())
	pat = expression_ref{head,args};
    else
	pat = head;

    // 7. Return the variables bound
    return bound;
}

// FIXME make a RnM (or Renamer) object for renaming that can store the module, the set of bound vars, etc.
expression_ref rename_decl(const Module& m, const expression_ref& decl, const set<string>& bound)
{
    assert(is_AST(decl,"Decl"));
    assert(decl.is_expression());
    vector<expression_ref> v = decl.sub();

    auto& lhs = v[0];
    auto& rhs = v[1];
    assert(not is_apply(lhs.head()));

    auto f = lhs.head();

    // 1. Rename the lhs, and get bound variables
    set<string> bound2 = bound;

    // 2. If this is not a pattern binding, then rename the argument patterns
    bool pattern_bind = f.is_a<constructor>();
    if (not pattern_bind)
    {
	assert(f.is_a<var>());
	assert(bound.count(f.as_<var>().name));

	if (lhs.size())
	{
	    auto args = lhs.sub();
	    for(auto& arg: args)
		add(bound2, rename_pattern(m, arg));
	    assert(args.size());
	    lhs = expression_ref{f, args};
	}
	else
	    lhs = f;
	assert(lhs.head().is_a<var>());
    }

    // 3. Rename the body given variables bound in the lhs
    rhs = rename(m, rhs, bound2);

    return expression_ref{decl.head(),v};
}


expression_ref rename(const Module& m, const expression_ref& E, const set<string>& bound)
{
    vector<expression_ref> v;
    if (E.is_expression())
	v = E.sub();
      
    if (E.head().is_a<AST_node>())
    {
	auto& n = E.head().as_<AST_node>();
	if (n.type == "infixexp")
	    std::abort();
	else if (n.type == "Decls" or n.type == "TopDecls")
	{
	    set<string> bound2 = bound;
	    bool top = is_AST(E,"TopDecls");

	    // Find all the names bound HERE, versus in individual decls.

	    // The idea is that we only add unqualified names here, and they shadow
	    // qualified names.
	    for(auto& decl: v)
	    {
		if (not is_AST(decl,"Decl")) continue;

		auto w = decl.sub();
		auto& lhs = w[0];
		auto head = lhs.head();
		assert(is_AST(head,"id"));
		auto bound_names = rename_pattern(m, head, top);
		// For a constructor pattern, rename the whole lhs.
		if (head.is_a<constructor>())
		{
		    assert(bound_names.empty());
		    bound_names = rename_pattern(m, lhs, top);
		}
		// For a variable pattern, just rename the variable.
		else if (lhs.size())
		    lhs = expression_ref{head,lhs.sub()};
		else
		    lhs = head;
		decl = expression_ref{decl.head(),w};
		add(bound2, bound_names);
	    }

	    // Replace ids with dummies
	    for(auto& e: v)
	    {
		if (is_AST(e,"Decl"))
		    e = rename_decl(m, e, bound2);
	    }

	    return new expression(E.head(),v);
	}
	else if (n.type == "Decl")
	    std::abort();
	else if (n.type == "WildcardPattern")
	{
	    return var(-1);
	}
	else if (n.type == "id")
	{
	    // Local vars bind id's tighter than global vars.
	    if (includes(bound,n.value))
		return var(n.value);
	    // If the variable is free, then try top-level names.
	    else if (m.is_declared(n.value))
	    {
		const symbol_info& S = m.lookup_symbol(n.value);
		string qualified_name = S.name;
		return var(qualified_name);
	    }
	    else
		throw myexception()<<"Can't find id '"<<n.value<<"'";
	}
	else if (n.type == "ListComprehension")
	{
	    expression_ref E2 = E;
	    // [ e | True   ]  =  [ e ]
	    // [ e | q      ]  =  [ e | q, True ]
	    // [ e | b, Q   ]  =  if b then [ e | Q ] else []
	    // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
	    // [ e | let decls, Q] = let decls in [ e | Q ]

	    expression_ref True = AST_node("SimpleQual") + bool_true;

	    assert(v.size() >= 2);
	    if (v.size() == 2 and (v[1] == True))
		E2 = AST_node("List") + v[0];
	    else if (v.size() == 2)
		E2 = E.head() + v[0] + v[1] + True;
	    else 
	    {
		expression_ref B = v[1];
		v.erase(v.begin()+1);
		E2 = expression_ref{E.head(),v};

		if (is_AST(B, "SimpleQual"))
		    E2 = AST_node("If") + B.sub()[0] + E2 + AST_node("id","[]");
		else if (is_AST(B, "PatQual"))
		{
		    expression_ref p = B.sub()[0];
		    expression_ref l = B.sub()[1];
		    if (is_irrefutable_pat(p))
		    {
			expression_ref f  = AST_node("Lambda") + p + E2;
			E2 = {AST_node("id","concatMap"),f,l};
		    }
		    else
		    {
			// Problem: "ok" needs to be a fresh variable.
			expression_ref ok = get_fresh_id("ok",E);

			expression_ref lhs1 = AST_node("funlhs1") + ok + p;
			expression_ref rhs1 = AST_node("rhs") + E2;
			expression_ref decl1 = AST_node("Decl") + lhs1 + rhs1;

			expression_ref lhs2 = AST_node("funlhs1") + ok + AST_node("WildcardPattern");
			expression_ref rhs2 = AST_node("rhs") + AST_node("id","[]");
			expression_ref decl2 = AST_node("Decl") + lhs2 + rhs2;

			expression_ref decls = AST_node("Decls") + decl1 + decl2;
			expression_ref body = {AST_node("id","concatMap"),ok,l};

			E2 = AST_node("Let") + decls + body;
		    }
		}
		else if (is_AST(B, "LetQual"))
		    E2 = AST_node("Let") + B.sub()[0] + E2;
	    }
	    return rename(m,E2,bound);
	}
	else if (n.type == "Lambda")
	{
	    // FIXME: Try to preserve argument names (in block_case( ), probably) when they are irrefutable apat_var's.

	    // 1. Extract the lambda body
	    expression_ref body = v.back();
	    v.pop_back();

	    // 2. Find bound vars and convert vars to dummies.
	    set<string> bound2 = bound;
	    for(auto& e: v) {
		add(bound2, find_bound_vars(e));
		e = rename(m, e, bound2);
	    }

	    // 3. Rename the body, binding vars mentioned in the lambda patterns.
	    body = rename(m, body, bound2);

	    v.push_back(body);
	    return expression_ref{E.head(),v};
	}
	else if (n.type == "Do")
	{
	    assert(is_AST(E.sub()[0],"Stmts"));
	    vector<expression_ref> stmts = E.sub()[0].sub();

	    // do { e }  =>  e
	    if (stmts.size() == 1)
		return rename(m,stmts[0],bound);

	    expression_ref first = stmts[0];
	    stmts.erase(stmts.begin());
	    expression_ref do_stmts = AST_node("Do") + expression_ref(AST_node("Stmts"),stmts);
	    expression_ref result;

	    // do {e ; stmts }  =>  e >> do { stmts }
	    if (is_AST(first,"SimpleStmt"))
	    {
		expression_ref e = first.sub()[0];
		expression_ref qop = AST_node("id",">>");
		result = AST_node("infixexp") + e + qop + do_stmts;
	    }

	    // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
	    // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
	    else if (is_AST(first,"PatStmt"))
	    {
		expression_ref p = first.sub()[0];
		expression_ref e = first.sub()[1];
		expression_ref qop = AST_node("id",">>=");

		if (is_irrefutable_pat(p))
		{
		    expression_ref lambda = AST_node("Lambda") + p + do_stmts;
		    result = AST_node("infixexp") + e + qop + lambda;
		}
		else
		{
		    expression_ref fail = {AST_node("id","fail"),"Fail!"};
		    expression_ref ok = get_fresh_id("ok",E);

		    expression_ref lhs1 = AST_node("funlhs1") + ok + p;
		    expression_ref rhs1 = AST_node("rhs") + do_stmts;
		    expression_ref decl1 = AST_node("Decl") + lhs1 + rhs1;

		    expression_ref lhs2 = AST_node("funlhs1") + ok + AST_node("WildcardPattern");
		    expression_ref rhs2 = AST_node("rhs") + fail;
		    expression_ref decl2 = AST_node("Decl") + lhs2 + rhs2;
		    expression_ref decls = AST_node("Decls") + decl1 +  decl2;

		    expression_ref body = AST_node("infixexp") + e + qop + ok;

		    result = AST_node("Let") + decls + body;
		}
	    }
	    // do {let decls ; rest} = let decls in do {stmts}
	    else if (is_AST(first,"LetStmt"))
	    {
		expression_ref decls = first.sub()[0];
		result = AST_node("Let") + decls + do_stmts;
	    }
	    else if (is_AST(first,"EmptyStmt"))
		result = do_stmts;

	    return rename(m,result,bound);
	}
	else if (n.type == "If")
	{
	    for(auto& e: v)
		e = rename(m, e, bound);

	    return case_expression(v[0],true,v[1],v[2]);
	}
	else if (n.type == "LeftSection")
	{
	    // FIXME... the infixexp needs to parse the same as if it was parenthesized.
	    // FIXME... probably we need to do a disambiguation on the infix expression. (infixexp op x)
	    std::set<var> free_vars;
	    for(auto& e: v) {
		e = rename(m, e, bound);
		add(free_vars, get_free_indices(e));
	    }
	    return apply_expression(v[1],v[0]);
	}
	else if (n.type == "RightSection")
	{
	    // FIXME... probably we need to do a disambiguation on the infix expression. (x op infixexp)
	    // FIXME... the infixexp needs to parse the same as if it was parenthesized.
	    std::set<var> free_vars;
	    for(auto& e: v) {
		e = rename(m, e, bound);
		add(free_vars, get_free_indices(e));
	    }
	    int safe_var_index = 0;
	    if (not free_vars.empty())
		safe_var_index = max_index(free_vars)+1;
	    var vsafe(safe_var_index);
	    return lambda_quantify(vsafe,apply_expression(apply_expression(v[0],vsafe),v[1]));
	}
	else if (n.type == "Let")
	{
	    expression_ref decls_ = v[0];
	    assert(is_AST(decls_,"Decls"));
	    expression_ref body = v[1];

	    // transform "let (a,b) = E in F" => "case E of (a,b) -> F"
	    {
		expression_ref decl = decls_.sub()[0];
		if (is_AST(decl,"Decl"))
		{
		    expression_ref pat = decl.sub()[0];
		    expression_ref rhs = decl.sub()[1];
		    // let pat = rhs in body -> case rhs of {pat->body}
		    if (is_AST(pat,"pat"))
		    {
			assert(is_AST(rhs,"rhs"));
			expression_ref pat0 = pat.sub()[0];
			if (is_AST(pat0,"Tuple"))
			{
			    if (decls_.size() != 1) throw myexception()<<"Can't currently handle pattern let with more than one decl.";
			    expression_ref alt = AST_node("alt") + pat + body;
			    expression_ref alts = AST_node("alts") + alt;
			    expression_ref EE = AST_node("Case") + rhs.sub()[0] + alts;
			    return rename(m, EE, bound);
			}
		    }
		}
	    }

	    // parse the decls and bind declared names internally to the decls.
	    v[0] = rename(m, v[0], bound);

	    vector<pair<var,expression_ref>> decls;

	    // find the bound var names + construct arguments to let_obj()
	    set<string> bound2 = bound;
	    for(const auto& decl: v[0].sub())
	    {
		if (is_AST(decl,"EmptyDecl")) continue;

		var x = decl.sub()[0].as_<var>().name;
		auto E = decl.sub()[1];

		decls.push_back({x,E});
		bound2.insert(x.name);
	    }

	    // finally rename let-body, now that we know the bound vars.
	    body = rename(m, body, bound2);

	    // construct the new let expression.
	    return let_expression(decls, body);
	}
	else if (n.type == "Case")
	{
	    expression_ref case_obj = rename(m, v[0], bound);
	    vector<expression_ref> alts = v[1].sub();
	    vector<expression_ref> patterns;
	    vector<expression_ref> bodies;
	    for(const auto& alt: alts)
	    {
		set<string> bound2 = bound;
		add(bound2, find_bound_vars(alt.sub()[0]));
		patterns.push_back(rename(m, alt.sub()[0], bound2) );

		// Handle where-clause.
		assert(alt.size() == 2 or alt.size() == 3);
		expression_ref body = alt.sub()[1];

		if (is_AST(body,"GdPat"))
		    throw myexception()<<"Guard patterns not yet implemented!";

		if (alt.size() == 3)
		{
		    assert(is_AST(alt.sub()[2],"Decls"));
		    body = AST_node("Let") + alt.sub()[2] + body;
		}

		bodies.push_back(rename(m, body, bound2) );
	    }
	    return case_expression(case_obj, patterns, bodies);
	}
    }

    for(auto& e: v)
	e = rename(m, e, bound);
    if (E.size())
	return expression_ref{E.head(),v};
    else
	return E;
}

expression_ref desugar(const Module& m, const expression_ref& E)
{
    vector<expression_ref> v;
    if (E.is_expression())
	v = E.sub();

    if (E.head().is_a<AST_node>())
    {
	auto& n = E.head().as_<AST_node>();
	if (n.type == "infixexp")
	    std::abort();
	else if (n.type == "Decls" or n.type == "TopDecls")
	{
	    // translate each individual decl
	    for(auto& e: v)
		e = desugar(m, e);

	    // Convert fundecls to normal decls
	    vector<expression_ref> decls = parse_fundecls(v);

	    return expression_ref{E.head(),decls};
	}
	else if (n.type == "Decl")
	{
	    auto& lhs = E.sub()[0];
	    auto& f = lhs.head();
	    if (f.is_a<constructor>())
	    {
		// Pattern x y z = E
		//      to
		// fresh = E
		// x = case fresh of Pattern x y z
		// y = case fresh of Pattern x y z
		// z = case fresh of Pattern x y z
	    }

            // Replace bound vars in (a) the patterns and (b) the body
	    for(auto& e: v)
		e = desugar(m, e);

	    return expression_ref{E.head(),v};
	}
	else if (n.type == "rhs")
	{
	    if (E.size() == 2) // where decls
	    {
		expression_ref decls = E.sub()[1];
		assert(is_AST(decls,"Decls"));
		expression_ref E2 = AST_node("Let") + decls + E.sub()[0];
		E2 = AST_node("rhs") + E2;
		return desugar(m,E2);
	    }
	    else
	    { }      // Fall through and let the standard case handle this.
	}
	else if (n.type == "WildcardPattern")
	{
	    return var(-1);
	}
	else if (n.type == "id")
	    std::abort();
	else if (n.type == "ListComprehension")
	{
	    expression_ref E2 = E;
	    // [ e | True   ]  =  [ e ]
	    // [ e | q      ]  =  [ e | q, True ]
	    // [ e | b, Q   ]  =  if b then [ e | Q ] else []
	    // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
	    // [ e | let decls, Q] = let decls in [ e | Q ]

	    expression_ref True = AST_node("SimpleQual") + bool_true;

	    assert(v.size() >= 2);
	    if (v.size() == 2 and (v[1] == True))
		E2 = List(v[0]);
	    else if (v.size() == 2)
		E2 = E.head() + v[0] + v[1] + True;
	    else
	    {
		expression_ref B = v[1];
		v.erase(v.begin()+1);
		E2 = expression_ref{E.head(),v};

		if (is_AST(B, "SimpleQual"))
		    E2 = AST_node("If") + B.sub()[0] + E2 + AST_node("id","[]");
		else if (is_AST(B, "PatQual"))
		{
		    expression_ref p = B.sub()[0];
		    expression_ref l = B.sub()[1];
		    if (is_irrefutable_pat(p))
		    {
			expression_ref f  = AST_node("Lambda") + p + E2;
			E2 = {AST_node("id","concatMap"),f,l};
		    }
		    else
		    {
			// Problem: "ok" needs to be a fresh variable.
			expression_ref ok = get_fresh_id("ok",E);

			expression_ref lhs1 = AST_node("funlhs1") + ok + p;
			expression_ref rhs1 = AST_node("rhs") + E2;
			expression_ref decl1 = AST_node("Decl") + lhs1 + rhs1;

			expression_ref lhs2 = AST_node("funlhs1") + ok + AST_node("WildcardPattern");
			expression_ref rhs2 = AST_node("rhs") + AST_node("id","[]");
			expression_ref decl2 = AST_node("Decl") + lhs2 + rhs2;

			expression_ref decls = AST_node("Decls") + decl1 + decl2;
			expression_ref body = {AST_node("id","concatMap"),ok,l};

			E2 = AST_node("Let") + decls + body;
		    }
		}
		else if (is_AST(B, "LetQual"))
		    E2 = AST_node("Let") + B.sub()[0] + E2;
	    }
	    return desugar(m,E2);
	}
	else if (n.type == "Lambda")
	{
	    // FIXME: Try to preserve argument names (in block_case( ), probably) when they are irrefutable apat_var's.

	    // 1. Extract the lambda body
	    expression_ref body = v.back();
	    v.pop_back();

	    // 2. Find bound vars and convert vars to dummies.
	    for(auto& e: v)
		e = desugar(m, e);

	    // 3. Desugar the body, binding vars mentioned in the lambda patterns.
	    body = desugar(m, body);

	    return def_function({v},{body}); 
	}
	else if (n.type == "Do")
	{
	    assert(is_AST(E.sub()[0],"Stmts"));
	    vector<expression_ref> stmts = E.sub()[0].sub();

	    // do { e }  =>  e
	    if (stmts.size() == 1) 
		return desugar(m,stmts[0]);

	    expression_ref first = stmts[0];
	    stmts.erase(stmts.begin());
	    expression_ref do_stmts = AST_node("Do") + expression_ref(AST_node("Stmts"),stmts);
	    expression_ref result;
      
	    // do {e ; stmts }  =>  e >> do { stmts }
	    if (is_AST(first,"SimpleStmt"))
	    {
		expression_ref e = first.sub()[0];
		expression_ref qop = AST_node("id",">>");
		result = AST_node("infixexp") + e + qop + do_stmts;
	    }

	    // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
	    // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
	    else if (is_AST(first,"PatStmt"))
	    {
		expression_ref p = first.sub()[0];
		expression_ref e = first.sub()[1];
		expression_ref qop = AST_node("id",">>=");

		if (is_irrefutable_pat(p))
		{
		    expression_ref lambda = AST_node("Lambda") + p + do_stmts;
		    result = AST_node("infixexp") + e + qop + lambda;
		}
		else
		{
		    expression_ref fail = {AST_node("id","fail"),"Fail!"};
		    expression_ref ok = get_fresh_id("ok",E);
	  
		    expression_ref lhs1 = AST_node("funlhs1") + ok + p;
		    expression_ref rhs1 = AST_node("rhs") + do_stmts;
		    expression_ref decl1 = AST_node("Decl") + lhs1 + rhs1;
	  
		    expression_ref lhs2 = AST_node("funlhs1") + ok + AST_node("WildcardPattern");
		    expression_ref rhs2 = AST_node("rhs") + fail;
		    expression_ref decl2 = AST_node("Decl") + lhs2 + rhs2;
		    expression_ref decls = AST_node("Decls") + decl1 +  decl2;

		    expression_ref body = AST_node("infixexp") + e + qop + ok;

		    result = AST_node("Let") + decls + body;
		}
	    }
	    // do {let decls ; rest} = let decls in do {stmts}
	    else if (is_AST(first,"LetStmt"))
	    {
		expression_ref decls = first.sub()[0];
		result = AST_node("Let") + decls + do_stmts;
	    }
	    else if (is_AST(first,"EmptyStmt"))
		result = do_stmts;

	    return desugar(m,result);
	}
	else if (n.type == "If")
	{
	    for(auto& e: v)
		e = desugar(m, e);

	    return case_expression(v[0],true,v[1],v[2]);
	}
	else if (n.type == "LeftSection")
	{
	    // FIXME... the infixexp needs to parse the same as if it was parenthesized.
	    // FIXME... probably we need to do a disambiguation on the infix expression. (infixexp op x)
	    std::set<var> free_vars;
	    for(auto& e: v) {
		e = desugar(m, e);
		add(free_vars, get_free_indices(e));
	    }
	    return apply_expression(v[1],v[0]);
	}
	else if (n.type == "RightSection")
	{
	    // FIXME... probably we need to do a disambiguation on the infix expression. (x op infixexp)
	    // FIXME... the infixexp needs to parse the same as if it was parenthesized.
	    std::set<var> free_vars;
	    for(auto& e: v) {
		e = desugar(m, e);
		add(free_vars, get_free_indices(e));
	    }
	    int safe_var_index = 0;
	    if (not free_vars.empty())
		safe_var_index = max_index(free_vars)+1;
	    var vsafe(safe_var_index);
	    return lambda_quantify(vsafe,apply_expression(apply_expression(v[0],vsafe),v[1]));
	}
	else if (n.type == "Let")
	{
	    expression_ref decls_ = v[0];
	    assert(is_AST(decls_,"Decls"));
	    expression_ref body = v[1];

	    // transform "let (a,b) = E in F" => "case E of (a,b) -> F"
	    {
		expression_ref decl = decls_.sub()[0];
		if (is_AST(decl,"Decl"))
		{
		    expression_ref pat = decl.sub()[0];
		    expression_ref rhs = decl.sub()[1];
		    // let pat = rhs in body -> case rhs of {pat->body}
		    if (is_AST(pat,"pat"))
		    {
			assert(is_AST(rhs,"rhs"));
			expression_ref pat0 = pat.sub()[0];
			if (is_AST(pat0,"Tuple"))
			{
			    if (decls_.size() != 1) throw myexception()<<"Can't currently handle pattern let with more than one decl.";
			    expression_ref alt = AST_node("alt") + pat + body;
			    expression_ref alts = AST_node("alts") + alt;
			    expression_ref EE = AST_node("Case") + rhs.sub()[0] + alts;
			    return desugar(m, EE);
			}
		    }
		}
	    }

	    // parse the decls and bind declared names internally to the decls.
	    v[0] = desugar(m, v[0]);

	    vector<pair<var,expression_ref>> decls;

	    // find the bound var names + construct arguments to let_obj()
	    for(const auto& decl: v[0].sub())
	    {
		if (is_AST(decl,"EmptyDecl")) continue;

		var x = decl.sub()[0].as_<var>().name;
		auto E = decl.sub()[1];

		decls.push_back({x,E});
	    }

	    // finally desugar let-body, now that we know the bound vars.
	    body = desugar(m, body);

	    // construct the new let expression.
	    return let_expression(decls, body);
	}
	else if (n.type == "Case")
	{
	    expression_ref case_obj = desugar(m, v[0]);
	    vector<expression_ref> alts = v[1].sub();
	    vector<expression_ref> patterns;
	    vector<expression_ref> bodies;
	    for(const auto& alt: alts)
	    {
		patterns.push_back(desugar(m, alt.sub()[0]) );

		// Handle where-clause.
		assert(alt.size() == 2 or alt.size() == 3);
		expression_ref body = alt.sub()[1];

		if (is_AST(body,"GdPat"))
		    throw myexception()<<"Guard patterns not yet implemented!";

		if (alt.size() == 3)
		{
		    assert(is_AST(alt.sub()[2],"Decls"));
		    body = AST_node("Let") + alt.sub()[2] + body;
		}

		bodies.push_back(desugar(m, body) );
	    }
	    return case_expression(case_obj, patterns, bodies);
	}
	else if (n.type == "enumFrom")
	{
	    expression_ref E2 = AST_node("id","enumFrom");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(m, E2);
	}
	else if (n.type == "enumFromTo")
	{
	    expression_ref E2 = AST_node("id","enumFromTo");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(m, E2);
	}
    }

    for(auto& e: v)
	e = desugar(m, e);
    if (E.size())
	return expression_ref{E.head(),v};
    else
	return E;
}

expression_ref parse_haskell_line(const Module& P, const string& line)
{
    return desugar(P, parse_haskell_line(line));
}

bool is_all_space(const string& line)
{
    for(int i=0;i<line.size();i++)
	if (not ::isspace(line[i])) return false;
    return true;
}

Module read_model(const string& filename)
{
    // 1. Read module
    return module_loader({}).load_module_from_file(filename);
}

void read_add_model(Model& M, const std::string& filename)
{
    auto m = read_model(filename);
    M += m;
    add_model(M, m.name);
}

void add_model(Model& M, const std::string& name)
{
    M += name;
    string prefix = name;
    M.perform_expression({var("Distributions.do_log"),prefix,{var("Distributions.gen_model"),var(name+".main")}});
}
