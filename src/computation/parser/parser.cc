// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.





#include "parser.hh"


// Unqualified %code blocks.
#line 55 "parser.y"

# include "driver.hh"

#line 50 "parser.cc"


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (false)
# endif


// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yy_stack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YY_USE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

namespace yy {
#line 142 "parser.cc"

  /// Build a parser object.
  parser::parser (driver& drv_yyarg)
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr),
#else
    :
#endif
      drv (drv_yyarg)
  {}

  parser::~parser ()
  {}

  parser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------.
  | symbol.  |
  `---------*/



  // by_state.
  parser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  parser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  parser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  parser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  parser::symbol_kind_type
  parser::by_state::kind () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST (symbol_kind_type, yystos_[+state]);
  }

  parser::stack_symbol_type::stack_symbol_type ()
  {}

  parser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.YY_MOVE_OR_COPY< Hs::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.YY_MOVE_OR_COPY< Hs::ConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.YY_MOVE_OR_COPY< Hs::ConstructorsDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.YY_MOVE_OR_COPY< Hs::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.YY_MOVE_OR_COPY< Hs::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
      case symbol_kind::S_decllist: // decllist
        value.YY_MOVE_OR_COPY< Hs::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
        value.YY_MOVE_OR_COPY< Hs::Export > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.YY_MOVE_OR_COPY< Hs::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.YY_MOVE_OR_COPY< Hs::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.YY_MOVE_OR_COPY< Hs::GADTConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.YY_MOVE_OR_COPY< Hs::GADTConstructorsDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.YY_MOVE_OR_COPY< Hs::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Hs::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.YY_MOVE_OR_COPY< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.YY_MOVE_OR_COPY< Hs::Module > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.YY_MOVE_OR_COPY< Hs::MultiGuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.YY_MOVE_OR_COPY< Hs::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.YY_MOVE_OR_COPY< Hs::Type > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.YY_MOVE_OR_COPY< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.YY_MOVE_OR_COPY< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Hs::Binds> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.YY_MOVE_OR_COPY< Located<Hs::Decls> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.YY_MOVE_OR_COPY< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.YY_MOVE_OR_COPY< Located<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcname: // qcname
        value.YY_MOVE_OR_COPY< Located<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.YY_MOVE_OR_COPY< bool > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.YY_MOVE_OR_COPY< char > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.YY_MOVE_OR_COPY< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.YY_MOVE_OR_COPY< integer > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.YY_MOVE_OR_COPY< std::optional<Hs::GADTConstructorsDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.YY_MOVE_OR_COPY< std::optional<Hs::Kind> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::Export>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Hs::Context,Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.YY_MOVE_OR_COPY< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.YY_MOVE_OR_COPY< std::vector<Hs::Export> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.YY_MOVE_OR_COPY< std::vector<Hs::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::LVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.YY_MOVE_OR_COPY< std::vector<Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeCon> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeFamilyInstanceEqn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Hs::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.YY_MOVE_OR_COPY< std::vector<Located<expression_ref>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.YY_MOVE_OR_COPY< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
        value.YY_MOVE_OR_COPY< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ops: // ops
        value.YY_MOVE_OR_COPY< std::vector<std::string> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  parser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.move< Hs::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.move< Hs::ConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< Hs::ConstructorsDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Hs::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Hs::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
      case symbol_kind::S_decllist: // decllist
        value.move< Hs::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
        value.move< Hs::Export > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Hs::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.move< Hs::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.move< Hs::GADTConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.move< Hs::GADTConstructorsDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Hs::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.move< Hs::Module > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.move< Hs::MultiGuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Hs::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.move< Hs::Type > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Hs::Binds> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.move< Located<Hs::Decls> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.move< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.move< Located<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcname: // qcname
        value.move< Located<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.move< bool > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.move< char > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.move< integer > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Hs::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.move< std::optional<Hs::GADTConstructorsDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.move< std::optional<Hs::Kind> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.move< std::optional<Located<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Hs::Export>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.move< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Hs::Export> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::LVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.move< std::vector<Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.move< std::vector<Hs::TypeCon> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.move< std::vector<Hs::TypeFamilyInstanceEqn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Hs::TypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Hs::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<Located<expression_ref>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
        value.move< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ops: // ops
        value.move< std::vector<std::string> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.copy< Hs::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.copy< Hs::ConstructorDecl > (that.value);
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.copy< Hs::ConstructorsDecl > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.copy< Hs::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.copy< Hs::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
      case symbol_kind::S_decllist: // decllist
        value.copy< Hs::Decls > (that.value);
        break;

      case symbol_kind::S_export: // export
        value.copy< Hs::Export > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.copy< Hs::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.copy< Hs::Fixity > (that.value);
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.copy< Hs::GADTConstructorDecl > (that.value);
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.copy< Hs::GADTConstructorsDecl > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.copy< Hs::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Hs::ImpDecl > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.copy< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.copy< Hs::Module > (that.value);
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.copy< Hs::MultiGuardedRHS > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.copy< Hs::Stmts > (that.value);
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.copy< Hs::Type > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.copy< Hs::TypeFamilyInstanceEqn > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.copy< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.copy< Located<Hs::Binds> > (that.value);
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.copy< Located<Hs::Decls> > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.copy< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.copy< Located<expression_ref> > (that.value);
        break;

      case symbol_kind::S_qcname: // qcname
        value.copy< Located<std::string> > (that.value);
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.copy< bool > (that.value);
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.copy< char > (that.value);
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.copy< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.copy< integer > (that.value);
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.copy< std::optional<Hs::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.copy< std::optional<Hs::GADTConstructorsDecl> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.copy< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.copy< std::optional<Hs::Kind> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.copy< std::optional<Located<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.copy< std::optional<Located<Hs::Kind>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.copy< std::optional<std::vector<Hs::Export>> > (that.value);
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.copy< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Hs::Context,Hs::Type> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.copy< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.copy< std::string > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.copy< std::vector<Hs::Export> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.copy< std::vector<Hs::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<Hs::LVar> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.copy< std::vector<Hs::Type> > (that.value);
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.copy< std::vector<Hs::TypeCon> > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.copy< std::vector<Hs::TypeFamilyInstanceEqn> > (that.value);
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.copy< std::vector<Hs::TypeVar> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Hs::Alt>> > (that.value);
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.copy< std::vector<Located<expression_ref>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.copy< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
        value.copy< std::vector<expression_ref> > (that.value);
        break;

      case symbol_kind::S_ops: // ops
        value.copy< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    return *this;
  }

  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.move< Hs::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.move< Hs::ConstructorDecl > (that.value);
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< Hs::ConstructorsDecl > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Hs::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Hs::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
      case symbol_kind::S_decllist: // decllist
        value.move< Hs::Decls > (that.value);
        break;

      case symbol_kind::S_export: // export
        value.move< Hs::Export > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Hs::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.move< Hs::Fixity > (that.value);
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.move< Hs::GADTConstructorDecl > (that.value);
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.move< Hs::GADTConstructorsDecl > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Hs::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::ImpDecl > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.move< Hs::Module > (that.value);
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.move< Hs::MultiGuardedRHS > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Hs::Stmts > (that.value);
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.move< Hs::Type > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Hs::Binds> > (that.value);
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.move< Located<Hs::Decls> > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.move< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.move< Located<expression_ref> > (that.value);
        break;

      case symbol_kind::S_qcname: // qcname
        value.move< Located<std::string> > (that.value);
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.move< bool > (that.value);
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.move< char > (that.value);
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.move< integer > (that.value);
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Hs::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.move< std::optional<Hs::GADTConstructorsDecl> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.move< std::optional<Hs::Kind> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.move< std::optional<Located<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Hs::Export>> > (that.value);
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::Type> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.move< std::string > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Hs::Export> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::LVar> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.move< std::vector<Hs::Type> > (that.value);
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.move< std::vector<Hs::TypeCon> > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.move< std::vector<Hs::TypeFamilyInstanceEqn> > (that.value);
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Hs::TypeVar> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Hs::Alt>> > (that.value);
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<Located<expression_ref>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
        value.move< std::vector<expression_ref> > (that.value);
        break;

      case symbol_kind::S_ops: // ops
        value.move< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
  parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);
  }

#if YYDEBUG
  template <typename Base>
  void
  parser::yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YY_USE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " ("
            << yysym.location << ": ";
        YY_USE (yykind);
        yyo << ')';
      }
  }
#endif

  void
  parser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  parser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  parser::yypop_ (int n) YY_NOEXCEPT
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  parser::debug_level_type
  parser::debug_level () const
  {
    return yydebug_;
  }

  void
  parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  parser::state_type
  parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool
  parser::yy_pact_value_is_default_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  parser::yy_table_value_is_error_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yytable_ninf_;
  }

  int
  parser::operator() ()
  {
    return parse ();
  }

  int
  parser::parse ()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';
    YY_STACK_PRINT ();

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[+yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            symbol_type yylookahead (yylex (drv));
            yyla.move (yylookahead);
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    if (yyla.kind () == symbol_kind::S_YYerror)
    {
      // The scanner already issued an error message, process directly
      // to error recovery.  But do not keep the error token as
      // lookahead, it is too special and may lead us to an endless
      // loop in error recovery. */
      yyla.kind_ = symbol_kind::S_YYUNDEF;
      goto yyerrlab1;
    }

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.kind ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[+yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* Variants are always initialized to an empty instance of the
         correct type. The default '$$ = $1' action is NOT applied
         when using variants.  */
      switch (yyr1_[yyn])
    {
      case symbol_kind::S_altslist: // altslist
        yylhs.value.emplace< Hs::Alts > ();
        break;

      case symbol_kind::S_constr: // constr
        yylhs.value.emplace< Hs::ConstructorDecl > ();
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        yylhs.value.emplace< Hs::ConstructorsDecl > ();
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        yylhs.value.emplace< Hs::Context > ();
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        yylhs.value.emplace< Hs::DataOrNewtype > ();
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
      case symbol_kind::S_decllist: // decllist
        yylhs.value.emplace< Hs::Decls > ();
        break;

      case symbol_kind::S_export: // export
        yylhs.value.emplace< Hs::Export > ();
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        yylhs.value.emplace< Hs::FieldDecl > ();
        break;

      case symbol_kind::S_infix: // infix
        yylhs.value.emplace< Hs::Fixity > ();
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        yylhs.value.emplace< Hs::GADTConstructorDecl > ();
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        yylhs.value.emplace< Hs::GADTConstructorsDecl > ();
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        yylhs.value.emplace< Hs::GuardedRHS > ();
        break;

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Hs::ImpDecl > ();
        break;

      case symbol_kind::S_impspec: // impspec
        yylhs.value.emplace< Hs::ImpSpec > ();
        break;

      case symbol_kind::S_module: // module
        yylhs.value.emplace< Hs::Module > ();
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        yylhs.value.emplace< Hs::MultiGuardedRHS > ();
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        yylhs.value.emplace< Hs::Stmts > ();
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        yylhs.value.emplace< Hs::Type > ();
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        yylhs.value.emplace< Hs::TypeFamilyInstanceEqn > ();
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        yylhs.value.emplace< Hs::TypeVar > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Hs::Alt> > ();
        break;

      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Hs::Binds> > ();
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        yylhs.value.emplace< Located<Hs::Decls> > ();
        break;

      case symbol_kind::S_infixexp: // infixexp
        yylhs.value.emplace< Located<Hs::InfixExp> > ();
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        yylhs.value.emplace< Located<expression_ref> > ();
        break;

      case symbol_kind::S_qcname: // qcname
        yylhs.value.emplace< Located<std::string> > ();
        break;

      case symbol_kind::S_optqualified: // optqualified
        yylhs.value.emplace< bool > ();
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        yylhs.value.emplace< char > ();
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        yylhs.value.emplace< double > ();
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< expression_ref > ();
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        yylhs.value.emplace< float > ();
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        yylhs.value.emplace< integer > ();
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        yylhs.value.emplace< std::optional<Hs::ExportSubSpec> > ();
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        yylhs.value.emplace< std::optional<Hs::GADTConstructorsDecl> > ();
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        yylhs.value.emplace< std::optional<Hs::ImpSpec> > ();
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        yylhs.value.emplace< std::optional<Hs::Kind> > ();
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Hs::Binds>> > ();
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        yylhs.value.emplace< std::optional<Located<Hs::Decls>> > ();
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        yylhs.value.emplace< std::optional<Located<Hs::Kind>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        yylhs.value.emplace< std::optional<std::vector<Hs::Export>> > ();
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        yylhs.value.emplace< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Hs::Context,Hs::Type> > ();
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        yylhs.value.emplace< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        yylhs.value.emplace< std::string > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        yylhs.value.emplace< std::vector<Hs::Export> > ();
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Hs::FieldDecl> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Hs::GuardedRHS> > ();
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        yylhs.value.emplace< std::vector<Hs::ImpDecl> > ();
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<Hs::LVar> > ();
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        yylhs.value.emplace< std::vector<Hs::Type> > ();
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        yylhs.value.emplace< std::vector<Hs::TypeCon> > ();
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        yylhs.value.emplace< std::vector<Hs::TypeFamilyInstanceEqn> > ();
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        yylhs.value.emplace< std::vector<Hs::TypeVar> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Hs::Alt>> > ();
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        yylhs.value.emplace< std::vector<Located<expression_ref>> > ();
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        yylhs.value.emplace< std::vector<Located<std::string>> > ();
        break;

      case symbol_kind::S_decls: // decls
        yylhs.value.emplace< std::vector<expression_ref> > ();
        break;

      case symbol_kind::S_ops: // ops
        yylhs.value.emplace< std::vector<std::string> > ();
        break;

      default:
        break;
    }


      // Default location.
      {
        stack_type::slice range (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, range, yylen);
        yyerror_range[1].location = yylhs.location;
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
  case 2: // unit: module
#line 503 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2411 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 520 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2417 "parser.cc"
    break;

  case 4: // module: body2
#line 521 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2423 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 523 "parser.y"
                                                                 {drv.push_module_context();}
#line 2429 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 531 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2435 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 532 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2441 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 534 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2447 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 535 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2453 "parser.cc"
    break;

  case 13: // top: semis top1
#line 538 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2459 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 540 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2465 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 541 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2471 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 542 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2477 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 550 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2483 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 551 "parser.y"
                                      {}
#line 2489 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 553 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2495 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 555 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2501 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 556 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2507 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 558 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2513 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 559 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2519 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 561 "parser.y"
                                      {}
#line 2525 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 562 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2531 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 563 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2537 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 565 "parser.y"
                   {}
#line 2543 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 566 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2549 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 568 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2555 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2561 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 571 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2567 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 572 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2573 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 582 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2579 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 584 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2585 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 585 "parser.y"
                         { }
#line 2591 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 587 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2599 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 600 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2605 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 601 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2611 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 603 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2617 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 604 "parser.y"
                               { }
#line 2623 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 606 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2629 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 607 "parser.y"
                               { }
#line 2635 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 611 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2641 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 612 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2647 "parser.cc"
    break;

  case 49: // prec: %empty
#line 617 "parser.y"
                   { }
#line 2653 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 618 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2659 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 620 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2665 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 621 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2671 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 622 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2677 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 624 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2683 "parser.cc"
    break;

  case 55: // ops: op
#line 625 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2689 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 629 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2695 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 631 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2701 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 632 "parser.y"
                                            { }
#line 2707 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 634 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2713 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 635 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2719 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 636 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2725 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 637 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2731 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 640 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2737 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 641 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2743 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 648 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2749 "parser.cc"
    break;

  case 66: // topdecl: infixexp
#line 650 "parser.y"
                                               {yylhs.value.as < expression_ref > () = unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ());}
#line 2755 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 652 "parser.y"
                                              {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ());}
#line 2761 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 654 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location, yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location, yystack_[0].value.as < Hs::Type > ()});}
#line 2767 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 655 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ());}
#line 2773 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 657 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ());}
#line 2779 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 658 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_type_family({yystack_[3].location,yystack_[3].value.as < Hs::Type > ()}, yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ());}
#line 2785 "parser.cc"
    break;

  case 72: // standalone_kind_sig: "type" sks_vars "::" kind
#line 661 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::TypeCon> > (),yystack_[0].value.as < expression_ref > ());}
#line 2791 "parser.cc"
    break;

  case 73: // sks_vars: sks_vars "," oqtycon
#line 663 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::TypeCon> > () = yystack_[2].value.as < std::vector<Hs::TypeCon> > (); yylhs.value.as < std::vector<Hs::TypeCon> > ().push_back(Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()})); }
#line 2797 "parser.cc"
    break;

  case 74: // sks_vars: oqtycon
#line 664 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::TypeCon> > () = {Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()})}; }
#line 2803 "parser.cc"
    break;

  case 75: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 667 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ());}
#line 2809 "parser.cc"
    break;

  case 76: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 668 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2815 "parser.cc"
    break;

  case 91: // where_type_family: %empty
#line 703 "parser.y"
                                                           {}
#line 2821 "parser.cc"
    break;

  case 92: // where_type_family: "where" ty_fam_inst_eqn_list
#line 704 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2827 "parser.cc"
    break;

  case 93: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 706 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2833 "parser.cc"
    break;

  case 94: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 707 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2839 "parser.cc"
    break;

  case 95: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 708 "parser.y"
                                                           {}
#line 2845 "parser.cc"
    break;

  case 96: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 709 "parser.y"
                                                           {}
#line 2851 "parser.cc"
    break;

  case 97: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 711 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2857 "parser.cc"
    break;

  case 98: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 712 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2863 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 713 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 2869 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqns: %empty
#line 714 "parser.y"
                                                           {}
#line 2875 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqn: type "=" ctype
#line 716 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn({yystack_[2].location,yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location,yystack_[0].value.as < Hs::Type > ()});}
#line 2881 "parser.cc"
    break;

  case 102: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 719 "parser.y"
                                                               {}
#line 2887 "parser.cc"
    break;

  case 103: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 721 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {}); }
#line 2893 "parser.cc"
    break;

  case 104: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 723 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {}); }
#line 2899 "parser.cc"
    break;

  case 105: // at_decl_cls: "type" ty_fam_inst_eqn
#line 725 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2905 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 726 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2911 "parser.cc"
    break;

  case 111: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 734 "parser.y"
                                                              { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2917 "parser.cc"
    break;

  case 112: // data_or_newtype: "data"
#line 736 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2923 "parser.cc"
    break;

  case 113: // data_or_newtype: "newtype"
#line 737 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2929 "parser.cc"
    break;

  case 114: // opt_kind_sig: %empty
#line 741 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 2935 "parser.cc"
    break;

  case 115: // opt_kind_sig: "::" kind
#line 742 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 2941 "parser.cc"
    break;

  case 118: // opt_tyfam_kind_sig: %empty
#line 747 "parser.y"
                                      {}
#line 2947 "parser.cc"
    break;

  case 119: // opt_tyfam_kind_sig: "::" kind
#line 748 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2953 "parser.cc"
    break;

  case 120: // opt_tyfam_kind_sig: "=" tv_bndr
#line 749 "parser.y"
                                      {}
#line 2959 "parser.cc"
    break;

  case 121: // opt_at_kind_inj_sig: %empty
#line 751 "parser.y"
                                      {}
#line 2965 "parser.cc"
    break;

  case 122: // opt_at_kind_inj_sig: "::" kind
#line 752 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2971 "parser.cc"
    break;

  case 123: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 753 "parser.y"
                                                                  {}
#line 2977 "parser.cc"
    break;

  case 124: // tycl_hdr: context "=>" type
#line 757 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 2983 "parser.cc"
    break;

  case 125: // tycl_hdr: type
#line 758 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 2989 "parser.cc"
    break;

  case 129: // decl_cls: at_decl_cls
#line 804 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2995 "parser.cc"
    break;

  case 130: // decl_cls: decl
#line 805 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3001 "parser.cc"
    break;

  case 131: // decls_cls: decls_cls ";" decl_cls
#line 807 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3007 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";"
#line 808 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3013 "parser.cc"
    break;

  case 133: // decls_cls: decl_cls
#line 809 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3019 "parser.cc"
    break;

  case 134: // decls_cls: %empty
#line 810 "parser.y"
                                           {}
#line 3025 "parser.cc"
    break;

  case 135: // decllist_cls: "{" decls_cls "}"
#line 812 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3031 "parser.cc"
    break;

  case 136: // decllist_cls: "vocurly" decls_cls close
#line 813 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3037 "parser.cc"
    break;

  case 137: // where_cls: "where" decllist_cls
#line 815 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3043 "parser.cc"
    break;

  case 138: // where_cls: %empty
#line 816 "parser.y"
                                           {}
#line 3049 "parser.cc"
    break;

  case 139: // decl_inst: at_decl_inst
#line 818 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3055 "parser.cc"
    break;

  case 140: // decl_inst: decl
#line 819 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3061 "parser.cc"
    break;

  case 141: // decls_inst: decls_inst ";" decl_inst
#line 821 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3067 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";"
#line 822 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3073 "parser.cc"
    break;

  case 143: // decls_inst: decl_inst
#line 823 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3079 "parser.cc"
    break;

  case 144: // decls_inst: %empty
#line 824 "parser.y"
                                           {}
#line 3085 "parser.cc"
    break;

  case 145: // decllist_inst: "{" decls_inst "}"
#line 826 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3091 "parser.cc"
    break;

  case 146: // decllist_inst: "vocurly" decls_inst close
#line 827 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3097 "parser.cc"
    break;

  case 147: // where_inst: "where" decllist_inst
#line 829 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3103 "parser.cc"
    break;

  case 148: // where_inst: %empty
#line 830 "parser.y"
                                           {}
#line 3109 "parser.cc"
    break;

  case 149: // decls: decls ";" decl
#line 833 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3115 "parser.cc"
    break;

  case 150: // decls: decls ";"
#line 834 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3121 "parser.cc"
    break;

  case 151: // decls: decl
#line 835 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3127 "parser.cc"
    break;

  case 152: // decls: %empty
#line 836 "parser.y"
                        {}
#line 3133 "parser.cc"
    break;

  case 153: // decllist: "{" decls "}"
#line 838 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3139 "parser.cc"
    break;

  case 154: // decllist: "vocurly" decls close
#line 839 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3145 "parser.cc"
    break;

  case 155: // binds: decllist
#line 841 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3151 "parser.cc"
    break;

  case 156: // wherebinds: "where" binds
#line 843 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3157 "parser.cc"
    break;

  case 157: // wherebinds: %empty
#line 844 "parser.y"
                                 {}
#line 3163 "parser.cc"
    break;

  case 163: // opt_tyconsig: %empty
#line 870 "parser.y"
                                 {}
#line 3169 "parser.cc"
    break;

  case 164: // opt_tyconsig: "::" gtycon
#line 871 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3175 "parser.cc"
    break;

  case 165: // sigtype: ctype
#line 880 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3181 "parser.cc"
    break;

  case 166: // sigtypedoc: ctypedoc
#line 882 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3187 "parser.cc"
    break;

  case 167: // sig_vars: sig_vars "," var
#line 884 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3193 "parser.cc"
    break;

  case 168: // sig_vars: var
#line 885 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3199 "parser.cc"
    break;

  case 169: // sigtypes1: sigtype
#line 887 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3205 "parser.cc"
    break;

  case 170: // sigtypes1: sigtypes1 "," sigtype
#line 888 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3211 "parser.cc"
    break;

  case 171: // ktype: ctype
#line 897 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3217 "parser.cc"
    break;

  case 172: // ktype: ctype "::" kind
#line 898 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeOfKind(yystack_[2].value.as < Hs::Type > (), yystack_[0].value.as < expression_ref > ());}
#line 3223 "parser.cc"
    break;

  case 173: // ctype: "forall" tv_bndrs "." ctype
#line 900 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 3229 "parser.cc"
    break;

  case 174: // ctype: context "=>" ctype
#line 901 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 3235 "parser.cc"
    break;

  case 175: // ctype: type
#line 903 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3241 "parser.cc"
    break;

  case 176: // ctypedoc: ctype
#line 905 "parser.y"
          { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3247 "parser.cc"
    break;

  case 177: // context: btype
#line 914 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 3253 "parser.cc"
    break;

  case 178: // context_no_ops: btype_no_ops
#line 916 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 3259 "parser.cc"
    break;

  case 179: // type: btype
#line 918 "parser.y"
      { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3265 "parser.cc"
    break;

  case 180: // type: btype "->" ctype
#line 919 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3271 "parser.cc"
    break;

  case 181: // typedoc: type
#line 921 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3277 "parser.cc"
    break;

  case 182: // btype: infixtype
#line 924 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3283 "parser.cc"
    break;

  case 183: // infixtype: ftype
#line 926 "parser.y"
           { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3289 "parser.cc"
    break;

  case 184: // infixtype: btype "~" btype
#line 927 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"~"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3295 "parser.cc"
    break;

  case 185: // btype_no_ops: atype_docs
#line 929 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3301 "parser.cc"
    break;

  case 186: // btype_no_ops: btype_no_ops atype_docs
#line 930 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3307 "parser.cc"
    break;

  case 187: // ftype: atype
#line 932 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3313 "parser.cc"
    break;

  case 188: // ftype: tyop
#line 933 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3319 "parser.cc"
    break;

  case 189: // ftype: ftype tyarg
#line 934 "parser.y"
                                   { yylhs.value.as < Hs::Type > () = Hs::TypeApp(yystack_[1].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3325 "parser.cc"
    break;

  case 190: // ftype: ftype "@" atype
#line 935 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[2].value.as < Hs::Type > (); }
#line 3331 "parser.cc"
    break;

  case 191: // tyarg: atype
#line 937 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3337 "parser.cc"
    break;

  case 192: // tyop: qtyconop
#line 939 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3343 "parser.cc"
    break;

  case 193: // tyop: tyvarop
#line 940 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3349 "parser.cc"
    break;

  case 194: // atype_docs: atype
#line 947 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3355 "parser.cc"
    break;

  case 195: // atype: ntgtycon
#line 954 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3361 "parser.cc"
    break;

  case 196: // atype: tyvar
#line 955 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3367 "parser.cc"
    break;

  case 197: // atype: "*"
#line 956 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 3373 "parser.cc"
    break;

  case 198: // atype: PREFIX_BANG atype
#line 957 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictType(yystack_[0].value.as < Hs::Type > ());}
#line 3379 "parser.cc"
    break;

  case 199: // atype: PREFIX_TILDE atype
#line 958 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::LazyType(yystack_[0].value.as < Hs::Type > ());}
#line 3385 "parser.cc"
    break;

  case 200: // atype: "{" fielddecls "}"
#line 959 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 3391 "parser.cc"
    break;

  case 201: // atype: "(" ")"
#line 960 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 3397 "parser.cc"
    break;

  case 202: // atype: "(" comma_types1 "," ktype ")"
#line 961 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 3403 "parser.cc"
    break;

  case 203: // atype: "[" ktype "]"
#line 967 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 3409 "parser.cc"
    break;

  case 204: // atype: "(" ktype ")"
#line 968 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 3415 "parser.cc"
    break;

  case 205: // inst_type: sigtype
#line 971 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3421 "parser.cc"
    break;

  case 208: // comma_types0: comma_types1
#line 976 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 3427 "parser.cc"
    break;

  case 209: // comma_types0: %empty
#line 977 "parser.y"
                                       { /* default construction OK */ }
#line 3433 "parser.cc"
    break;

  case 210: // comma_types1: ktype
#line 979 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3439 "parser.cc"
    break;

  case 211: // comma_types1: comma_types1 "," ktype
#line 980 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3445 "parser.cc"
    break;

  case 212: // tv_bndrs: tv_bndrs tv_bndr
#line 987 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 3451 "parser.cc"
    break;

  case 213: // tv_bndrs: %empty
#line 988 "parser.y"
                               { /* default construction OK */}
#line 3457 "parser.cc"
    break;

  case 214: // tv_bndr: tv_bndr_no_braces
#line 990 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = yystack_[0].value.as < Hs::TypeVar > ();}
#line 3463 "parser.cc"
    break;

  case 215: // tv_bndr: "{" tyvar "}"
#line 991 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[1].location,yystack_[1].value.as < std::string > ()});}
#line 3469 "parser.cc"
    break;

  case 216: // tv_bndr: "{" tyvar "::" kind "}"
#line 992 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()});}
#line 3475 "parser.cc"
    break;

  case 217: // tv_bndr_no_braces: tyvar
#line 995 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3481 "parser.cc"
    break;

  case 218: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 996 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 3487 "parser.cc"
    break;

  case 219: // kind: ctype
#line 1014 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 3493 "parser.cc"
    break;

  case 220: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1020 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3499 "parser.cc"
    break;

  case 221: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1021 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3505 "parser.cc"
    break;

  case 222: // gadt_constrlist: %empty
#line 1022 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3511 "parser.cc"
    break;

  case 223: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1024 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3517 "parser.cc"
    break;

  case 224: // gadt_constrs: gadt_constr
#line 1025 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3523 "parser.cc"
    break;

  case 225: // gadt_constr: optSemi con_list "::" sigtype
#line 1027 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),{{},yystack_[0].value.as < Hs::Type > ()});}
#line 3529 "parser.cc"
    break;

  case 226: // constrs: "=" constrs1
#line 1029 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3535 "parser.cc"
    break;

  case 227: // constrs1: constrs1 "|" constr
#line 1031 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3541 "parser.cc"
    break;

  case 228: // constrs1: constr
#line 1032 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3547 "parser.cc"
    break;

  case 229: // constr: forall context_no_ops "=>" constr_stuff
#line 1034 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 3553 "parser.cc"
    break;

  case 230: // constr: forall constr_stuff
#line 1035 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 3559 "parser.cc"
    break;

  case 231: // forall: "forall" tv_bndrs "."
#line 1037 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 3565 "parser.cc"
    break;

  case 232: // forall: %empty
#line 1038 "parser.y"
                                {}
#line 3571 "parser.cc"
    break;

  case 233: // constr_stuff: btype_no_ops
#line 1040 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 3577 "parser.cc"
    break;

  case 234: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1041 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 3583 "parser.cc"
    break;

  case 235: // fielddecls: %empty
#line 1043 "parser.y"
                                {}
#line 3589 "parser.cc"
    break;

  case 236: // fielddecls: fielddecls1
#line 1044 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3595 "parser.cc"
    break;

  case 237: // fielddecls1: fielddecls1 "," fielddecl
#line 1046 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3601 "parser.cc"
    break;

  case 238: // fielddecls1: fielddecl
#line 1047 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3607 "parser.cc"
    break;

  case 239: // fielddecl: sig_vars "::" ctype
#line 1049 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::Type > ());}
#line 3613 "parser.cc"
    break;

  case 250: // decl_no_th: sigdecl
#line 1068 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3619 "parser.cc"
    break;

  case 251: // decl_no_th: infixexp rhs
#line 1070 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 3625 "parser.cc"
    break;

  case 252: // decl: decl_no_th
#line 1072 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3631 "parser.cc"
    break;

  case 253: // rhs: "=" exp wherebinds
#line 1076 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3637 "parser.cc"
    break;

  case 254: // rhs: gdrhs wherebinds
#line 1077 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3643 "parser.cc"
    break;

  case 255: // gdrhs: gdrhs gdrh
#line 1079 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3649 "parser.cc"
    break;

  case 256: // gdrhs: gdrh
#line 1080 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3655 "parser.cc"
    break;

  case 257: // gdrh: "|" guardquals "=" exp
#line 1084 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3661 "parser.cc"
    break;

  case 258: // sigdecl: sig_vars "::" sigtypedoc
#line 1094 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 3667 "parser.cc"
    break;

  case 259: // sigdecl: infix prec ops
#line 1095 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 3673 "parser.cc"
    break;

  case 260: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1097 "parser.y"
                                                    {}
#line 3679 "parser.cc"
    break;

  case 261: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1098 "parser.y"
                                            {}
#line 3685 "parser.cc"
    break;

  case 262: // sigdecl: "{-# SCC" qvar "#-}"
#line 1099 "parser.y"
                              {}
#line 3691 "parser.cc"
    break;

  case 263: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1100 "parser.y"
                                     {}
#line 3697 "parser.cc"
    break;

  case 264: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1101 "parser.y"
                                                               {}
#line 3703 "parser.cc"
    break;

  case 265: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1102 "parser.y"
                                                                      {}
#line 3709 "parser.cc"
    break;

  case 266: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1103 "parser.y"
                                                     {}
#line 3715 "parser.cc"
    break;

  case 271: // exp: infixexp "::" sigtype
#line 1115 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::Type > ())}; }
#line 3721 "parser.cc"
    break;

  case 272: // exp: infixexp
#line 1116 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3727 "parser.cc"
    break;

  case 273: // infixexp: exp10
#line 1120 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3733 "parser.cc"
    break;

  case 274: // infixexp: infixexp qop exp10
#line 1121 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3739 "parser.cc"
    break;

  case 275: // exp10: "-" fexp
#line 1123 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3745 "parser.cc"
    break;

  case 276: // exp10: fexp
#line 1124 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3751 "parser.cc"
    break;

  case 279: // fexp: fexp aexp
#line 1132 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3757 "parser.cc"
    break;

  case 280: // fexp: fexp "@" atype
#line 1133 "parser.y"
                                 {}
#line 3763 "parser.cc"
    break;

  case 281: // fexp: "static" aexp
#line 1134 "parser.y"
                                 {}
#line 3769 "parser.cc"
    break;

  case 282: // fexp: aexp
#line 1135 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3775 "parser.cc"
    break;

  case 283: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1138 "parser.y"
                                            {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern(Hs::Var(yystack_[2].value.as < std::string > ()),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3781 "parser.cc"
    break;

  case 284: // aexp: PREFIX_TILDE aexp
#line 1139 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3787 "parser.cc"
    break;

  case 285: // aexp: PREFIX_BANG aexp
#line 1140 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3793 "parser.cc"
    break;

  case 286: // aexp: "\\" apats1 "->" exp
#line 1141 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3799 "parser.cc"
    break;

  case 287: // aexp: "let" binds "in" exp
#line 1142 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3805 "parser.cc"
    break;

  case 288: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1144 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3811 "parser.cc"
    break;

  case 289: // aexp: "case" exp "of" altslist
#line 1146 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 3817 "parser.cc"
    break;

  case 290: // aexp: "do" stmtlist
#line 1147 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3823 "parser.cc"
    break;

  case 291: // aexp: "mdo" stmtlist
#line 1148 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3829 "parser.cc"
    break;

  case 292: // aexp: aexp1
#line 1150 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3835 "parser.cc"
    break;

  case 293: // aexp1: aexp1 "{" fbinds "}"
#line 1153 "parser.y"
                              {}
#line 3841 "parser.cc"
    break;

  case 294: // aexp1: aexp2
#line 1154 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3847 "parser.cc"
    break;

  case 295: // aexp2: qvar
#line 1157 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 3853 "parser.cc"
    break;

  case 296: // aexp2: qcon
#line 1158 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 3859 "parser.cc"
    break;

  case 297: // aexp2: literal
#line 1159 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 3865 "parser.cc"
    break;

  case 298: // aexp2: "(" texp ")"
#line 1160 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 3871 "parser.cc"
    break;

  case 299: // aexp2: "(" tup_exprs ")"
#line 1161 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 3877 "parser.cc"
    break;

  case 300: // aexp2: "[" list "]"
#line 1166 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 3883 "parser.cc"
    break;

  case 301: // aexp2: "_"
#line 1167 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 3889 "parser.cc"
    break;

  case 302: // texp: exp
#line 1173 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3895 "parser.cc"
    break;

  case 303: // texp: infixexp qop
#line 1174 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 3901 "parser.cc"
    break;

  case 304: // texp: qopm infixexp
#line 1175 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 3907 "parser.cc"
    break;

  case 305: // tup_exprs: tup_exprs "," texp
#line 1180 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3913 "parser.cc"
    break;

  case 306: // tup_exprs: texp "," texp
#line 1181 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3919 "parser.cc"
    break;

  case 307: // list: texp
#line 1199 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 3925 "parser.cc"
    break;

  case 308: // list: lexps
#line 1200 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 3931 "parser.cc"
    break;

  case 309: // list: texp ".."
#line 1201 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3937 "parser.cc"
    break;

  case 310: // list: texp "," exp ".."
#line 1202 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3943 "parser.cc"
    break;

  case 311: // list: texp ".." exp
#line 1203 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3949 "parser.cc"
    break;

  case 312: // list: texp "," exp ".." exp
#line 1204 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3955 "parser.cc"
    break;

  case 313: // list: texp "|" squals
#line 1205 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 3961 "parser.cc"
    break;

  case 314: // lexps: lexps "," texp
#line 1207 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3967 "parser.cc"
    break;

  case 315: // lexps: texp "," texp
#line 1208 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3973 "parser.cc"
    break;

  case 316: // squals: squals "," qual
#line 1221 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3979 "parser.cc"
    break;

  case 317: // squals: qual
#line 1223 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3985 "parser.cc"
    break;

  case 318: // guardquals: guardquals1
#line 1233 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 3991 "parser.cc"
    break;

  case 319: // guardquals1: guardquals1 "," qual
#line 1235 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3997 "parser.cc"
    break;

  case 320: // guardquals1: qual
#line 1236 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4003 "parser.cc"
    break;

  case 321: // altslist: "{" alts "}"
#line 1239 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4009 "parser.cc"
    break;

  case 322: // altslist: "vocurly" alts close
#line 1240 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4015 "parser.cc"
    break;

  case 323: // altslist: "{" "}"
#line 1241 "parser.y"
                                 {}
#line 4021 "parser.cc"
    break;

  case 324: // altslist: "vocurly" close
#line 1242 "parser.y"
                                 {}
#line 4027 "parser.cc"
    break;

  case 325: // alts: alts1
#line 1244 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4033 "parser.cc"
    break;

  case 326: // alts: ";" alts
#line 1245 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4039 "parser.cc"
    break;

  case 327: // alts1: alts1 ";" alt
#line 1247 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4045 "parser.cc"
    break;

  case 328: // alts1: alts1 ";"
#line 1248 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4051 "parser.cc"
    break;

  case 329: // alts1: alt
#line 1249 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4057 "parser.cc"
    break;

  case 330: // alt: pat alt_rhs
#line 1251 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4063 "parser.cc"
    break;

  case 331: // alt_rhs: "->" exp wherebinds
#line 1253 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4069 "parser.cc"
    break;

  case 332: // alt_rhs: gdpats wherebinds
#line 1254 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4075 "parser.cc"
    break;

  case 333: // gdpats: gdpats gdpat
#line 1256 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4081 "parser.cc"
    break;

  case 334: // gdpats: gdpat
#line 1257 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4087 "parser.cc"
    break;

  case 335: // gdpat: "|" guardquals "->" exp
#line 1266 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4093 "parser.cc"
    break;

  case 336: // pat: exp
#line 1268 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4099 "parser.cc"
    break;

  case 337: // bindpat: exp
#line 1270 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4105 "parser.cc"
    break;

  case 338: // apat: aexp
#line 1272 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4111 "parser.cc"
    break;

  case 339: // apats1: apats1 apat
#line 1274 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4117 "parser.cc"
    break;

  case 340: // apats1: apat
#line 1275 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4123 "parser.cc"
    break;

  case 341: // stmtlist: "{" stmts "}"
#line 1278 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4129 "parser.cc"
    break;

  case 342: // stmtlist: "vocurly" stmts close
#line 1279 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4135 "parser.cc"
    break;

  case 343: // stmts: stmts ";" stmt
#line 1281 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4141 "parser.cc"
    break;

  case 344: // stmts: stmts ";"
#line 1282 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4147 "parser.cc"
    break;

  case 345: // stmts: stmt
#line 1283 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4153 "parser.cc"
    break;

  case 346: // stmts: %empty
#line 1284 "parser.y"
                       {}
#line 4159 "parser.cc"
    break;

  case 347: // stmt: qual
#line 1289 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4165 "parser.cc"
    break;

  case 348: // stmt: "rec" stmtlist
#line 1290 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4171 "parser.cc"
    break;

  case 349: // qual: bindpat "<-" exp
#line 1292 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4177 "parser.cc"
    break;

  case 350: // qual: exp
#line 1293 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4183 "parser.cc"
    break;

  case 351: // qual: "let" binds
#line 1294 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4189 "parser.cc"
    break;

  case 359: // qcon: gen_qcon
#line 1339 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4195 "parser.cc"
    break;

  case 360: // qcon: sysdcon
#line 1340 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4201 "parser.cc"
    break;

  case 361: // gen_qcon: qconid
#line 1342 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4207 "parser.cc"
    break;

  case 362: // gen_qcon: "(" qconsym ")"
#line 1343 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4213 "parser.cc"
    break;

  case 363: // con: conid
#line 1345 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4219 "parser.cc"
    break;

  case 364: // con: "(" consym ")"
#line 1346 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4225 "parser.cc"
    break;

  case 365: // con: sysdcon
#line 1347 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4231 "parser.cc"
    break;

  case 366: // con_list: con_list "," con
#line 1349 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4237 "parser.cc"
    break;

  case 367: // con_list: con
#line 1350 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4243 "parser.cc"
    break;

  case 368: // sysdcon_no_list: "(" ")"
#line 1352 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4249 "parser.cc"
    break;

  case 369: // sysdcon_no_list: "(" commas ")"
#line 1353 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4255 "parser.cc"
    break;

  case 370: // sysdcon_no_list: "(#" "#)"
#line 1354 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4261 "parser.cc"
    break;

  case 371: // sysdcon_no_list: "(#" commas "#)"
#line 1355 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4267 "parser.cc"
    break;

  case 372: // sysdcon: sysdcon_no_list
#line 1357 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4273 "parser.cc"
    break;

  case 373: // sysdcon: "[" "]"
#line 1358 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4279 "parser.cc"
    break;

  case 374: // conop: consym
#line 1360 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4285 "parser.cc"
    break;

  case 375: // conop: "`" conid "`"
#line 1361 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4291 "parser.cc"
    break;

  case 376: // qconop: qconsym
#line 1363 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4297 "parser.cc"
    break;

  case 377: // qconop: "`" qconid "`"
#line 1364 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4303 "parser.cc"
    break;

  case 378: // gtycon: ntgtycon
#line 1367 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4309 "parser.cc"
    break;

  case 379: // gtycon: "(" ")"
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4315 "parser.cc"
    break;

  case 380: // gtycon: "(#" "#)"
#line 1369 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4321 "parser.cc"
    break;

  case 381: // ntgtycon: oqtycon
#line 1371 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4327 "parser.cc"
    break;

  case 382: // ntgtycon: "(" commas ")"
#line 1372 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4333 "parser.cc"
    break;

  case 383: // ntgtycon: "(#" commas "#)"
#line 1373 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4339 "parser.cc"
    break;

  case 384: // ntgtycon: "(" "->" ")"
#line 1374 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4345 "parser.cc"
    break;

  case 385: // ntgtycon: "[" "]"
#line 1375 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4351 "parser.cc"
    break;

  case 386: // oqtycon: qtycon
#line 1377 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4357 "parser.cc"
    break;

  case 387: // oqtycon: "(" qtyconsym ")"
#line 1378 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4363 "parser.cc"
    break;

  case 388: // oqtycon: "(" "~" ")"
#line 1379 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4369 "parser.cc"
    break;

  case 389: // oqtycon_no_varcon: qtycon
#line 1381 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4375 "parser.cc"
    break;

  case 390: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1382 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4381 "parser.cc"
    break;

  case 391: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1383 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4387 "parser.cc"
    break;

  case 392: // oqtycon_no_varcon: "(" ":" ")"
#line 1384 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4393 "parser.cc"
    break;

  case 393: // oqtycon_no_varcon: "(" "~" ")"
#line 1385 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4399 "parser.cc"
    break;

  case 394: // qtyconop: qtyconsym
#line 1388 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4405 "parser.cc"
    break;

  case 395: // qtyconop: "`" qtycon "`"
#line 1389 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4411 "parser.cc"
    break;

  case 396: // qtycondoc: qtycon
#line 1391 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4417 "parser.cc"
    break;

  case 397: // qtycon: "QCONID"
#line 1393 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4423 "parser.cc"
    break;

  case 398: // qtycon: tycon
#line 1394 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4429 "parser.cc"
    break;

  case 399: // tycon: "CONID"
#line 1398 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4435 "parser.cc"
    break;

  case 400: // qtyconsym: "QCONSYM"
#line 1400 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4441 "parser.cc"
    break;

  case 401: // qtyconsym: "QVARSYM"
#line 1401 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4447 "parser.cc"
    break;

  case 402: // qtyconsym: tyconsym
#line 1402 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4453 "parser.cc"
    break;

  case 403: // tyconsym: "CONSYM"
#line 1404 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4459 "parser.cc"
    break;

  case 404: // tyconsym: "VARSYM"
#line 1405 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4465 "parser.cc"
    break;

  case 405: // tyconsym: ":"
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4471 "parser.cc"
    break;

  case 406: // tyconsym: "-"
#line 1407 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4477 "parser.cc"
    break;

  case 407: // op: varop
#line 1412 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4483 "parser.cc"
    break;

  case 408: // op: conop
#line 1413 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4489 "parser.cc"
    break;

  case 409: // varop: varsym
#line 1415 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4495 "parser.cc"
    break;

  case 410: // varop: "`" varid "`"
#line 1416 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4501 "parser.cc"
    break;

  case 411: // qop: qvarop
#line 1418 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4507 "parser.cc"
    break;

  case 412: // qop: qconop
#line 1419 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4513 "parser.cc"
    break;

  case 413: // qopm: qvaropm
#line 1422 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4519 "parser.cc"
    break;

  case 414: // qopm: qconop
#line 1423 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4525 "parser.cc"
    break;

  case 415: // qvarop: qvarsym
#line 1428 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4531 "parser.cc"
    break;

  case 416: // qvarop: "`" qvarid "`"
#line 1429 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4537 "parser.cc"
    break;

  case 417: // qvaropm: qvarsym_no_minus
#line 1431 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4543 "parser.cc"
    break;

  case 418: // qvaropm: "`" qvarid "`"
#line 1432 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4549 "parser.cc"
    break;

  case 419: // tyvar: tyvarid
#line 1436 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4555 "parser.cc"
    break;

  case 420: // tyvarop: "`" tyvarid "`"
#line 1438 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4561 "parser.cc"
    break;

  case 421: // tyvarid: "VARID"
#line 1440 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4567 "parser.cc"
    break;

  case 422: // tyvarid: special_id
#line 1441 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4573 "parser.cc"
    break;

  case 423: // tyvarid: "unsafe"
#line 1442 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4579 "parser.cc"
    break;

  case 424: // tyvarid: "safe"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4585 "parser.cc"
    break;

  case 425: // tyvarid: "interruptible"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4591 "parser.cc"
    break;

  case 426: // var: varid
#line 1447 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4597 "parser.cc"
    break;

  case 427: // var: "(" varsym ")"
#line 1448 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4603 "parser.cc"
    break;

  case 428: // qvar: qvarid
#line 1450 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4609 "parser.cc"
    break;

  case 429: // qvar: "(" varsym ")"
#line 1451 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4615 "parser.cc"
    break;

  case 430: // qvar: "(" qvarsym1 ")"
#line 1452 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4621 "parser.cc"
    break;

  case 431: // qvarid: varid
#line 1454 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4627 "parser.cc"
    break;

  case 432: // qvarid: "QVARID"
#line 1455 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4633 "parser.cc"
    break;

  case 433: // varid: "VARID"
#line 1457 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4639 "parser.cc"
    break;

  case 434: // varid: special_id
#line 1458 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4645 "parser.cc"
    break;

  case 435: // varid: "unsafe"
#line 1459 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4651 "parser.cc"
    break;

  case 436: // varid: "safe"
#line 1460 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4657 "parser.cc"
    break;

  case 437: // varid: "interruptible"
#line 1461 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4663 "parser.cc"
    break;

  case 438: // varid: "forall"
#line 1462 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4669 "parser.cc"
    break;

  case 439: // varid: "family"
#line 1463 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4675 "parser.cc"
    break;

  case 440: // varid: "role"
#line 1464 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4681 "parser.cc"
    break;

  case 441: // qvarsym: varsym
#line 1466 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4687 "parser.cc"
    break;

  case 442: // qvarsym: qvarsym1
#line 1467 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4693 "parser.cc"
    break;

  case 443: // qvarsym_no_minus: varsym_no_minus
#line 1469 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4699 "parser.cc"
    break;

  case 444: // qvarsym_no_minus: qvarsym1
#line 1470 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4705 "parser.cc"
    break;

  case 445: // qvarsym1: "QVARSYM"
#line 1472 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4711 "parser.cc"
    break;

  case 446: // varsym: varsym_no_minus
#line 1474 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4717 "parser.cc"
    break;

  case 447: // varsym: "-"
#line 1475 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4723 "parser.cc"
    break;

  case 448: // varsym_no_minus: "VARSYM"
#line 1477 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4729 "parser.cc"
    break;

  case 449: // varsym_no_minus: special_sym
#line 1478 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4735 "parser.cc"
    break;

  case 450: // special_id: "as"
#line 1480 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4741 "parser.cc"
    break;

  case 451: // special_id: "qualified"
#line 1481 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4747 "parser.cc"
    break;

  case 452: // special_id: "hiding"
#line 1482 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4753 "parser.cc"
    break;

  case 453: // special_id: "export"
#line 1483 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4759 "parser.cc"
    break;

  case 454: // special_id: "label"
#line 1484 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4765 "parser.cc"
    break;

  case 455: // special_id: "dynamic"
#line 1485 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4771 "parser.cc"
    break;

  case 456: // special_id: "stdcall"
#line 1486 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4777 "parser.cc"
    break;

  case 457: // special_id: "ccall"
#line 1487 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4783 "parser.cc"
    break;

  case 458: // special_id: "capi"
#line 1488 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4789 "parser.cc"
    break;

  case 459: // special_id: "prim"
#line 1489 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4795 "parser.cc"
    break;

  case 460: // special_id: "javascript"
#line 1490 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4801 "parser.cc"
    break;

  case 461: // special_id: "group"
#line 1491 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4807 "parser.cc"
    break;

  case 462: // special_id: "stock"
#line 1492 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4813 "parser.cc"
    break;

  case 463: // special_id: "anyclass"
#line 1493 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4819 "parser.cc"
    break;

  case 464: // special_id: "via"
#line 1494 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4825 "parser.cc"
    break;

  case 465: // special_id: "unit"
#line 1495 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4831 "parser.cc"
    break;

  case 466: // special_id: "dependency"
#line 1496 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4837 "parser.cc"
    break;

  case 467: // special_id: "signature"
#line 1497 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4843 "parser.cc"
    break;

  case 468: // special_sym: "!"
#line 1499 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4849 "parser.cc"
    break;

  case 469: // special_sym: "."
#line 1500 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4855 "parser.cc"
    break;

  case 470: // special_sym: "*"
#line 1501 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4861 "parser.cc"
    break;

  case 471: // qconid: conid
#line 1505 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4867 "parser.cc"
    break;

  case 472: // qconid: "QCONID"
#line 1506 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4873 "parser.cc"
    break;

  case 473: // conid: "CONID"
#line 1508 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4879 "parser.cc"
    break;

  case 474: // qconsym: consym
#line 1510 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4885 "parser.cc"
    break;

  case 475: // qconsym: "QCONSYM"
#line 1511 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4891 "parser.cc"
    break;

  case 476: // consym: "CONSYM"
#line 1513 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4897 "parser.cc"
    break;

  case 477: // consym: ":"
#line 1514 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4903 "parser.cc"
    break;

  case 478: // literal: "CHAR"
#line 1518 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4909 "parser.cc"
    break;

  case 479: // literal: "STRING"
#line 1519 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 4915 "parser.cc"
    break;

  case 480: // literal: "INTEGER"
#line 1520 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 4921 "parser.cc"
    break;

  case 481: // literal: "RATIONAL"
#line 1521 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 4927 "parser.cc"
    break;

  case 482: // literal: "PRIMINTEGER"
#line 1522 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 4933 "parser.cc"
    break;

  case 484: // close: error
#line 1530 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4939 "parser.cc"
    break;

  case 485: // modid: "CONID"
#line 1534 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4945 "parser.cc"
    break;

  case 486: // modid: "QCONID"
#line 1535 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4951 "parser.cc"
    break;

  case 487: // commas: commas ","
#line 1537 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4957 "parser.cc"
    break;

  case 488: // commas: ","
#line 1538 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4963 "parser.cc"
    break;


#line 4967 "parser.cc"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        context yyctx (*this, yyla);
        std::string msg = yysyntax_error_ (yyctx);
        error (yyla.location, YY_MOVE (msg));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind () == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    // Pop stack until we find a state that shifts the error token.
    for (;;)
      {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_ (yyn))
          {
            yyn += symbol_kind::S_YYerror;
            if (0 <= yyn && yyn <= yylast_
                && yycheck_[yyn] == symbol_kind::S_YYerror)
              {
                yyn = yytable_[yyn];
                if (0 < yyn)
                  break;
              }
          }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size () == 1)
          YYABORT;

        yyerror_range[1].location = yystack_[0].location;
        yy_destroy_ ("Error: popping", yystack_[0]);
        yypop_ ();
        YY_STACK_PRINT ();
      }
    {
      stack_symbol_type error_token;

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
    goto yynewstate;


  /*-------------------------------------.
  | yyacceptlab -- YYACCEPT comes here.  |
  `-------------------------------------*/
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;


  /*-----------------------------------.
  | yyabortlab -- YYABORT comes here.  |
  `-----------------------------------*/
  yyabortlab:
    yyresult = 1;
    goto yyreturn;


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    YY_STACK_PRINT ();
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what ());
  }

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr;
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              else
                goto append;

            append:
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }

  std::string
  parser::symbol_name (symbol_kind_type yysymbol)
  {
    return yytnamerr_ (yytname_[yysymbol]);
  }



  // parser::context.
  parser::context::context (const parser& yyparser, const symbol_type& yyla)
    : yyparser_ (yyparser)
    , yyla_ (yyla)
  {}

  int
  parser::context::expected_tokens (symbol_kind_type yyarg[], int yyargn) const
  {
    // Actual number of expected tokens
    int yycount = 0;

    const int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_ (yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        const int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        const int yychecklim = yylast_ - yyn + 1;
        const int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
          if (yycheck_[yyx + yyn] == yyx && yyx != symbol_kind::S_YYerror
              && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
            {
              if (!yyarg)
                ++yycount;
              else if (yycount == yyargn)
                return 0;
              else
                yyarg[yycount++] = YY_CAST (symbol_kind_type, yyx);
            }
      }

    if (yyarg && yycount == 0 && 0 < yyargn)
      yyarg[0] = symbol_kind::S_YYEMPTY;
    return yycount;
  }






  int
  parser::yy_syntax_error_arguments_ (const context& yyctx,
                                                 symbol_kind_type yyarg[], int yyargn) const
  {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */

    if (!yyctx.lookahead ().empty ())
      {
        if (yyarg)
          yyarg[0] = yyctx.token ();
        int yyn = yyctx.expected_tokens (yyarg ? yyarg + 1 : yyarg, yyargn - 1);
        return yyn + 1;
      }
    return 0;
  }

  // Generate an error message.
  std::string
  parser::yysyntax_error_ (const context& yyctx) const
  {
    // Its maximum.
    enum { YYARGS_MAX = 5 };
    // Arguments of yyformat.
    symbol_kind_type yyarg[YYARGS_MAX];
    int yycount = yy_syntax_error_arguments_ (yyctx, yyarg, YYARGS_MAX);

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
      default: // Avoid compiler warnings.
        YYCASE_ (0, YY_("syntax error"));
        YYCASE_ (1, YY_("syntax error, unexpected %s"));
        YYCASE_ (2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_ (3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_ (4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_ (5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    std::ptrdiff_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += symbol_name (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short parser::yypact_ninf_ = -633;

  const short parser::yytable_ninf_ = -447;

  const short
  parser::yypact_[] =
  {
      23,   271,  -633,    72,  -633,  -633,  -633,  -633,  -633,   368,
      17,   -25,  -633,    61,   103,   103,    26,  -633,  -633,  -633,
    -633,    86,  -633,  -633,  -633,    63,  -633,   139,   162,  4413,
     221,   236,   171,  -633,   727,  -633,    83,  -633,  -633,  -633,
    -633,   271,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
    -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
    -633,  -633,  -633,  -633,   990,  -633,  -633,  -633,  -633,   183,
     192,  -633,   218,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
      89,  -633,   271,  -633,   217,  -633,  2196,  3906,  -633,   244,
     219,  2196,  -633,  -633,  -633,   388,   281,  -633,  3081,   362,
     219,  2861,   251,  4683,   235,  2861,  2861,  2462,  2861,  1531,
    1398,   312,  -633,  -633,  -633,  -633,  -633,  -633,  -633,    22,
     251,   268,   171,  -633,  -633,  -633,  -633,   318,    68,  -633,
    -633,   857,  -633,  2595,  -633,   314,  -633,  -633,  -633,  -633,
    -633,  -633,   359,    88,  -633,  -633,  -633,  -633,   296,  -633,
     373,   380,  -633,  -633,  -633,  -633,  -633,   389,  -633,   398,
     400,   401,  -633,  -633,  -633,  4413,  4450,  -633,  -633,  -633,
    -633,   445,  -633,  1398,   492,   322,  -633,  -633,  -633,  -633,
    4311,  -633,  4311,  -633,  4779,  3184,  2970,   405,  4609,  -633,
    -633,  -633,  -633,  -633,   489,   426,  -633,   307,  -633,  4213,
    -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
    -633,  -633,  3391,  1930,  1930,  -633,   409,   448,   449,   450,
     451,  3391,  1132,  1132,  -633,   514,  3906,  3906,    95,   453,
     346,    96,   490,  -633,  -633,   -21,  4683,  -633,   275,   -16,
     424,   227,  -633,   116,  -633,  -633,  -633,  -633,  2728,  -633,
    2595,  -633,  -633,  -633,  4548,  -633,  -633,  -633,   322,    94,
     430,   416,  -633,  2196,  -633,  -633,  -633,  -633,  -633,  -633,
    2462,  -633,  -633,   140,   178,   400,   427,   428,   429,   247,
    -633,   326,  3391,  4683,  4683,  -633,   486,   217,   410,  3906,
    3391,  4779,  2196,  2329,  4548,  -633,    27,  -633,  -633,  2196,
    -633,  -633,  -633,  -633,  4311,  -633,  4646,  2861,  -633,  -633,
    -633,  -633,  -633,  -633,  -633,  -633,   431,   432,   433,  -633,
     441,    61,   271,    70,   306,  3391,  -633,  -633,   241,   126,
     443,   435,  -633,  -633,  -633,  -633,   440,   474,   464,  -633,
     446,   454,  -633,   456,   460,   459,   261,   339,   455,   467,
     324,  -633,  3906,  3391,  3906,  4311,  -633,  -633,  -633,   477,
     461,   281,   219,   485,   508,   177,  -633,  -633,    39,  -633,
     569,  -633,  -633,  -633,  -633,  -633,  -633,   570,   202,  -633,
    -633,   857,    46,  2196,  -633,   515,   376,  3391,   -36,  3391,
     470,   471,   495,   529,  -633,   532,   503,   152,   235,   537,
    2196,  -633,   498,   501,  2196,  2196,  2329,  1664,  -633,  1664,
     397,  -633,  1664,  -633,  1664,   142,  -633,  -633,  -633,  -633,
     543,   541,   549,  4742,   516,  -633,  -633,  -633,  -633,  -633,
      -1,   391,  -633,  -633,  -633,  -633,   604,   561,   527,  -633,
     528,   281,  -633,  -633,  -633,  -633,  -633,   546,  -633,   534,
     571,  -633,  -633,  -633,  4511,  -633,  -633,  -633,   545,  4413,
    -633,  -633,  1797,  1265,  -633,  -633,   547,  3391,  -633,  4779,
    4816,  -633,  3391,  3391,  -633,  -633,  -633,  3391,  -633,  -633,
    -633,  -633,  -633,   866,   866,  -633,  -633,  -633,   576,  -633,
    -633,  3391,   514,  -633,  2196,  -633,  1930,  -633,  2196,   363,
    -633,  -633,  1132,  -633,  -633,  3391,  3391,  4922,   575,  -633,
    -633,   190,  -633,  -633,  4779,   568,  -633,  -633,  -633,  -633,
     573,   115,   342,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
    -633,   551,  -633,   598,  -633,  -633,  -633,  -633,  -633,  3391,
    3391,   560,   563,   486,  -633,   606,  3391,   653,   657,   676,
    -633,  2196,  2329,  -633,  -633,  -633,  4646,  1664,  -633,  4413,
     578,  -633,  2063,  -633,   587,   577,  -633,   169,    61,  -633,
    -633,  -633,  -633,  3391,  5015,  5015,  -633,  -633,  -633,  -633,
    -633,   583,   658,  3288,  -633,  -633,   246,  -633,    55,  -633,
    -633,  -633,   409,   999,   999,  -633,  -633,  -633,  -633,  -633,
    5015,   669,   459,   617,  -633,  -633,  -633,  2329,  2196,  -633,
     -19,   -12,  -633,  -633,  -633,  -633,  -633,  -633,   614,  -633,
    4311,   367,   676,    57,  -633,   676,  -633,  -633,  -633,  -633,
    -633,   589,  -633,  -633,  -633,  2196,  2329,  2196,  -633,    37,
    -633,  -633,  -633,    -6,   620,  -633,  -633,  3906,  3906,  3906,
    -633,   414,  -633,   866,  -633,   690,   683,  -633,  -633,   249,
    -633,    56,  -633,   618,   374,  -633,  3391,  -633,  -633,  -633,
    3391,  -633,  4889,   653,   619,  4009,  -633,  -633,  -633,   409,
     409,  -633,  -633,  -633,  -633,  3494,   125,   652,  -633,  -633,
    -633,  -633,  -633,   625,   604,  -633,  -633,  3391,  -633,  3391,
     632,  -633,   421,  3391,  3597,  -633,  -633,  2196,  -633,  3906,
    -633,   999,  -633,  5015,  3700,  3803,  -633,  -633,  -633,  -633,
    -633,  4311,   593,  -633,  4311,   257,  -633,   235,    58,  -633,
    -633,   599,   607,  -633,  3906,  -633,  2196,  -633,   612,   610,
    3391,  -633,  4982,  -633,  -633,  2970,   637,   638,  -633,  -633,
    -633,  5015,  -633,   621,   258,  -633,    61,    59,  4111,  -633,
    4311,  -633,   409,   167,  -633,  3906,  -633,  -633,  -633,  -633,
    -633,  -633,   620,  5015,  -633,  -633,  -633,  3906,  -633,  -633,
    -633,  3391,  -633,  -633,  -633,  -633
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   485,   486,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   484,   483,    12,   162,   158,     0,     0,     0,
       0,    42,    37,    15,    14,   161,     0,     6,     7,   450,
     452,     0,   451,   438,   453,   454,   455,   436,   437,   435,
     439,   440,   456,   457,   458,   459,   460,   461,   462,   463,
     464,   465,   467,   466,     0,   433,   399,   432,   397,     0,
      19,    21,    24,    32,   389,   398,    31,   428,   431,   434,
       0,    41,     0,    34,    38,   301,     0,     0,   112,     0,
       0,     0,    51,    52,    53,    81,     0,   113,     0,     0,
       0,     0,   267,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   473,   472,   478,   479,   480,   481,   482,   267,
     267,    49,    56,    59,    60,    61,    62,   128,     0,    65,
     250,    66,   273,   276,   282,   292,   294,   296,   359,   372,
     360,   168,   295,   431,   361,   471,   297,   159,     0,    23,
       0,     0,   447,   468,   470,   469,   448,     0,   445,     0,
       0,     0,   446,   449,    17,     0,    27,    22,    36,    36,
       3,    44,    33,     0,     0,   272,   424,   425,   423,   405,
       0,   406,     0,   197,   235,     0,     0,     0,     0,   421,
     404,   403,   401,   400,   138,     0,   125,   179,   182,   183,
     188,   187,   195,   381,   192,   386,   394,   402,   196,   193,
     419,   422,   209,   346,   346,   290,   278,     0,     0,     0,
       0,     0,   152,   152,   155,     0,     0,     0,     0,     0,
     179,   381,     0,   291,   281,     0,     0,   268,     0,     0,
       0,     0,   367,   163,   365,   363,   338,   340,     0,   284,
     275,   285,   477,   373,     0,   476,   475,   302,   272,   307,
       0,   308,   414,     0,   413,   417,   444,   443,   376,   474,
     447,   368,   488,     0,     0,   444,     0,   443,   376,     0,
     370,     0,     0,     0,     0,    50,     0,    57,     0,     0,
       0,     0,     0,     0,     0,   251,   157,   256,   412,     0,
     411,   415,   442,   441,     0,   279,   353,     0,   160,   392,
     393,   391,   390,   430,   429,    20,     0,     0,    28,    30,
       0,     0,     0,    46,     0,     0,   199,   198,     0,     0,
       0,   236,   238,   426,   213,   385,     0,   171,     0,   175,
       0,     0,   201,   210,     0,   394,     0,     0,     0,     0,
       0,    67,     0,     0,     0,     0,   189,   191,   210,     0,
     208,     0,     0,   350,     0,     0,   345,   347,     0,   277,
       0,    78,    77,    79,    80,   205,   165,   148,     0,   252,
     151,     0,     0,     0,    76,     0,   118,     0,     0,     0,
       0,     0,     0,     0,   262,     0,     0,     0,     0,     0,
       0,   339,     0,     0,   303,   309,     0,     0,   300,     0,
     304,   298,     0,   299,     0,   429,   362,   369,   487,   371,
       0,     0,     0,     0,   259,   408,    55,   407,   409,   374,
       0,   114,   258,   176,   166,   167,   157,     0,   318,   320,
       0,     0,   254,   255,   274,   280,   356,     0,   352,   355,
     358,   283,    26,    25,     0,     9,    10,    43,     0,     0,
      40,    45,     0,     0,   289,   271,     0,     0,   200,     0,
       0,   203,     0,     0,   384,   388,   204,     0,   387,   382,
     383,   395,   420,   134,   134,   137,   124,   180,   184,   190,
      63,     0,   351,   348,     0,   341,   344,   342,     0,     0,
      75,   153,   150,   154,   287,     0,     0,     0,    86,   219,
      72,     0,    73,    68,     0,     0,   269,   261,   263,   364,
       0,     0,     0,   164,   378,   366,   260,   286,   418,   377,
     311,   313,   317,   302,   315,   314,   306,   305,   266,     0,
       0,     0,     0,     0,   127,     0,     0,   232,   222,   240,
     253,     0,     0,   416,   156,   293,     0,     0,    29,     0,
       0,   323,     0,   336,     0,   325,   329,     0,     0,   324,
     427,   239,   237,     0,     0,     0,   212,   214,   217,   172,
     174,   211,   107,     0,   129,   133,     0,   130,     0,   211,
     349,   343,   278,   144,   144,   147,   149,   101,   119,   120,
       0,    91,     0,     0,   270,   379,   380,     0,   310,   169,
       0,     0,   410,   375,    54,   126,   115,   213,   226,   228,
       0,     0,   240,     0,    69,   241,   243,   257,   319,   354,
     357,     0,    47,   326,   321,   328,     0,     0,   330,   157,
     334,   322,   173,     0,     0,   202,   108,     0,     0,     0,
     105,   121,   135,   132,   136,     0,   109,   139,   143,     0,
     140,     0,    87,     0,     0,    71,     0,   316,   312,   264,
       0,   265,     0,   232,     0,   233,   185,   194,   230,   278,
     278,    70,    84,    82,    83,     0,     0,   244,   247,   396,
     242,    48,   327,     0,   157,   332,   333,     0,   215,     0,
     116,   106,   121,     0,     0,   103,   131,     0,   110,     0,
     145,   142,   146,     0,   100,   100,    92,    64,   170,   231,
     227,     0,     0,   186,     0,     0,   224,     0,     0,   248,
     181,   206,     0,   245,     0,   246,     0,   331,     0,     0,
       0,   102,     0,   104,   122,     0,     0,   196,   288,   111,
     141,    88,    90,     0,     0,    99,     0,     0,   233,   229,
     234,   220,   278,     0,   221,     0,   249,    85,   335,   216,
     218,   117,   196,     0,    89,    95,    93,    98,    96,    94,
     223,     0,   207,   123,    97,   225
  };

  const short
  parser::yypgoto_[] =
  {
    -633,  -633,  -633,  -633,  -633,  -633,  -633,    29,  -633,  -633,
    -408,  -633,   559,  -633,  -633,  -633,  -156,   594,  -633,  -633,
    -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
    -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,  -633,
    -633,   -48,  -633,  -633,  -633,    11,  -225,  -633,  -633,  -633,
    -633,  -633,  -633,  -633,  -633,    25,   439,  -633,    76,   252,
    -633,  -633,    30,   144,  -633,  -633,   519,  -633,  -308,  -409,
     733,  -633,  -633,  -316,   102,  -170,   212,  -146,   -99,  -633,
     -54,  -633,   -74,  -633,   -90,  -633,  -632,  -633,  -633,  -633,
    -572,   -52,   491,     7,  -633,   564,   158,   278,  -597,  -426,
    -633,   106,    31,  -633,  -633,   114,  -633,    67,  -633,  -633,
     320,   170,  -633,   166,   108,   761,  -206,  -633,  -633,   500,
    -633,   384,  -633,   256,    -4,  -249,  -198,   -70,   -63,  -633,
    -633,   -84,  -633,  -633,  -633,  -633,   161,  -633,  -633,  -395,
    -633,   163,  -633,  -633,   160,  -633,  -633,   552,  -633,   -71,
     588,   305,  -257,  -633,   248,  -633,  -633,  -633,   407,    79,
    -633,   -97,  -620,  -100,  -633,   411,   -75,  -633,  -633,  -633,
     -24,  -633,  -182,  -633,   267,  -633,   553,  -633,  -633,  -633,
    -458,  -633,  -169,  -263,    -9,  -171,    -2,  -633,  -633,   -29,
     -58,   -88,   -87,  -633,  -163,   -89,   -47,  -238,  -633,  -250,
     -15,  -109
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      69,    70,    71,   167,   317,   318,    72,    84,    11,    20,
      21,    32,    82,   323,   460,   461,   286,   121,   424,    33,
      34,   122,   123,   124,   125,   228,   126,   221,   686,   735,
     601,   662,   751,   665,   716,   754,   755,   584,   647,   709,
     657,   127,   548,   741,   508,   705,   194,   289,   585,   586,
     485,   351,   658,   659,   595,   500,   378,   224,   225,   442,
      27,    36,   399,   375,   432,   128,   610,   343,   509,   434,
     338,   674,   339,   731,   197,   198,   675,   199,   356,   200,
     676,   201,   377,   732,   359,   344,   470,   576,   577,   510,
     622,   725,   726,   549,   618,   619,   620,   678,   330,   331,
     332,   624,   625,   626,   687,   379,   587,   295,   296,   297,
     130,   236,   237,   363,   175,   132,   727,   133,   134,   135,
     136,   273,   274,   260,   261,   531,   437,   438,   464,   564,
     565,   566,   638,   639,   640,   567,   364,   247,   248,   215,
     365,   366,   367,   447,   448,   449,   137,   138,   242,   243,
     139,   140,   425,   262,   523,   202,   203,    73,   204,   688,
     205,    75,   206,   207,   426,   427,   299,   263,   300,   264,
     208,   209,   210,   141,   142,    77,    78,   301,   265,   266,
     303,   162,    79,   163,   144,   145,   268,   269,   146,    24,
       9,   279
  };

  const short
  parser::yytable_[] =
  {
     211,   384,   281,   396,   345,    74,   161,   244,   230,   465,
     319,   211,   578,   196,   329,   245,   380,   380,   370,   349,
      76,   267,   277,   231,   229,   259,   149,   550,   435,   233,
     131,   298,   143,   195,    13,   160,   439,   250,   234,   336,
      22,   282,   246,   249,     1,   251,   579,    22,   429,   578,
     444,   560,   276,   492,   441,   724,    22,    22,   669,    22,
      22,   394,    22,   278,   441,   671,   358,   171,   568,   391,
     305,   456,    12,   511,   697,   298,   544,   346,   347,   682,
     598,   275,   458,   402,    66,   277,   337,   337,    68,   758,
      18,   403,   760,   211,   239,   211,   698,   670,   211,   211,
      31,   211,   302,   723,   670,   258,   258,   746,   683,   684,
     392,   293,   211,   337,   395,   161,   643,   644,   497,    17,
     616,   636,   376,   440,     2,   211,   278,   235,   326,   545,
     327,   403,   503,   554,   211,    29,   230,   230,   724,   211,
     211,    74,    74,    23,   275,   746,   302,   357,   290,   532,
      23,   631,   385,   386,   496,   444,    76,    76,   298,    23,
      23,   502,    23,    23,   348,    23,   685,   633,  -426,   258,
     653,   711,   405,   762,   777,   387,   -74,    66,   406,   459,
     161,    68,   333,   376,   291,   246,   723,   305,   723,   147,
     168,   433,   169,    35,   179,   211,   397,   320,   321,   148,
     250,   340,   211,   211,  -426,   341,   467,   181,    25,   160,
     407,   388,   -74,   569,   578,   196,    37,   211,   381,   381,
     143,   143,  -427,   609,   609,   605,   376,   393,   428,   302,
     695,   272,   398,    26,   685,   195,   190,   191,   211,    38,
     192,   193,   291,   513,   451,    66,   747,   781,    80,    68,
     411,   603,   445,   636,   487,   637,   412,   520,  -427,   410,
      81,   521,   230,   522,   488,   211,   211,   211,   211,   179,
     466,   738,    66,   739,   421,   422,    68,   744,   486,   495,
     341,   298,   181,   398,   578,   737,    83,   772,   413,   333,
     337,   493,   496,   164,   414,   628,   596,   450,   558,   329,
     211,   244,   211,   489,   501,   429,   252,   457,   165,   245,
     298,   190,   191,   512,   771,   192,   193,   502,   641,   267,
     213,   267,   214,   534,   267,   535,   267,   166,   536,   602,
     537,   581,   172,   152,   542,   153,   154,   271,   654,   602,
     240,   155,   174,   272,   241,   589,   111,   216,   652,   255,
     667,   710,   302,   212,   718,   112,   235,   417,   650,   761,
     776,   653,   156,   418,   711,   257,   257,   152,   571,   153,
     154,   479,   762,   777,   580,   155,   232,   418,   337,   439,
     211,   302,   222,   211,   223,   211,   211,   660,   660,   288,
     211,     7,   337,   353,   655,     8,   156,   354,  -177,   285,
     158,   252,   325,   258,   211,   258,   597,   462,   258,   463,
     258,   712,   346,   347,   152,   306,   153,   154,   211,   211,
     211,   541,   155,   701,   280,   483,   308,   484,   272,   257,
      74,   663,   353,    14,    15,    74,   354,   429,   419,   294,
     376,   376,   418,   156,   255,    76,   307,   158,   256,   322,
      76,   480,   211,   211,   606,   418,   506,   507,   272,   211,
     217,   218,   219,   220,   593,   785,   594,   333,   679,   267,
     680,   546,   547,   630,   642,   714,   252,   715,   764,   381,
     381,   143,   143,   309,   749,   428,   211,   211,   211,   152,
     310,   153,   154,   230,   703,   704,   211,   155,   381,   311,
     143,   703,   742,   283,   284,   660,   778,   779,   312,   651,
     313,   314,   333,   211,   294,   324,   350,   352,   156,   255,
     429,   272,   158,   256,   369,   371,   372,   373,   374,   383,
     253,   390,   409,   211,   389,    74,   408,   415,  -446,   416,
     430,   452,   453,   455,   752,   468,   471,   450,   436,   454,
      76,   469,   784,   258,   472,   473,   474,   230,   230,   230,
     211,   211,   211,   345,   475,   252,   476,   433,   677,   478,
    -337,   376,   481,   700,   385,   702,   477,   491,   152,   211,
     153,   154,   774,   211,   482,   211,   155,   490,   211,   381,
     381,   143,   143,   494,   498,   230,   505,   499,   211,   689,
     514,   516,   515,   423,   663,   597,   517,   156,   255,   518,
     211,   730,   211,   519,   526,   528,   211,   211,   529,   230,
     538,   539,   211,   677,   230,   230,   211,   211,   211,   540,
     244,   441,   543,   542,   211,   385,   346,   211,   245,   504,
     385,   385,   551,   552,   230,   553,   337,   211,   555,   381,
     556,   143,   557,   211,   559,   211,   527,   570,   211,   600,
     767,   530,   689,   533,   211,   257,   354,   607,   257,   677,
     257,   211,   677,   211,   604,   230,   608,   612,   211,   335,
     613,   617,   376,   615,   621,   623,   211,   230,   632,   634,
     211,   730,   635,   645,   211,   646,   664,   666,   673,   691,
     699,   707,   708,   385,   713,   734,   677,   381,   677,   143,
     721,   736,   740,   112,   769,   765,   287,   766,   563,   563,
     770,   773,  -217,   775,   315,   783,   757,   743,   431,   706,
      85,    39,    86,    87,    88,    89,   588,    90,   661,    40,
      91,   750,   382,    92,    93,    94,    95,    96,    28,    97,
     590,    42,   611,    98,   592,    43,    99,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,   717,    54,
      55,    56,   782,   420,    57,   672,   360,   101,    58,    59,
      60,    61,    62,    63,   102,   599,   728,   720,   759,   572,
     103,   690,   681,   780,   733,   129,   443,   693,   692,   696,
     401,   591,   368,   104,   629,   525,   763,   627,   524,   105,
     614,   404,     0,   257,     0,     0,   106,     0,   563,   107,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,   110,     0,   111,     0,
       0,     0,     0,     0,     0,     0,    65,   112,     0,     0,
      67,   113,     0,     0,     0,     0,   114,   115,   116,   117,
       0,     0,   118,     0,   668,     0,   119,   120,     0,    85,
      39,    86,     0,   582,     0,     0,    90,     0,    40,    91,
       0,     0,    92,    93,    94,     0,    96,     0,     0,     0,
      42,   563,   583,   694,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,   102,     0,     0,     0,     0,     0,   103,
       0,     0,     0,     0,     0,     0,   252,     0,   292,     0,
       0,   293,   104,     0,     0,     0,     0,     0,   105,   152,
       0,   153,   154,     0,     0,   106,     0,   155,   107,   108,
       0,     0,     0,   748,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,   294,   110,     0,   111,   156,   255,
       0,     0,   158,   256,     0,    65,   112,     0,     0,    67,
     113,     0,   768,     0,     0,   114,   115,   116,   117,     0,
       0,   118,    85,    39,    86,   119,   120,     0,     0,    90,
       0,    40,    91,     0,     0,    92,    93,    94,     0,    96,
       0,     0,     0,    42,     0,   656,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,   102,     0,     0,     0,
       0,     0,   103,     0,     0,     0,     0,     0,     0,   150,
       0,     0,     0,     0,     0,   104,     0,     0,     0,     0,
     151,   105,   152,     0,   153,   154,     0,     0,   106,     0,
     155,   107,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   110,     0,
     111,   156,   157,     0,     0,   158,   159,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,   119,   120,
       0,     0,    90,     0,    40,    91,     0,     0,    92,    93,
      94,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,   102,
       0,     0,     0,     0,     0,   103,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   104,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,   106,     0,     0,   107,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,   110,     0,   111,     0,     0,     0,     0,     0,     0,
       0,    65,   112,     0,     0,    67,   113,     0,     0,     0,
       0,   114,   115,   116,   117,     0,    22,   118,    85,    39,
      86,   119,   120,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,     0,   106,     0,     0,   107,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    23,
     109,     0,     0,     0,   173,     0,   111,     0,     0,     0,
     562,     0,     0,     0,    65,   112,     0,     0,    67,   113,
       0,     0,     0,     0,   114,   115,   116,   117,     0,     0,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   252,     0,     0,
     105,     0,     0,     0,     0,     0,     0,   106,     0,     0,
     270,   108,   153,   154,     0,     0,     0,     0,   155,     0,
       0,     0,     0,   109,     0,     0,     0,   173,   271,   111,
       0,     0,     0,     0,   272,   254,     0,    65,   112,   156,
     255,    67,   113,   158,   256,     0,     0,   114,   115,   116,
     117,     0,     0,   118,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     252,     0,     0,   105,     0,     0,     0,     0,     0,     0,
     106,     0,     0,   107,   108,   153,   154,     0,     0,     0,
       0,   155,     0,     0,     0,     0,   109,   253,     0,     0,
     173,     0,   111,     0,     0,     0,     0,     0,   254,     0,
      65,   112,   156,   255,    67,   113,   158,   256,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   252,     0,     0,   105,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   107,   108,   153,   154,
       0,     0,     0,     0,   155,     0,     0,     0,     0,   109,
       0,     0,     0,   173,     0,   111,     0,     0,     0,     0,
       0,   254,     0,    65,   112,   156,   255,    67,   113,   158,
     256,     0,     0,   114,   115,   116,   117,     0,     0,   118,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,     0,     0,     0,     0,     0,   106,     0,     0,   107,
     108,     0,     0,     0,     0,     0,     0,     0,     0,   561,
       0,     0,   109,     0,     0,     0,   173,     0,   111,     0,
       0,     0,   562,     0,     0,     0,    65,   112,     0,     0,
      67,   113,     0,     0,     0,     0,   114,   115,   116,   117,
       0,     0,   118,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
     361,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,   362,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,   106,
       0,     0,   107,   108,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   173,
       0,   111,     0,     0,     0,     0,     0,     0,     0,    65,
     112,     0,     0,    67,   113,     0,     0,     0,     0,   114,
     115,   116,   117,     0,     0,   118,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,     0,   106,     0,     0,   107,   108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,   173,     0,   111,     0,     0,     0,   562,     0,
       0,     0,    65,   112,     0,     0,    67,   113,     0,     0,
       0,     0,   114,   115,   116,   117,     0,     0,   118,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,     0,   106,     0,     0,   107,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,   173,     0,   111,     0,     0,
       0,     0,     0,     0,     0,    65,   112,     0,     0,    67,
     113,     0,     0,     0,     0,   114,   115,   116,   117,     0,
       0,   118,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,   106,     0,
       0,   107,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   173,     0,
     111,     0,     0,     0,     0,     0,     0,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,   173,     0,   111,     0,     0,     0,     0,     0,     0,
       0,    65,   112,     0,     0,    67,   113,     0,     0,     0,
       0,   114,   115,   116,   117,     0,     0,   118,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,   304,   106,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,   173,     0,   111,     0,     0,     0,
       0,     0,     0,     0,    65,   112,     0,     0,    67,   113,
       0,     0,     0,     0,   114,   115,   116,   117,     0,     0,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,   400,     0,     0,   106,     0,     0,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,   173,     0,   111,
       0,     0,     0,     0,     0,     0,     0,    65,   112,     0,
       0,    67,   113,     0,     0,     0,     0,   114,   115,   116,
     117,     0,     0,   118,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
     173,     0,   111,     0,    39,     0,     0,     0,     0,     0,
      65,   112,    40,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,    42,     0,   118,     0,   334,     0,
      44,    45,    46,   176,   177,   178,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
       0,     0,     0,     0,     0,     0,   340,     0,     0,   180,
     341,     0,   181,   182,     0,   183,     0,     0,     0,     0,
       0,   184,     0,     0,     0,   185,     0,     0,     0,   186,
     342,   187,     0,     0,     0,    39,   272,   188,     0,   189,
      66,   190,   191,    40,    68,   192,   193,     0,     0,     0,
     226,     0,     0,     0,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,   227,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,   181,   182,     0,   183,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,     0,    39,     0,
     186,     0,   187,     0,     0,     0,    40,     0,   188,     0,
     189,    66,   190,   191,     0,    68,   192,   193,    42,     0,
       0,     0,   334,     0,    44,    45,    46,   176,   177,   178,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,   181,   182,     0,   183,
       0,     0,     0,     0,     0,   184,     0,     0,     0,   185,
     335,     0,    39,   186,     0,   187,     0,     0,     0,     0,
      40,   188,     0,   189,    66,   190,   191,   648,    68,   192,
     193,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   176,   177,   178,     0,   649,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
     181,   182,     0,   183,     0,     0,     0,     0,     0,   184,
       0,     0,     0,   185,     0,    39,     0,   186,     0,   187,
       0,     0,     0,    40,     0,   188,     0,   189,    66,   190,
     191,     0,    68,   192,   193,    42,     0,     0,     0,   334,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,   181,   182,     0,   183,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,     0,    39,     0,
     186,     0,   187,     0,     0,     0,    40,     0,   188,     0,
     189,    66,   190,   191,     0,    68,   192,   193,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   176,   177,   178,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,   181,   182,     0,   183,
       0,     0,     0,     0,     0,   184,     0,     0,     0,   185,
       0,    39,     0,   186,   729,   187,     0,     0,     0,    40,
       0,   188,     0,   189,    66,   190,   191,     0,    68,   192,
     193,    42,     0,     0,     0,   334,     0,    44,    45,    46,
     176,   177,   178,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,   181,
     182,     0,   183,     0,     0,     0,     0,     0,   184,     0,
       0,     0,   185,     0,    39,     0,   745,     0,   187,     0,
       0,     0,    40,     0,   188,     0,   189,    66,   190,   191,
       0,    68,   192,   193,    42,     0,     0,     0,     0,     0,
      44,    45,    46,   176,   177,   178,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   753,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,   181,   182,     0,   183,     0,     0,     0,     0,
       0,   184,     0,     0,     0,   185,     0,    39,     0,   186,
       0,   187,     0,     0,     0,    40,     0,   188,     0,   189,
      66,   190,   191,     0,    68,   192,   193,    42,     0,     0,
       0,     0,     0,    44,    45,    46,   176,   177,   178,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   756,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,     0,   181,   182,     0,   183,     0,
       0,     0,     0,     0,   184,     0,     0,     0,   185,     0,
      39,     0,   186,     0,   187,     0,     0,     0,    40,     0,
     188,     0,   189,    66,   190,   191,     0,    68,   192,   193,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   176,
     177,   178,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,   181,   182,
       0,   183,     0,     0,     0,     0,     0,   184,     0,     0,
       0,   185,     0,    39,     0,   186,     0,   187,     0,     0,
       0,    40,     0,   188,     0,   189,    66,   190,   191,     0,
      68,   192,   193,    42,     0,     0,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   252,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,     0,
    -178,     0,   182,     0,   183,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,    39,     0,     0,   186,     0,
     187,     0,     0,    40,     0,     0,   722,     0,   189,    66,
       0,   255,     0,    68,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     252,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,    39,     0,     0,
     186,     0,   187,     0,     0,    40,     0,     0,   722,     0,
     189,    66,     0,   255,     0,    68,     0,    42,     0,     0,
       0,     0,     0,    44,    45,    46,   176,   177,   178,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   355,   180,     0,     0,     0,   182,     0,   183,     0,
       0,     0,     0,     0,   184,    39,     0,     0,   185,     0,
       0,     0,   186,    40,   187,     0,     0,     0,     0,     0,
       0,     0,   189,    66,     0,    42,     0,    68,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,    39,     0,     0,
     186,     0,   187,     0,     0,    40,     0,     0,     0,     0,
     189,    66,     0,     0,    41,    68,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    39,    54,    55,    56,     0,     0,
      57,     0,    40,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,    64,    40,     0,     0,     0,     0,   316,     0,
       0,     0,    65,    66,     0,    42,    67,    68,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,    64,
      40,     0,    58,    59,    60,    61,    62,    63,     0,    65,
      66,     0,    42,    67,    68,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
      64,    40,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,     0,    42,    67,    68,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,    65,   112,     0,
      42,    67,   113,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,    39,    54,    55,
      56,     0,     0,    57,     0,    40,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,   446,    54,    55,    56,   189,    66,
      57,     0,     0,    68,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,     0,    40,   238,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,    42,     0,     0,    67,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,    39,    54,    55,    56,     0,     0,    57,
       0,    40,   238,    58,    59,    60,    61,    62,    63,     0,
       0,     0,    65,    42,     0,     0,    67,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   176,
     177,   178,     0,     0,     0,    52,    53,     0,    54,    55,
      56,    65,   112,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   328,     0,
       0,     0,     0,    39,     0,     0,     0,     0,    65,     0,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,     0,   573,   574,     0,    44,
      45,    46,   176,   177,   178,   575,    39,     0,    52,    53,
       0,    54,    55,    56,    40,   189,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,   719,
     574,     0,     0,     0,    40,     0,     0,     0,   575,     0,
       0,     0,     0,     0,     0,     0,    42,     0,   189,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,    39,
       0,    52,    53,   574,    54,    55,    56,    40,     0,    57,
       0,   575,     0,    58,    59,    60,    61,    62,    63,    42,
       0,   189,     0,     0,     0,    44,    45,    46,   176,   177,
     178,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   575,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   189
  };

  const short
  parser::yycheck_[] =
  {
      87,   226,   111,   241,   186,    29,    64,   104,    98,   325,
     166,    98,   470,    87,   184,   104,   222,   223,   216,   188,
      29,   109,   110,    98,    98,   109,    41,   436,   291,   100,
      34,   131,    34,    87,     5,    64,   293,   107,   101,   185,
       1,    19,   105,   106,    21,   108,   472,     1,   286,   507,
     299,   459,   110,   361,    27,   675,     1,     1,    77,     1,
       1,    77,     1,   110,    27,    77,   212,    82,   463,    90,
     133,   321,     0,   109,    80,   175,    77,   186,   187,    22,
     506,   110,    12,   254,   120,   173,   185,   186,   124,   721,
     115,   254,   724,   180,   103,   182,   102,   116,   185,   186,
      14,   188,   131,   675,   116,   109,   110,   704,    51,    52,
     131,    84,   199,   212,   130,   173,   574,   575,   368,   102,
     546,    84,   221,   294,   101,   212,   173,   105,   180,   130,
     182,   294,   382,   441,   221,   109,   226,   227,   758,   226,
     227,   165,   166,   104,   173,   742,   175,   199,    80,   406,
     104,   559,   226,   227,   115,   404,   165,   166,   258,   104,
     104,   115,   104,   104,   188,   104,   109,   562,    80,   173,
     115,   115,    78,   115,   115,    80,    80,   120,    84,   109,
     238,   124,   184,   282,   116,   248,   758,   250,   760,   106,
     101,   290,   103,   130,    79,   282,    80,   168,   169,   116,
     270,    86,   289,   290,   116,    90,    80,    92,   105,   238,
     116,   116,   116,   463,   672,   289,    77,   304,   222,   223,
     222,   223,    80,   539,   540,   110,   325,   236,   286,   258,
     639,   116,   116,   130,   109,   289,   121,   122,   325,    77,
     125,   126,   116,   389,   307,   120,   704,    80,    27,   124,
     110,   514,   304,    84,   353,    86,   116,   105,   116,   263,
      24,   109,   352,   111,   354,   352,   353,   354,   355,    79,
     328,   697,   120,   699,   283,   284,   124,   703,   352,   102,
      90,   381,    92,   116,   742,   694,   115,   745,   110,   291,
     389,   362,   115,   110,   116,   552,   502,   306,   454,   469,
     387,   398,   389,   355,   102,   543,    79,   322,   116,   398,
     410,   121,   122,   388,   740,   125,   126,   115,   568,   407,
     101,   409,   103,   407,   412,   409,   414,   109,   412,   511,
     414,   477,   115,    92,   423,    94,    95,   110,   588,   521,
     105,   100,    86,   116,   109,   491,   111,    91,   102,   122,
     607,   102,   381,   109,   670,   120,   105,   110,   583,   102,
     102,   115,   121,   116,   115,   109,   110,    92,   467,    94,
      95,   110,   115,   115,   473,   100,    14,   116,   477,   636,
     467,   410,   101,   470,   103,   472,   473,   593,   594,    71,
     477,   120,   491,    86,   592,   124,   121,    90,    91,   131,
     125,    79,    80,   407,   491,   409,   505,   101,   412,   103,
     414,   661,   521,   522,    92,   101,    94,    95,   505,   506,
     507,   423,   100,   648,   112,   101,   130,   103,   116,   173,
     454,   600,    86,    65,    66,   459,    90,   675,   112,   117,
     539,   540,   116,   121,   122,   454,    87,   125,   126,     4,
     459,   112,   539,   540,   112,   116,    80,    81,   116,   546,
      72,    73,    74,    75,   101,   781,   103,   469,   101,   557,
     103,    80,    81,   557,   573,   101,    79,   103,   728,   483,
     484,   483,   484,   110,   709,   543,   573,   574,   575,    92,
     110,    94,    95,   583,    80,    81,   583,   100,   502,   110,
     502,    80,    81,   119,   120,   711,   756,   757,   110,   583,
     110,   110,   514,   600,   117,    23,    27,    91,   121,   122,
     758,   116,   125,   126,   115,    77,    77,    77,    77,    15,
     106,    41,   116,   620,    81,   559,   106,   110,   110,   110,
     130,   110,   110,   102,   713,   102,   106,   556,   292,   116,
     559,   116,   777,   557,    80,    91,   110,   647,   648,   649,
     647,   648,   649,   745,   110,    79,   110,   666,   620,   110,
      85,   670,   117,   647,   648,   649,   116,   116,    92,   666,
      94,    95,   751,   670,   117,   672,   100,   110,   675,   593,
     594,   593,   594,    85,    25,   685,    81,    27,   685,   623,
     130,   106,   131,   117,   773,   704,    77,   121,   122,    77,
     697,   685,   699,   110,    77,   117,   703,   704,   117,   709,
      77,    80,   709,   675,   714,   715,   713,   714,   715,    80,
     727,    27,   116,   722,   721,   709,   745,   724,   727,   383,
     714,   715,    81,   116,   734,   117,   745,   734,   102,   653,
     116,   653,    81,   740,   109,   742,   400,   110,   745,    84,
     734,   405,   686,   407,   751,   409,    90,   116,   412,   721,
     414,   758,   724,   760,   106,   765,    78,   117,   765,   106,
     117,    28,   781,    77,    27,     9,   773,   777,   110,   102,
     777,   765,   115,   110,   781,    37,    27,    80,    84,   110,
      80,    11,    19,   777,    86,    53,   758,   711,   760,   711,
      91,    86,    80,   120,   102,   116,   122,   110,   462,   463,
     110,    84,    84,   102,   165,   773,   715,   702,   289,   653,
       3,     4,     5,     6,     7,     8,   484,    10,   594,    12,
      13,   711,   223,    16,    17,    18,    19,    20,    15,    22,
     494,    24,   540,    26,   498,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   666,    42,
      43,    44,   765,   282,    47,   617,   212,    50,    51,    52,
      53,    54,    55,    56,    57,   507,   680,   673,   721,   469,
      63,   625,   622,   762,   686,    34,   296,   636,   635,   639,
     248,   496,   214,    76,   556,   398,   727,   551,   397,    82,
     543,   258,    -1,   557,    -1,    -1,    89,    -1,   562,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,    -1,   608,    -1,   139,   140,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      24,   635,    26,   637,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    81,    -1,
      -1,    84,    76,    -1,    -1,    -1,    -1,    -1,    82,    92,
      -1,    94,    95,    -1,    -1,    89,    -1,   100,    92,    93,
      -1,    -1,    -1,   707,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,   117,   109,    -1,   111,   121,   122,
      -1,    -1,   125,   126,    -1,   119,   120,    -1,    -1,   123,
     124,    -1,   736,    -1,    -1,   129,   130,   131,   132,    -1,
      -1,   135,     3,     4,     5,   139,   140,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    26,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      90,    82,    92,    -1,    94,    95,    -1,    -1,    89,    -1,
     100,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,   121,   122,    -1,    -1,   125,   126,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,   139,   140,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,     1,   135,     3,     4,
       5,   139,   140,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
     115,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,   100,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,   110,   111,
      -1,    -1,    -1,    -1,   116,   117,    -1,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
     132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,    -1,   105,   106,    -1,    -1,
     109,    -1,   111,    -1,    -1,    -1,    -1,    -1,   117,    -1,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,   132,    -1,    -1,   135,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    94,    95,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,
      -1,   117,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,   115,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    46,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    -1,    -1,   135,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,   111,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,
      -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,
     124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,
      -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    86,    -1,    -1,    89,    -1,    -1,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,
      -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,   111,    -1,     4,    -1,    -1,    -1,    -1,    -1,
     119,   120,    12,    -1,   123,   124,    -1,    -1,    -1,    -1,
     129,   130,   131,   132,    24,    -1,   135,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,
      90,    -1,    92,    93,    -1,    95,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
     110,   111,    -1,    -1,    -1,     4,   116,   117,    -1,   119,
     120,   121,   122,    12,   124,   125,   126,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    -1,     4,    -1,
     109,    -1,   111,    -1,    -1,    -1,    12,    -1,   117,    -1,
     119,   120,   121,   122,    -1,   124,   125,   126,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
     106,    -1,     4,   109,    -1,   111,    -1,    -1,    -1,    -1,
      12,   117,    -1,   119,   120,   121,   122,    19,   124,   125,
     126,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,     4,    -1,   109,    -1,   111,
      -1,    -1,    -1,    12,    -1,   117,    -1,   119,   120,   121,
     122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    -1,     4,    -1,
     109,    -1,   111,    -1,    -1,    -1,    12,    -1,   117,    -1,
     119,   120,   121,   122,    -1,   124,   125,   126,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,     4,    -1,   109,   110,   111,    -1,    -1,    -1,    12,
      -1,   117,    -1,   119,   120,   121,   122,    -1,   124,   125,
     126,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,    -1,     4,    -1,   109,    -1,   111,    -1,
      -1,    -1,    12,    -1,   117,    -1,   119,   120,   121,   122,
      -1,   124,   125,   126,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,     4,    -1,   109,
      -1,   111,    -1,    -1,    -1,    12,    -1,   117,    -1,   119,
     120,   121,   122,    -1,   124,   125,   126,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    78,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,
       4,    -1,   109,    -1,   111,    -1,    -1,    -1,    12,    -1,
     117,    -1,   119,   120,   121,   122,    -1,   124,   125,   126,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,    -1,     4,    -1,   109,    -1,   111,    -1,    -1,
      -1,    12,    -1,   117,    -1,   119,   120,   121,   122,    -1,
     124,   125,   126,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,    -1,
     111,    -1,    -1,    12,    -1,    -1,   117,    -1,   119,   120,
      -1,   122,    -1,   124,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,     4,    -1,    -1,
     109,    -1,   111,    -1,    -1,    12,    -1,    -1,   117,    -1,
     119,   120,    -1,   122,    -1,   124,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,   101,     4,    -1,    -1,   105,    -1,
      -1,    -1,   109,    12,   111,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,    -1,    24,    -1,   124,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,     4,    -1,    -1,
     109,    -1,   111,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     119,   120,    -1,    -1,    21,   124,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,    -1,    12,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    12,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,   119,   120,    -1,    24,   123,   124,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,   109,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,   119,
     120,    -1,    24,   123,   124,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,    -1,    24,   123,   124,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,   119,   120,    -1,
      24,   123,   124,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    78,    42,    43,    44,   119,   120,
      47,    -1,    -1,   124,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    24,    -1,    -1,   123,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,   109,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,   119,    24,    -1,    -1,   123,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,   119,   120,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,   119,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,   100,   101,    -1,    30,
      31,    32,    33,    34,    35,   109,     4,    -1,    39,    40,
      -1,    42,    43,    44,    12,   119,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,   100,
     101,    -1,    -1,    -1,    12,    -1,    -1,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,   119,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,     4,
      -1,    39,    40,   101,    42,    43,    44,    12,    -1,    47,
      -1,   109,    -1,    51,    52,    53,    54,    55,    56,    24,
      -1,   119,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   101,   142,   143,   144,   147,   120,   124,   341,
     148,   159,     0,   148,    65,    66,   145,   102,   115,   149,
     160,   161,     1,   104,   340,   105,   130,   211,   211,   109,
     150,    14,   162,   170,   171,   130,   212,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   109,   119,   120,   123,   124,   151,
     152,   153,   157,   308,   311,   312,   325,   326,   327,   333,
      27,    24,   163,   115,   158,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    63,    76,    82,    89,    92,    93,   105,
     109,   111,   120,   124,   129,   130,   131,   132,   135,   139,
     140,   168,   172,   173,   174,   175,   177,   192,   216,   256,
     261,   265,   266,   268,   269,   270,   271,   297,   298,   301,
     302,   324,   325,   327,   335,   336,   339,   106,   116,   341,
      79,    90,    92,    94,    95,   100,   121,   122,   125,   126,
     330,   331,   332,   334,   110,   116,   109,   154,   101,   103,
     146,   341,   115,   109,   264,   265,    33,    34,    35,    79,
      89,    92,    93,    95,   101,   105,   109,   111,   117,   119,
     121,   122,   125,   126,   197,   221,   223,   225,   226,   228,
     230,   232,   306,   307,   309,   311,   313,   314,   321,   322,
     323,   333,   109,   101,   103,   290,   264,    72,    73,    74,
      75,   178,   101,   103,   208,   209,    19,    37,   176,   223,
     225,   307,    14,   290,   269,   105,   262,   263,   109,   325,
     105,   109,   299,   300,   302,   336,   269,   288,   289,   269,
     268,   269,    79,   106,   117,   122,   126,   264,   265,   272,
     274,   275,   304,   318,   320,   329,   330,   332,   337,   338,
      92,   110,   116,   272,   273,   330,   331,   332,   337,   342,
     112,   342,    19,   262,   262,   131,   167,   158,    71,   198,
      80,   116,    81,    84,   117,   258,   259,   260,   304,   317,
     319,   328,   330,   331,    88,   269,   101,    87,   130,   110,
     110,   110,   110,   110,   110,   153,    78,   155,   156,   157,
     148,   148,     4,   164,    23,    80,   232,   232,   109,   216,
     249,   250,   251,   327,    28,   106,   218,   219,   221,   223,
      86,    90,   110,   218,   236,   313,   342,   342,   311,   323,
      27,   202,    91,    86,    90,    88,   229,   232,   218,   235,
     236,    20,    46,   264,   287,   291,   292,   293,   291,   115,
     267,    77,    77,    77,    77,   214,   219,   233,   207,   256,
     257,   265,   207,    15,   187,   223,   223,    80,   116,    81,
      41,    90,   131,   325,    77,   130,   338,    80,   116,   213,
      86,   288,   326,   335,   317,    78,    84,   116,   106,   116,
     265,   110,   116,   110,   116,   110,   110,   110,   116,   112,
     233,   325,   325,   117,   169,   303,   315,   316,   331,   338,
     130,   197,   215,   219,   220,   324,   264,   277,   278,   293,
     326,    27,   210,   260,   266,   232,    78,   294,   295,   296,
     325,   269,   110,   110,   116,   102,   340,   341,    12,   109,
     165,   166,   101,   103,   279,   214,   331,    80,   102,   116,
     237,   106,    80,    91,   110,   110,   110,   116,   110,   110,
     112,   117,   117,   101,   103,   201,   223,   219,   225,   232,
     110,   116,   209,   290,    85,   102,   115,   340,    25,    27,
     206,   102,   115,   340,   264,    81,    80,    81,   195,   219,
     240,   109,   307,   218,   130,   131,   106,    77,    77,   110,
     105,   109,   111,   305,   306,   299,    77,   264,   117,   117,
     264,   276,   293,   264,   272,   272,   272,   272,    77,    80,
      80,   327,   336,   116,    77,   130,    80,    81,   193,   244,
     210,    81,   116,   117,   209,   102,   116,    81,   157,   109,
     151,   102,   115,   264,   280,   281,   282,   286,   280,   340,
     110,   219,   251,   100,   101,   109,   238,   239,   321,   240,
     219,   218,     7,    26,   188,   199,   200,   257,   200,   218,
     264,   292,   264,   101,   103,   205,   257,   219,   240,   238,
      84,   181,   313,   324,   106,   110,   112,   116,    78,   214,
     217,   217,   117,   117,   315,    77,   240,    28,   245,   246,
     247,    27,   241,     9,   252,   253,   254,   264,   293,   295,
     272,   151,   110,   280,   102,   115,    84,    86,   283,   284,
     285,   340,   219,   321,   321,   110,    37,   189,    19,    37,
     187,   223,   102,   115,   340,   267,    26,   191,   203,   204,
     257,   204,   182,   323,    27,   184,    80,   293,   264,    77,
     116,    77,   237,    84,   222,   227,   231,   232,   248,   101,
     103,   252,    22,    51,    52,   109,   179,   255,   310,   311,
     254,   110,   282,   277,   264,   210,   285,    80,   102,    80,
     223,   187,   223,    80,    81,   196,   199,    11,    19,   190,
     102,   115,   340,    86,   101,   103,   185,   215,   214,   100,
     246,    91,   117,   231,   303,   242,   243,   267,   242,   110,
     223,   224,   234,   255,    53,   180,    86,   210,   240,   240,
      80,   194,    81,   196,   240,   109,   239,   321,   264,   187,
     203,   183,   323,    78,   186,   187,    78,   186,   227,   248,
     227,   102,   115,   300,   340,   116,   110,   223,   264,   102,
     110,   240,   321,    84,   323,   102,   102,   115,   340,   340,
     243,    80,   234,   182,   187,   214
  };

  const short
  parser::yyr1_[] =
  {
       0,   141,   142,   143,   143,   144,   145,   145,   145,   146,
     146,   147,   147,   148,   149,   149,   149,   150,   150,   151,
     152,   152,   153,   153,   154,   154,   154,   155,   155,   156,
     156,   157,   157,   158,   158,   159,   159,   160,   161,   161,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   167,
     167,   168,   168,   168,   169,   169,   170,   171,   171,   172,
     172,   172,   172,   172,   172,   172,   172,   173,   174,   174,
     174,   174,   175,   176,   176,   177,   177,   178,   178,   178,
     178,   178,   179,   179,   179,   180,   181,   181,   182,   183,
     183,   184,   184,   185,   185,   185,   185,   186,   186,   186,
     186,   187,   188,   188,   188,   188,   188,   189,   189,   190,
     190,   191,   192,   192,   193,   193,   194,   194,   195,   195,
     195,   196,   196,   196,   197,   197,   198,   198,   198,   199,
     199,   200,   200,   200,   200,   201,   201,   202,   202,   203,
     203,   204,   204,   204,   204,   205,   205,   206,   206,   207,
     207,   207,   207,   208,   208,   209,   210,   210,   211,   211,
     212,   212,   212,   213,   213,   214,   215,   216,   216,   217,
     217,   218,   218,   219,   219,   219,   220,   221,   222,   223,
     223,   224,   225,   226,   226,   227,   227,   228,   228,   228,
     228,   229,   230,   230,   231,   232,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   233,   234,   234,   235,   235,
     236,   236,   237,   237,   238,   238,   238,   239,   239,   240,
     241,   241,   241,   242,   242,   243,   244,   245,   245,   246,
     246,   247,   247,   248,   248,   249,   249,   250,   250,   251,
     252,   252,   253,   253,   254,   254,   254,   255,   255,   255,
     256,   256,   257,   258,   258,   259,   259,   260,   261,   261,
     261,   261,   261,   261,   261,   261,   261,   262,   262,   263,
     263,   264,   264,   265,   265,   266,   266,   267,   267,   268,
     268,   268,   268,   269,   269,   269,   269,   269,   269,   269,
     269,   269,   269,   270,   270,   271,   271,   271,   271,   271,
     271,   271,   272,   272,   272,   273,   273,   274,   274,   274,
     274,   274,   274,   274,   275,   275,   276,   276,   277,   278,
     278,   279,   279,   279,   279,   280,   280,   281,   281,   281,
     282,   283,   283,   284,   284,   285,   286,   287,   288,   289,
     289,   290,   290,   291,   291,   291,   291,   292,   292,   293,
     293,   293,   294,   294,   295,   295,   295,   296,   296,   297,
     297,   298,   298,   299,   299,   299,   300,   300,   301,   301,
     301,   301,   302,   302,   303,   303,   304,   304,   305,   305,
     305,   306,   306,   306,   306,   306,   307,   307,   307,   308,
     308,   308,   308,   308,   309,   309,   310,   311,   311,   312,
     313,   313,   313,   314,   314,   314,   314,   315,   315,   316,
     316,   317,   317,   318,   318,   319,   319,   320,   320,   321,
     322,   323,   323,   323,   323,   323,   324,   324,   325,   325,
     325,   326,   326,   327,   327,   327,   327,   327,   327,   327,
     327,   328,   328,   329,   329,   330,   331,   331,   332,   332,
     333,   333,   333,   333,   333,   333,   333,   333,   333,   333,
     333,   333,   333,   333,   333,   333,   333,   333,   334,   334,
     334,   335,   335,   336,   337,   337,   338,   338,   339,   339,
     339,   339,   339,   340,   340,   341,   341,   342,   342
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     0,     3,     3,     0,     1,     3,
       1,     1,     1,     2,     1,     2,     0,     2,     3,     0,
       5,     1,     0,     2,     0,     1,     0,     3,     4,     0,
       1,     1,     1,     1,     3,     1,     2,     3,     0,     1,
       1,     1,     1,     4,     7,     1,     1,     3,     4,     5,
       6,     6,     4,     3,     1,     4,     3,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     0,     2,     3,     2,
       1,     0,     2,     3,     3,     3,     3,     3,     2,     1,
       0,     3,     4,     3,     4,     2,     3,     0,     1,     0,
       1,     3,     1,     1,     0,     2,     0,     2,     0,     2,
       2,     0,     2,     4,     3,     1,     4,     3,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     3,
       2,     1,     0,     3,     3,     1,     2,     0,     1,     3,
       3,     1,     0,     0,     2,     1,     1,     3,     1,     1,
       3,     1,     3,     4,     3,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     3,     1,     2,     1,     1,     2,
       3,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       3,     2,     5,     3,     3,     1,     1,     3,     1,     0,
       1,     3,     2,     0,     1,     3,     5,     1,     5,     1,
       4,     4,     0,     3,     1,     4,     2,     3,     1,     4,
       2,     3,     0,     1,     3,     0,     1,     3,     1,     3,
       0,     1,     2,     1,     2,     3,     3,     1,     2,     3,
       1,     2,     1,     3,     2,     2,     1,     4,     3,     3,
       4,     4,     3,     4,     6,     6,     4,     0,     1,     3,
       4,     3,     1,     1,     3,     2,     1,     1,     0,     2,
       3,     2,     1,     3,     2,     2,     4,     4,     8,     4,
       2,     2,     1,     4,     1,     1,     1,     1,     3,     3,
       3,     1,     1,     2,     2,     3,     3,     1,     1,     2,
       4,     3,     5,     3,     3,     3,     3,     1,     1,     3,
       1,     3,     3,     2,     2,     1,     2,     3,     2,     1,
       2,     3,     2,     2,     1,     4,     1,     1,     1,     2,
       1,     3,     3,     3,     2,     1,     0,     1,     2,     3,
       1,     2,     1,     0,     3,     1,     1,     3,     1,     1,
       1,     1,     3,     1,     3,     1,     3,     1,     2,     3,
       2,     3,     1,     2,     1,     3,     1,     3,     1,     2,
       2,     1,     3,     3,     3,     2,     1,     3,     3,     1,
       3,     3,     3,     3,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     3,     1,     3,     1,
       3,     1,     1,     1,     1,     1,     1,     3,     1,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1
  };


#if YYDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "\"_\"", "\"as\"",
  "\"case\"", "\"class\"", "\"data\"", "\"default\"", "\"deriving\"",
  "\"do\"", "\"else\"", "\"hiding\"", "\"if\"", "\"import\"", "\"in\"",
  "\"infix\"", "\"infixl\"", "\"infixr\"", "\"instance\"", "\"let\"",
  "\"module\"", "\"newtype\"", "\"of\"", "\"qualified\"", "\"then\"",
  "\"type\"", "\"where\"", "\"forall\"", "\"foreign\"", "\"export\"",
  "\"label\"", "\"dynamic\"", "\"safe\"", "\"interruptible\"",
  "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"", "\"stdcall\"",
  "\"ccall\"", "\"bpcall\"", "\"capi\"", "\"prim\"", "\"javascript\"",
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"static\"",
  "\"stock\"", "\"anyclass\"", "\"via\"", "\"unit\"", "\"signature\"",
  "\"dependency\"", "\"{-# INLINE\"", "\"{-# SPECIALIZE\"",
  "\"{-# SPECIALIZE_INLINE\"", "\"{-# SOURCE\"", "\"{-# RULES\"",
  "\"{-# CORE\"", "\"{-# SCC\"", "\"{-# GENERATED\"", "\"{-# DEPRECATED\"",
  "\"{-# WARNING\"", "\"{-# UNPACK\"", "\"{-# NOUNPACK\"", "\"{-# ANN\"",
  "\"{-# MINIMAL\"", "\"{-# CTYPE\"", "\"{-# OVERLAPPING\"",
  "\"{-# OVERLAPPABLE\"", "\"{-# OVERLAPS\"", "\"{-# INCOHERENT\"",
  "\"{-# COMPLETE\"", "\"#-}\"", "\"..\"", "\":\"", "\"::\"", "\"=\"",
  "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"", "TIGHT_INFIX_AT",
  "\"@\"", "PREFIX_TILDE", "\"~\"", "\"=>\"", "\"-\"", "PREFIX_BANG",
  "\"!\"", "\"*\"", "\"-<\"", "\">-\"", "\"-<<\"", "\">>-\"", "\".\"",
  "\"{\"", "\"}\"", "\"vocurly\"", "\"vccurly\"", "\"[\"", "\"]\"",
  "\"[:\"", "\":]\"", "\"(\"", "\")\"", "\"(#\"", "\"#)\"", "\"(|\"",
  "\"|)\"", "\";\"", "\",\"", "\"`\"", "\"'\"", "\"VARID\"", "\"CONID\"",
  "\"VARSYM\"", "\"CONSYM\"", "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"",
  "\"QCONSYM\"", "\"IPDUPVARID\"", "\"LABELVARID\"", "\"CHAR\"",
  "\"STRING\"", "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"",
  "\"PRIMSTRING\"", "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"",
  "\"PRIMDOUBLE\"", "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"",
  "$accept", "unit", "module", "missing_module_keyword", "maybemodwarning",
  "body", "body2", "top", "top1", "maybeexports", "exportlist",
  "exportlist1", "export", "export_subspec", "qcnames", "qcnames1",
  "qcname", "semis1", "semis", "importdecls", "importdecls_semi",
  "importdecl", "optqualified", "maybeas", "maybeimpspec", "impspec",
  "prec", "infix", "ops", "topdecls", "topdecls_semi", "topdecl",
  "cl_decl", "ty_decl", "standalone_kind_sig", "sks_vars", "inst_decl",
  "overlap_pragma", "deriv_strategy_no_via", "deriv_strategy_via",
  "opt_injective_info", "injectivity_cond", "inj_varids",
  "where_type_family", "ty_fam_inst_eqn_list", "ty_fam_inst_eqns",
  "ty_fam_inst_eqn", "at_decl_cls", "opt_family", "opt_instance",
  "at_decl_inst", "data_or_newtype", "opt_kind_sig",
  "opt_datafam_kind_sig", "opt_tyfam_kind_sig", "opt_at_kind_inj_sig",
  "tycl_hdr", "capi_ctype", "decl_cls", "decls_cls", "decllist_cls",
  "where_cls", "decl_inst", "decls_inst", "decllist_inst", "where_inst",
  "decls", "decllist", "binds", "wherebinds", "strings", "stringlist",
  "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars", "sigtypes1",
  "ktype", "ctype", "ctypedoc", "context", "context_no_ops", "type",
  "typedoc", "btype", "infixtype", "btype_no_ops", "ftype", "tyarg",
  "tyop", "atype_docs", "atype", "inst_type", "deriv_types",
  "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr",
  "tv_bndr_no_braces", "kind", "gadt_constrlist", "gadt_constrs",
  "gadt_constr", "constrs", "constrs1", "constr", "forall", "constr_stuff",
  "fielddecls", "fielddecls1", "fielddecl", "maybe_derivings", "derivings",
  "deriving", "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs",
  "gdrh", "sigdecl", "activation", "explicit_activation", "exp",
  "infixexp", "exp10", "optSemi", "fexp", "aexp", "aexp1", "aexp2", "texp",
  "tup_exprs", "list", "lexps", "squals", "guardquals", "guardquals1",
  "altslist", "alts", "alts1", "alt", "alt_rhs", "gdpats", "gdpat", "pat",
  "bindpat", "apat", "apats1", "stmtlist", "stmts", "stmt", "qual",
  "fbinds", "fbinds1", "fbind", "qcon", "gen_qcon", "con", "con_list",
  "sysdcon_no_list", "sysdcon", "conop", "qconop", "gtycon", "ntgtycon",
  "oqtycon", "oqtycon_no_varcon", "qtyconop", "qtycondoc", "qtycon",
  "tycon", "qtyconsym", "tyconsym", "op", "varop", "qop", "qopm", "qvarop",
  "qvaropm", "tyvar", "tyvarop", "tyvarid", "var", "qvar", "qvarid",
  "varid", "qvarsym", "qvarsym_no_minus", "qvarsym1", "varsym",
  "varsym_no_minus", "special_id", "special_sym", "qconid", "conid",
  "qconsym", "consym", "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   503,   503,   520,   521,   523,   527,   528,   529,   531,
     532,   534,   535,   538,   540,   541,   542,   550,   551,   553,
     555,   556,   558,   559,   561,   562,   563,   565,   566,   568,
     569,   571,   572,   576,   577,   579,   580,   582,   584,   585,
     587,   600,   601,   603,   604,   606,   607,   611,   612,   617,
     618,   620,   621,   622,   624,   625,   629,   631,   632,   634,
     635,   636,   637,   640,   641,   648,   650,   652,   654,   655,
     657,   658,   661,   663,   664,   667,   668,   672,   673,   674,
     675,   676,   678,   679,   680,   682,   693,   694,   696,   698,
     699,   703,   704,   706,   707,   708,   709,   711,   712,   713,
     714,   716,   719,   721,   723,   725,   726,   728,   728,   730,
     730,   734,   736,   737,   741,   742,   744,   745,   747,   748,
     749,   751,   752,   753,   757,   758,   760,   761,   762,   804,
     805,   807,   808,   809,   810,   812,   813,   815,   816,   818,
     819,   821,   822,   823,   824,   826,   827,   829,   830,   833,
     834,   835,   836,   838,   839,   841,   843,   844,   852,   853,
     855,   856,   857,   870,   871,   880,   882,   884,   885,   887,
     888,   897,   898,   900,   901,   903,   905,   914,   916,   918,
     919,   921,   924,   926,   927,   929,   930,   932,   933,   934,
     935,   937,   939,   940,   947,   954,   955,   956,   957,   958,
     959,   960,   961,   967,   968,   971,   973,   974,   976,   977,
     979,   980,   987,   988,   990,   991,   992,   995,   996,  1014,
    1020,  1021,  1022,  1024,  1025,  1027,  1029,  1031,  1032,  1034,
    1035,  1037,  1038,  1040,  1041,  1043,  1044,  1046,  1047,  1049,
    1051,  1052,  1054,  1055,  1057,  1058,  1059,  1061,  1062,  1063,
    1068,  1070,  1072,  1076,  1077,  1079,  1080,  1084,  1094,  1095,
    1097,  1098,  1099,  1100,  1101,  1102,  1103,  1106,  1107,  1109,
    1110,  1115,  1116,  1120,  1121,  1123,  1124,  1126,  1127,  1132,
    1133,  1134,  1135,  1138,  1139,  1140,  1141,  1142,  1144,  1146,
    1147,  1148,  1150,  1153,  1154,  1157,  1158,  1159,  1160,  1161,
    1166,  1167,  1173,  1174,  1175,  1180,  1181,  1199,  1200,  1201,
    1202,  1203,  1204,  1205,  1207,  1208,  1221,  1223,  1233,  1235,
    1236,  1239,  1240,  1241,  1242,  1244,  1245,  1247,  1248,  1249,
    1251,  1253,  1254,  1256,  1257,  1266,  1268,  1270,  1272,  1274,
    1275,  1278,  1279,  1281,  1282,  1283,  1284,  1289,  1290,  1292,
    1293,  1294,  1299,  1300,  1302,  1303,  1304,  1306,  1307,  1339,
    1340,  1342,  1343,  1345,  1346,  1347,  1349,  1350,  1352,  1353,
    1354,  1355,  1357,  1358,  1360,  1361,  1363,  1364,  1367,  1368,
    1369,  1371,  1372,  1373,  1374,  1375,  1377,  1378,  1379,  1381,
    1382,  1383,  1384,  1385,  1388,  1389,  1391,  1393,  1394,  1398,
    1400,  1401,  1402,  1404,  1405,  1406,  1407,  1412,  1413,  1415,
    1416,  1418,  1419,  1422,  1423,  1428,  1429,  1431,  1432,  1436,
    1438,  1440,  1441,  1442,  1443,  1444,  1447,  1448,  1450,  1451,
    1452,  1454,  1455,  1457,  1458,  1459,  1460,  1461,  1462,  1463,
    1464,  1466,  1467,  1469,  1470,  1472,  1474,  1475,  1477,  1478,
    1480,  1481,  1482,  1483,  1484,  1485,  1486,  1487,  1488,  1489,
    1490,  1491,  1492,  1493,  1494,  1495,  1496,  1497,  1499,  1500,
    1501,  1505,  1506,  1508,  1510,  1511,  1513,  1514,  1518,  1519,
    1520,  1521,  1522,  1527,  1530,  1534,  1535,  1537,  1538
  };

  void
  parser::yy_stack_print_ () const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  void
  parser::yy_reduce_print_ (int yyrule) const
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG


} // yy
#line 6935 "parser.cc"

#line 1547 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

pair<vector<Hs::ImpDecl>, optional<Hs::Decls>> make_body(const std::vector<Hs::ImpDecl>& imports, const std::optional<Hs::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Hs::Decls(*topdecls);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<Hs::Type>>
check_type_or_class_header(const Hs::Type& type)
{
    auto [type_head, type_args] = Hs::decompose_type_apps(type);

    // FIXME -- add location!
    if (not type_head.is_a<Hs::TypeCon>())
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    auto name = unloc(type_head.as_<Hs::TypeCon>().name);

    return {name, type_args};
}

vector<Hs::TypeVar> check_all_type_vars(const vector<Hs::Type>& types)
{
    vector<Hs::TypeVar> type_vars;
    for(auto& type: types)
    {
        if (type.is_a<Hs::TypeVar>())
        {
            type_vars.push_back(type.as_<Hs::TypeVar>());
        }
        else
        {
            throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
    }
    return type_vars;
}

Hs::TypeSynonymDecl make_type_synonym(const Located<Hs::Type>& lhs_type, const Located<Hs::Type>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, check_all_type_vars(type_args), rhs_type};
}

Hs::TypeFamilyDecl make_type_family(const Located<Hs::Type>& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig,
                                    const std::optional<std::vector<Hs::TypeFamilyInstanceEqn>>& eqns)
{
    auto [head, args] = Hs::decompose_type_apps(lhs_type.value());

    // Get type con
    auto con = head.to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family '"<<lhs_type.print()<<"' does not begin with a type constructor.";

    // Get args as type vars
    std::vector<Hs::TypeVar> tyvars;
    for(auto arg: args)
    {
        std::optional<Hs::Kind> kind;
        if (auto ktype = arg.to<Hs::TypeOfKind>())
        {
            arg = ktype->type;
            kind = ktype->kind;
        }

        if (auto tyvar = arg.to<Hs::TypeVar>())
        {
            auto tv = *tyvar;
            tv.kind = kind;
            tyvars.push_back(tv);
        }
        else
            throw myexception()<<"Type family '"<<lhs_type.print()<<"' argument '"<<arg.print()<<"' is not a type variable.";
    }

    std::optional<Hs::Kind> kind;
    if (kind_sig)
        kind = kind_sig->value();

    return Hs::TypeFamilyDecl(*con, tyvars, kind, eqns);
}

Hs::TypeFamilyInstanceEqn make_type_family_instance_eqn(const Located<Hs::Type>& lhs_type, const Located<Hs::Type>& rhs_type)
{
    auto [head, args] = Hs::decompose_type_apps(lhs_type.value());

    // Get type con
    auto con = head.to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family instance '"<<lhs_type.print()<<"' does not begin with a type constructor.";

    return Hs::TypeFamilyInstanceEqn(*con, args, rhs_type.value());
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::Type& header, const std::optional<Hs::Kind>& k, const Hs::ConstructorsDecl& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, k, constrs};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::Type& header, const std::optional<Hs::Kind>& k, const std::optional<Hs::GADTConstructorsDecl>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype)
    {
        if (not constrs or constrs->size() != 1 or (*constrs)[0].con_names.size() != 1)
            throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    }

    if (not constrs)
        return {d_or_n, name, check_all_type_vars(type_args), context, k};
    else
        return {d_or_n, name, check_all_type_vars(type_args), context, k, *constrs};
}

Hs::InstanceDecl make_instance_decl(const Located<Hs::Type>& ltype, const optional<Located<Hs::Decls>>& decls)
{
    // GHC stores the instance as a polytype?
    // This would seem to allow (instance forall a.Eq a => forall a.Eq [a] x y ....)

    auto type = unloc(ltype);
    if (type.is_a<Hs::ForallType>())
        throw myexception()<<"instance declaration '"<<type<<"' is malformed";
    Hs::Context context;
    if (type.is_a<Hs::ConstrainedType>())
    {
        auto& T = type.as_<Hs::ConstrainedType>();
        context = T.context;
        type = T.type;
    }

    std::vector<Hs::TypeFamilyInstanceDecl> type_inst_decls;
    Hs::Decls method_decls;
    if (decls)
        for(auto& decl: unloc(*decls))
        {
            if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                type_inst_decls.push_back(*TI);
            else if (auto V = decl.to<Hs::ValueDecl>())
                method_decls.push_back(*V);
            else
                throw myexception()<<"In declaration of instance "<<unloc(ltype).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {context, type, type_inst_decls, method_decls};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::Type& header, const optional<Located<Hs::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);

    std::vector<Hs::FixityDecl> fixity_decls;
    std::vector<Hs::TypeFamilyDecl> type_fam_decls;
    std::vector<Hs::TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<Hs::SignatureDecl> sig_decls;
    Hs::Decls default_method_decls;

    if (decls)
        for(auto& decl: unloc(*decls))
        {
            if (auto F = decl.to<Hs::FixityDecl>())
                fixity_decls.push_back(*F);
            else if (auto TF = decl.to<Hs::TypeFamilyDecl>())
                type_fam_decls.push_back(*TF);
            else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                default_type_inst_decls.push_back(*TI);
            else if (auto S = decl.to<Hs::SignatureDecl>())
                sig_decls.push_back(*S);
            else if (auto V = decl.to<Hs::ValueDecl>())
                default_method_decls.push_back(*V);
            else
                throw myexception()<<"In declaration of class "<<name<<", I don't recognize declaration:\n   "<<decl.print();
        }

    return {context, name, check_all_type_vars(type_args), fixity_decls, type_fam_decls, default_type_inst_decls, sig_decls, default_method_decls};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Hs::Context make_context(const Hs::Type& context)
{
    vector<Hs::Type> constraints;
    if (context.is_a<Hs::TupleType>())
    {
        constraints = context.as_<Hs::TupleType>().element_types;
    }
    else
        constraints.push_back(context);

    return {constraints};
}

std::optional<Hs::Kind> type_to_kind_(const Hs::Type& kind)
{
    auto [kind_head, kind_args] = Hs::decompose_type_apps(kind);

    if (not kind_head.is_a<Hs::TypeCon>()) return {};
    auto V = kind_head.as_<Hs::TypeCon>();
    auto head_name = unloc(V.name);

    if (kind_args.empty())
    {
        if (head_name == "*" or head_name == "Type")
            return kind_type();
        else
            return {};
    }
    else if (kind_args.size() == 2)
    {
        auto k1 = type_to_kind_(kind_args[0]);
        auto k2 = type_to_kind_(kind_args[1]);
        if (k1 and k2 and head_name == "->")
            return kind_arrow(*k1, *k2);
        else
            return {};
    }
    else
        return {};
}

Hs::Kind type_to_kind(const Hs::Type& kind)
{
    auto maybe_kind = type_to_kind_(kind);

    if (not maybe_kind)
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return *maybe_kind;
}

Hs::ConstructorDecl make_constructor(const vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::Type& typeish)
{
    // 1. Split into head and arguments
    auto [head,args] = Hs::decompose_type_apps(typeish);

    // 2. Get the constructor name.
    auto tc = head.to<Hs::TypeCon>();
    if (not tc)
        throw myexception()<<"In constructor `"<<typeish<<"`:\n    `"<<head<<"` is not a data constructor!";
    auto name = unloc(tc->name);

    // 3. If we have 1 arg and its a FieldDecls, then make a record constructor.
    if (args.size() == 1)
    {
        if (auto fd = args[0].to<Hs::FieldDecls>())
        {
            return {forall, c, name, *fd};
        }
    }

    // 4. Otherwise make a normal constructor.
    return {forall, c, name, args};
}

