#include "symbols.H"
#include "util/variant.H"

using std::string;
using std::optional;

symbol_info::symbol_info(const string& s, symbol_type_t st, const optional<string>& p, int a)
    :name(s), symbol_type(st), parent(p), arity(a)
{ }

symbol_info::symbol_info(const string& s, symbol_type_t st, const optional<string>& p, int a, fixity_info f)
    :name(s), symbol_type(st), parent(p), arity(a), fixity(f)
{
}

bool operator==(const symbol_info&S1, const symbol_info& S2)
{
    return (S1.name == S2.name) and (S1.symbol_type == S2.symbol_type) and 
        (S1.arity == S2.arity);
}

bool operator==(const type_info& T1, const type_info& T2)
{
    return (T1.name == T2.name) and (T1.category() == T2.category());
}

int type_info::category() const
{
    return info.index();
}

bool type_info::is_type_other() const
{
    return (bool)to<std::monostate>(info);
}

const type_info::class_info* type_info::is_class() const
{
    return to<class_info>(info);
}

const type_info::data_info* type_info::is_data() const
{
    return to<data_info>(info);
}

bool type_info::is_type_syn() const
{
    return (bool)to<type_syn_info>(info);
}

bool type_info::is_type_fam() const
{
    return (bool)to<type_fam_info>(info);
}

