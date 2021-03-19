#include "haskell.H"
#include "util/string/join.H"

using std::string;
using std::vector;

namespace Haskell
{

string List::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "[" + join(parts,",") +"]";
}

string Tuple::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "(" + join(parts,",") +")";
}

string LetQual::print() const
{
    return "let " + binds.print();
}

string SimpleQual::print() const
{
    return exp.print();
}

string PatQual::print() const
{
    return bindpat.print() + " <- " + exp.print();
}

string ID::print() const
{
    return name;
}

string WildcardPattern::print() const
{
    return "_";
}

string LazyPattern::print() const
{
    // FIXME -- do we need parentheses around the pattern, and if so, when?
    return "~"+pattern.print();
}

string AsPattern::print() const
{
    // FIXME -- do we need parentheses around the pattern, and if so, when?
    return var.print()+"@"+pattern.print();
}

}
