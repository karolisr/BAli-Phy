#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "math/exponential.H"
#include "math/eigenvalue.H"
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/io.H"
#include <valarray>
#include "dp/2way.H"
#include "util/range.H"
#include <unsupported/Eigen/MatrixFunctions>
#include "substitution/parsimony.H"

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_scaleMatrix(OperationArgs& Args)
{
    double factor = Args.evaluate(0).as_double();;

    auto arg2 = Args.evaluate(1);
    const Matrix& m = arg2.as_<Box<Matrix>>();

    int n1 = m.size1();
    int n2 = m.size2();

    auto m2 = new Box<Matrix>(n1,n2);
    for(int i=0;i<n1;i++)
	for(int j=0;j<n2;j++)
	    (*m2)(i,j) = factor * m(i,j);

    return m2;
}

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_elementwise_multiply(OperationArgs& Args)
{
    auto arg1 = Args.evaluate(0);
    const Matrix& m1 = arg1.as_<Box<Matrix>>();

    auto arg2 = Args.evaluate(1);
    const Matrix& m2 = arg2.as_<Box<Matrix>>();

    int n1 = m1.size1();
    int n2 = m1.size2();

    if (m2.size1() != n1 or m2.size2() != n2)
	throw myexception()<<"Trying to multiply matrices of unequal sizes ("<<n1<<","<<n2<<") and ("<<m2.size1()<<","<<m2.size2()<<") elementwise";

    auto m3 = new Box<Matrix>(n1,n2);
    for(int i=0;i<n1;i++)
	for(int j=0;j<n2;j++)
	    (*m3)(i,j) = m1(i,j) * m2(i,j);

    return m3;
}

extern "C" closure builtin_function_MatrixExp(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& Q = arg0.as_<Box<Matrix>>();
    int n = Q.size1();
    assert(Q.size2() == n);

    double t = Args.evaluate(1).as_double();

    // 1. Copy to Eigen matrix
    Eigen::MatrixXd QQ(n,n);
    for(int i=0;i<n;i++)
        for(int j=0;j<n;j++)
            QQ(i,j) = Q(i,j)*t;

    // 2. Take the matrix exponential
    Eigen::MatrixXd EE = QQ.exp();

    // 3. Copy back from Eigen matrix
    auto E = new Box<Matrix>(n,n);

    for(int i=0;i<n;i++)
        for(int j=0;j<n;j++)
            (*E)(i,j) = EE(i,j);

    // 4. Ensure that all entries are non-negative and rows sum to 1
    positivize_and_renormalize_matrix(*E);

    return E;
}



extern "C" closure builtin_function_lExp(OperationArgs& Args)
{
    auto L = Args.evaluate(0);
    auto pi = (vector<double>) Args.evaluate(1).as_<EVector>();
    double t = Args.evaluate(2).as_double();

    auto M = new Box<Matrix>;
    *M = exp(L.as_<EigenValues>(), pi, t);
    return M;
}



/*
 * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
 * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
 * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
 * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
 *           = pi^-1.2 * exp(S2) * pi^1/2
 */

extern "C" closure builtin_function_get_eigensystem(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const Matrix& Q = arg0.as_< Box<Matrix> >();

    auto pi = vector<double>(Args.evaluate(1).as_<EVector>() );

    const unsigned n = Q.size1();
    assert(Q.size2() == Q.size1());

#ifdef DEBUG_RATE_MATRIX
    assert(std::abs(sum(pi)-1.0) < 1.0e-6);
    for(int i=0;i<n;i++) {
	double sum = 0;
	for(int j=0;j<n;j++)
	    sum += Q(i,j);
	assert(abs(sum) < 1.0e-6);
    }
#endif

    //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
    vector<double> sqrt_pi(n, 1.0);
    vector<double> inverse_sqrt_pi(n, 1.0);
    for(int i=0;i<n;i++) {
        if (pi[i] > 1.0e-13)
        {
            sqrt_pi[i] = sqrt(pi[i]);
            inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];
        }
    }

    //--------------- Calculate eigensystem -----------------//
    Matrix S(n,n);
    for(int i=0;i<n;i++)
	for(int j=0;j<=i;j++) {
	    S(j,i) = S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
	    // check reversibility of rate matrix
	    if (i != j) {
		assert (S(i,j) >= 0);
		double p12 = Q(i,j)*pi[i];
		double p21 = Q(j,i)*pi[j];
		assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
		if (i > j)
		    assert( abs(S(i,j) - S(j,i)) < 1.0e-13 );
	    }
	    else
		assert (Q(i,j) <= 0);
#endif
	}

    //---------------- Compute eigensystem ------------------//
    return {new EigenValues(S)};
}



extern "C" closure builtin_function_transpose(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& M1 = arg0.as_<Box<Matrix>>();

    auto M2p = new Box<Matrix>(M1.size2(), M1.size1());
    auto& M2 = *M2p;
    for(int i=0;i<M2.size1();i++)
        for(int j=0;i<M2.size2();i++)
            M2(i,j) = M1(j,i);

    return M2p;
}
