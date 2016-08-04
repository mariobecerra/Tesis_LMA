#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

List gradiente(NumericVector i, NumericVector j, NumericVector x, 
                  NumericMatrix U_in, NumericMatrix P_in,
                  NumericVector a_in, NumericVector b_in, 
                  double gamma_in, double lambda) {
    NumericVector u_row;
    NumericVector p_row;
    double gamma = 2 * gamma_in;
    NumericMatrix P = clone(P_in);
    NumericMatrix U = clone(U_in);
    NumericVector a = clone(a_in);
    NumericVector b = clone(b_in);
    double e;

    for(int t = 0; t < i.size(); t++){
         e = x(t) - a(i(t)-1) - b(j(t)-1)- sum(U(i(t)-1,_)  * P(j(t)-1,_) );
         u_row = U(i(t)-1,_) + gamma*(e * P(j(t)-1,_) - lambda* U(i(t)-1,_));
         p_row = P(j(t)-1,_) + gamma*(e * U(i(t)-1,_) - lambda*P(j(t)-1,_));
         U(i(t)-1,_) = u_row;
         P(j(t)-1,_) = p_row;
         a(i(t)-1) = a(i(t)-1) + gamma * (e - lambda*a (i(t)-1));
         b(j(t)-1) = b(j(t)-1) + gamma * (e - lambda*b (j(t)-1));
    }
    return List::create(U,P,a,b);
}
