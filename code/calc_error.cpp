#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]


double calc_error(NumericVector i, NumericVector j, NumericVector x, 
                  NumericMatrix U, NumericMatrix P, NumericVector a, NumericVector b){
    double suma = 0;
    for(int t = 0; t < i.size(); t++){
        double e = x(t) - a(i[t]-1) - b(j[t]-1)- sum(U(i(t)-1,_)  * P(j(t)-1,_) );
        suma = suma + e*e;
    }              
    double tam = i.size() + 0.0;
    return suma/tam      ;                             
}
