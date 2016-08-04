#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

double cosine_similarity(NumericVector A, NumericVector B)
{
  int Vector_Length = A.size();
  double dot = 0.0, denom_a = 0.0, denom_b = 0.0 ;
  for(int i = 0u; i < Vector_Length; ++i) {
    dot += A[i] * B[i] ;
    denom_a += A[i] * A[i] ;
    denom_b += B[i] * B[i] ;
  }
  return dot / (sqrt(denom_a) * sqrt(denom_b)) ;
}