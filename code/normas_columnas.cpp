#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List normas_columnas(NumericMatrix A, NumericVector normas) {
	int num_columnas = A.ncol();
	int num_filas = A.ncol();
	double suma;

	for(int i = 0u; i < num_columnas; ++i) {
		suma = 0.0;
		for(int j = 0u; j < num_filas; j++) {
		//normas(i) = std::norm(A(_,i));
		suma = suma + A[j,i]*A[j,i];
		}
		normas(i) = sqrt(suma);
	}
  return List::create(normas);
}