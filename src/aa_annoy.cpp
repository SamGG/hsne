#include <RcppAnnoy.h>
#include <vector>
#include <algorithm>

typedef Annoy::AnnoyIndex<int, float, Annoy::Euclidean, Kiss64Random,
                          RcppAnnoyIndexThreadPolicy> MyAnnoyIndex;

//' Run the HSNE dimensionality reduction algorithm on a NumericalMatrix.
//' @export
// [[Rcpp::export]]
void my_annoy(
    Rcpp::NumericMatrix mat,
    int K
) {
  int c;

  const size_t nsamples=mat.nrow();
  auto ndims=mat.ncol();

  MyAnnoyIndex obj(ndims);
  // from <vector>
  std::vector<float> tmp(ndims);
  for (size_t i=0; i<nsamples; ++i) {
    Rcpp::NumericMatrix::Row cr=mat.row(i);
    // from <algorithm>
    std::copy(cr.begin(), cr.end(), tmp.begin());
    obj.add_item(i, tmp.data());
  }
  obj.build(50);

  std::vector<int> neighbor_index;
  std::vector<float> neighbor_dist;
  c = 1;
  obj.get_nns_by_item(
    c, K + 1, -1, &neighbor_index, &neighbor_dist);
  for (auto i : neighbor_index) {
    std::printf("%d ", i);
  }

  c = 2;
  obj.get_nns_by_item(
    c, K + 1, -1, &neighbor_index, &neighbor_dist);

}
