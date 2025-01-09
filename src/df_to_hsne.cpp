#include <Rcpp.h>
#include "r_hdi/hierarchical_sne_inl.h"
#include "r_hdi/panel_data_inl.h"
#include "r_hdi/cout_log.h"
#include "r_hdi/log_helper_functions.h"
#include "r_hdi/timers.h"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>

using namespace Rcpp;

//' Run the HSNE dimensionality reduction algorithm on a NumericalMatrix.
//' @params nm
//' @params num_scales
//' @params seed
//' @params landmark_threshold
//' @params num_neighbors
//' @params num_trees
//' @params num_checks
//' @params transition_matrix_prune_thresh
//' @params num_walks
//' @params num_walks_per_landmark
//' @params monte_carlo_sampling
//' @params out_of_core_computation
//' @export
// [[Rcpp::export]]
void run(
    NumericMatrix nm,
    int num_scales,
    int seed,
    float landmark_threshold,
    int num_neighbors,
    int num_trees,
    int num_checks,
    float transition_matrix_prune_thresh,
    int num_walks,
    int num_walks_per_landmark,
    bool monte_carlo_sampling,
    bool out_of_core_computation
  ) {
  hdi::dr::HierarchicalSNE<float, std::vector<hdi::data::MapMemEff<uint32_t,float>>> hsne;
  hdi::dr::HierarchicalSNE<float, std::vector<hdi::data::MapMemEff<uint32_t,float>>>::Parameters params;
  hdi::data::PanelData<float> panel_data;

  hdi::utils::CoutLog log;
  hdi::utils::AbstractLog* logger;

  params._seed = seed;
  params._mcmcs_landmark_thresh = landmark_threshold;
  params._num_neighbors = num_neighbors;
  params._aknn_num_trees = num_trees;
  params._aknn_num_checks = num_checks;
  params._transition_matrix_prune_thresh = transition_matrix_prune_thresh;
  params._mcmcs_num_walks = num_walks;
  params._num_walks_per_landmark = num_walks_per_landmark;
  params._monte_carlo_sampling = monte_carlo_sampling;
  params._out_of_core_computation = out_of_core_computation;

  int num_data_points = nm.rows();
  int num_dimensions = nm.cols();

  Rprintf("hsne.initialize\n");

  for(int j = 0; j < num_dimensions; ++j){
    panel_data.addDimension(std::make_shared<hdi::data::EmptyData>(hdi::data::EmptyData()));
  }
  panel_data.initialize();
  panel_data.reserve(num_data_points);

  Rprintf("hsne.setLogger\n");

  hsne.setLogger(&log);
  hsne.setDimensionality(num_dimensions);

  Rprintf("hsne.addDataPoint\n");
  Rprintf("mumDataPoint = %d\n", num_data_points);

  for (int j = 0; j < num_data_points; j++) {
    NumericVector a = nm.row(j);
    std::vector<float> data = as<std::vector<float>> (a);
    panel_data.addDataPoint(std::make_shared<hdi::data::EmptyData>(hdi::data::EmptyData()),data);
  }

  Rprintf("hsne.initialize\n");

  hsne.initialize(panel_data.getData().data(),num_data_points,params);
  hsne.statistics().log(logger);

  Rprintf("hsne.addScale\n");
  hsne.addScale();

  // for(int i = 0; i < num_scales-1; ++i){
  //   hsne.addScale();
  // }

  // std::ofstream filebin("test.hsne", std::ios::binary);
  // hdi::dr::IO::saveHSNE(hsne, filebin, logger);
}
