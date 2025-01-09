#' SCHNEL clustering
#'
#' Function that runs the SCHNEL algorithm on the specified file(s) and returns
#' a matrix/matrices which rows correspond to datapoints provided for
#' clustering; columns correspond to scales on which the clustering was
#' performed. The value of a cell represents the cluster to which a given point
#' belongs to.
#'
#' @param data_matrix Data
#' @param n_scales Number of scales in the HSNE hierarchy.
#' @param n_neighbors Number of neighbors for kNN graph clustering performed
#'   during HSNE hierarchy creation.
#' @param seeds Seed parameter for the HSNE dimensionality reduction.
#' @param landmark_treshold Minimum value of the connectivity score at which
#'   points at a particular scales will be selected as future landmarks.
#' @param num_trees Number of trees parameter for the HSNE dimensionality
#'   reduction
#' @param num_checks Number of checks parameter for the HSNE dimensionality
#'   reduction
#' @param trans_matrix_prune_threshold The beta threshold
#' @param num_walks Number of random walks performed on transition matrices
#'   during the creation of the HSNE heirarchy creation.
#' @param num_walks_per_landmark Number of random walks performed from every
#'   single landmark during the HSNE hierarchy creation.
#' @param monte_carlo_sampling Boolean specifying whether to perform Monte Carlo
#'   Method during the HSNE hierarchy creation.
#' @param out_of_core_computation Boolean for the out_of_core parameter for the
#'   HSNE dimensionality reduction
#' @param prop_method Default: cluster; other option: label
#' @examples
#' # None yet
#' @export
cluster <- function(
    data_matrix,
    n_scales = 0,
    n_neighbors = 50,
    seeds = -1,
    landmark_treshold = 1.5,
    num_trees = 6,
    num_checks = 1024,
    trans_matrix_prune_treshold = 1.5,
    num_walks = 200,
    num_walks_per_landmark = 200,
    monte_carlo_sampling = TRUE,
    out_of_core_computation = TRUE,
    prop_method = "cluster") {

  # Determine the number of scales of the HSNE hierarchy
  check_num <- round(log10(nrow(data_matrix) / 100))
  if (n_scales < 2 && check_num > 1)
    n_scales <- check_num
  else if (n_scales < 2 && check_num < 2) {
    warning("The specified number of scales is below 2. The default value (2) will be used")
    n_scales <- 2
  }

  # Create the HSNE hierarchy
  matrix_to_hsne <- run(
    data_matrix, n_scales, seeds, landmark_treshold,
    n_neighbors, num_trees, num_checks, trans_matrix_prune_treshold,
    num_walks, num_walks_per_landmark, monte_carlo_sampling, out_of_core_computation)

  # Read in the hsne hierarchy
  hsne_parser <- HSNE_parser()
  hier <- hsne_parser$read_HSNE_binary("test.hsne")

  # Initialize a matrix to store the result of the algorithm
  final_matrix <- matrix(nrow = nrow(data_matrix))
  # Get rid of the empty column
  final_matrix <- final_matrix[,-1]

  # Create a list with subscale numbers
  n_scales <- c(1:hier$num_scales)

  # Cluster all the subscales
  lapply(n_scales, function(x) {
    final_matrix <<- cbind2(final_matrix,  as.matrix(hier$cluster_scale(x, prop_method)))
  })

  # Create the output form
  return(final_matrix)
}
