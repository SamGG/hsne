data_matrix = read.csv("../rSCHNEL/dev/mnist_test_data.csv")
dim(data_matrix)

# data_matrix = as.matrix(iris[,-5])

if (!inherits(data_matrix, "matrix"))
  data_matrix = as.matrix(data_matrix)


n_scales = 0
n_neighbors = 50
seeds = -1
landmark_treshold = 1.5
num_trees = 6
num_checks = 1024
trans_matrix_prune_treshold = 1.5
num_walks = 200
num_walks_per_landmark = 200
monte_carlo_sampling = TRUE
out_of_core_computation = TRUE
prop_method = "cluster"

check_num <- round(log10(nrow(data_matrix) / 100))
if (n_scales < 2 && check_num > 1) {
  n_scales <- check_num
} else if (n_scales < 2 && check_num < 2) {
  warning("The specified number of scales is below 2. The default value (2) will be used")
  n_scales <- 2
}




matrix_to_hsne <- hsne::run(
  data_matrix, n_scales, seeds, landmark_treshold,
  n_neighbors, num_trees, num_checks, trans_matrix_prune_treshold,
  num_walks, num_walks_per_landmark, monte_carlo_sampling, out_of_core_computation)

# Read in the hsne hierarchy
hsne_parser <- rSCHNEL:::HSNE_parser()
hsne_parser$build_subscale()
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

hsne::my_annoy(data_matrix, 5)
as.matrix(dist(data_matrix[c(2, c(408, 591, 531, 452, 291)+1),]))[1,]


hist(as.matrix(dist(data_matrix[sample.int(nrow(data_matrix), 1000)])), breaks = 99)

order(sqrt(rowSums(sweep(data_matrix[,], 2, data_matrix[2,], "-")**2)))[1:5]-1
sqrt(rowSums(sweep(data_matrix[,], 2, data_matrix[2,], "-")**2))[592]



