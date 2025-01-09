library(Matrix)
# library(ramify)
# library(Dict)

#' A Reference Class to represent a HSNE.
#'
#' @field num_scales A numeric value of the number of scales.
#' @field scales A data frame with DataScale and SubScale.
#' @field index A numeric value of the current scale number.
#' @importFrom Dict dict
#' @importFrom ramify argmax
HSNE <- setRefClass(
  "HSNE",
  fields = list(
    num_scales = "numeric",
    # Data scale is indexed as 1 and the rest increments by 1 for each sub-scales.
    scales = "data.frame",
    index = "numeric"
  ),
  methods = list(
    initialize = function(num_scales) {
      "Initialize a HSNE with num_scales scales."
      num_scales <<- num_scales
      # set up the shape (column numbers) of the data frame
      scales <<- data.frame(matrix(ncol = num_scales, nrow = 1))
      index <<- 0
      # rename column names for accessing each columns
      colnames(scales) <<- c(1:num_scales)
    },
    toString = function() {
      "To string for HSNE."
      message(paste("HSNE hierarchy with", num_scales, "scales."))
      return(paste("HSNE hierarchy with", num_scales, "scales."))
    },
    getItem = function(index) {
      "Get item from scale."
      if(index <= 0) {
        stop("Can't get this scale. Data scale starts at index 1.")
      } else if(index > num_scales) {
        stop("Can't get this scale. Index is too high.")
      } else {
        scales[index]
      }
    },
    setItem = function(index, value) {
      "Set item on scale."
      scales[index] <<- value
    },
    iter = function() {
      "Iterate scale."
      .self
    },
    next_scale = function() {
      "Get next scale."
      if(index == num_scales - 1) {
        index <<- 0
        stop("There are no more scale.")
      } else {
        index <<- index + 1
        scales[index]
      }
    },
    get_topscale = function() {
      "Get the top scale (data scale)."
      scales[1]
    },
    #TODO: I think it's done. But I need to test it
    get_datascale_mappings = function(scalenumber) {
      "Get datascale of choice at scalenumber. Used automatically by cluster_scale(scalenumber) method."
      if(scalenumber <= 1) {
        stop("Can't generate mapping for complete datset, only sub-scales get clustered.")
      }
      if(scalenumber > num_scales) {
        stop(paste("Scale doesn't exist, object has", num_scales, "scales."))
      }
      # initialize an empty dictionary
      maps <- NULL
      # loop through all the subscales
      for (scale in 2:scalenumber) {
        if(is.null(maps)) { #if(length(maps$keys())==0) {
          maps <- Dict::dict(init_keys = NULL, init_values = NULL)
          #message(length(scales[1]$'1'$size))
          #message(1:length(.self$get_topscale()))
          #maps$set(key = 1:1000, value = NA)
          j <- 1
          #message(length(scales[[scale]]$best_representatives)) = 1000
          for (v in scales[[scale]]$best_representatives) {
            maps$set(key = j, value = v)
            j <- j + 1
          }
        } else {
          #message(length(maps$keys()))
          for (k in 1:maps$length()) {
            maps$set(key = k, value = scales[[scale]]$best_representatives[maps$get(k)])
            #maps[[key]] <- scales[[scale]]$best_representatives[maps[[key]]]
          }
        }
      }
      #message(paste(maps$keys(), ","))
      #message("-----------------------------------------------------------__")
      #message(paste(maps$values(), ","))
      maps
    },
    get_map_by_cluster = function(scalenumber, clustering) {
      "Get map for prop_method=clustering for a subscale at scalenumber. Used automatically by cluster_scale(scalenumber) method."
      if(length(clustering) != NCOL(scales[[scalenumber]]$area_of_influence)) {
        stop("Number of labels does not match number of landmarks in scale.")
      }
      if(scalenumber <= 1) {
        stop("Can't generate mapping for complete dataset, only sub-scales get clustered.")
      }
      labels <- sort(unique(clustering), decreasing = FALSE)
      # loop through subscales in reverse order
      for (scale in scalenumber:2) {
        # setting up a matrix with given shapes of current scale dimensions
        new_aoi <- matrix(nrow = NROW(scales[[scale]]$area_of_influence), ncol = length(unique(clustering)))
        # i used as an enumerator
        i <- 1
        for (label in labels) {
          vec <- vector()
          j <- 1
          for (x in clustering) {
            if(label==x) {
              vec[j] <- 1
            } else {
              vec[j] <- 0
            }
            j <- j + 1
          }
          # matrix multiplication then converted to a list
          new_aoi[, i] <- as.list(scales[[scale]]$area_of_influence %*% vec)
          # reformat shape of matrix to normal from list.
          new_aoi <- matrix(new_aoi, ncol = length(labels))
          i <- i + 1
        }
        # round all values in matrix to the closest whole number
        lapply(new_aoi, trunc)
        # get the highest weight from row.
        # minus one for indexing. So that the label with 0 is the first label
        clustering <- ramify::argmax(new_aoi, row = TRUE) - 1
      }
      clustering
    },
    cluster_scale = function(scalenumber, prop_method = "cluster") {
      "Cluster the given scale using Leiden community detection."
      message("Applying Leiden algorithm.")
      if(scalenumber == 1) {
        message("Warning: You are about to cluster the full dataset, this might take a very long time")
        return(run_leiden(scalenumber))
      } else if(scalenumber > num_scales) {
        stop(paste("Scale doesn't exist, object has", num_scales, "scales"))
      }
      if(prop_method=="cluster") {
        membership = run_leiden(scalenumber)
        return(get_map_by_cluster(scalenumber, membership))
      } else if(prop_method=="label") {
        membership = run_leiden(scalenumber)
        mapping = get_datascale_mappings(scalenumber)
        result <- vector()
        for (x in mapping$values()) {
          result <- c(result, membership[x])
        }
        return(result)
      }
    },
    run_leiden = function(scalenumber) {
      "Run leiden algorithm on a scale"
      # get a column in the scales matrix
      sc <- scales[[scalenumber]]

      #get the column in the tmatrix of sc. Plus one as indexing in R starts with one.
      sources <- sc$tmatrix[,1] + 1
      targets <- sc$tmatrix[,2] + 1

      # zip sources and targets
      edgelist = cbind(sources, targets)
      G = igraph::graph_from_edgelist(edgelist)
      GWeight <- sc$tmatrix[,3]
      # leiden algorithm applied to G withe GWeight
      # partition <- leiden::leiden(G, partition_type = "ModularityVertexPartition", weights = GWeight)
      # TODO: add resolution = 1, n_iterations = 2
      # TODO: check Gweight is larger when connection is stronger.
      Gcommunities <- igraph::cluster_leiden(G, objective_function = "CPM", weights = GWeight)
      partition <- igraph::membership(Gcommunities)
      # indexing back to python standards
      partition - 1
    }
  )
)

#' A Reference Class to represent a DataScale.
#'
#' @field tmatix A matrix of shape num_scalesXnum_data_points.
#' @field size A numeric vale of points in scale.
#' @field datapoints A vector of all the datapoints represented by numeric values.
#' @field scalenum A numeric value of the current scale number in HSNE object.
DataScale <- setRefClass(
  "DataScale",
  fields = list(
    tmatrix = "matrix",
    size = "numeric",
    datapoints = "vector",
    scalenum = "numeric",
    num_scales = "numeric"
  ),
  methods = list(
    initialize = function(num_scales, tmatrix) {
      "Initialize data-scale."
      tmatrix <<- tmatrix
      size <<- NROW(tmatrix)
      datapoints <<- vector(mode = "numeric", length = size)
      scalenum <<- 0
      num_scales <<- num_scales
    },
    toString = function() {
      "This method returns the string representation of a data-scale."
      # Use case in HSNE class: scales[[num_scale]]$toString()
      message(paste("HSNE datascale", scalenum, "with", size, "datapoints."))
    }
  )
)

#' A Reference Class to represent a SubScale
#'
#' @field tmatix A matrix of shape num_scalesXnum_data_points.
#' @field size A numeric vale of points in scale.
#' @field datapoints A vector of all the datapoints represented by numeric values.
#' @field scalenum A numeric value of the current scale number in HSNE object.
#' @field num_scales A numeric value of the total number scales in HSNE object.
#' @field lm_to_original A vector of labels from each datapoints to its closest original landmarks.
#' @field lm_to_previous A vector of labels from each datapoints to its closest previous landmarks.
#' @field lm_weights A vector of all landmark's weights.
#' @field previous_to_current A vector of previous to current label.
#' @field area_of_infleunce A sparse matrix of area of influence.
#' @field best_representatives A vector of each datapoint's best representative.
#' @importFrom Matrix sparseMatrix
SubScale <- setRefClass(
  "SubScale",
  fields = list(
    tmatrix = "matrix",
    size = "numeric",
    datapoints = "vector",
    scalenum = "numeric",
    num_scales = "numeric",
    lm_to_original = "vector",
    lm_to_previous = "vector",
    lm_weights = "vector",
    previous_to_current = "vector",
    area_of_influence = "sparseMatrix",
    best_representatives = "vector"
  ),
  methods = list(
    initialize = function(scalenum, num_scales, tmatrix, lm_to_original, lm_to_previous, lm_weights, previous_to_current, area_of_influence) {
      "Initialize sub-scale."
      tmatrix <<- tmatrix
      size <<- NROW(tmatrix)
      datapoints <<- vector(mode = "numeric", length = size)
      scalenum <<- scalenum
      num_scales <<- num_scales
      lm_to_original <<- lm_to_original
      lm_to_previous <<- lm_to_previous
      lm_weights <<- lm_weights
      previous_to_current <<- previous_to_current
      area_of_influence <<- Matrix::sparseMatrix(
        i = area_of_influence[,1],
        j = area_of_influence[,2],
        x = area_of_influence[,3],
        # R indexing cannot start from 0
        index1 = FALSE
      )
      # minus 1 for indexing correctly
      best_representatives <<- ramify::argmax(.self$area_of_influence, row=TRUE) - 1
    },
    toString = function() {
      "This method returns the string representation of a sub-scale."
      message(paste("HSNE subscale", scalenum, "with", size, "datapoints."))
    }
  )
)

#' A Reference Class to represent a HSNE_parser.
#'
#' Parses a .hsne file using the method read_HSNE_binary(filename).
HSNE_parser <- setRefClass(
  "HSNE_parser",
  fields = list(),
  #contains = "HSNE",
  methods = list(
    read_unit_vector = function(handle) {
      "Read unsigned int vector from HDI binary file."
      vectorlength <- readBin(handle, integer(), n = 1)
      vec <- vector()
      vec <- read_bin_vector(handle, vec, vectorlength)

      #for (rep in 1:vectorlength) {
      #  vec <- append(vec, c(readBin(handle, integer(), n = 1, size = 4)))
      #}

      vec
    },
    read_scalar_vector = function(handle) {
      "Read float vector from HDI binary file."
      vectorlength <- readBin(handle, integer(), n = 1)
      vec <- vector()
      vec <- read_bin_vector(handle, vec, vectorlength)

      #for (rep in 1:vectorlength) {
      #  vec <- append(vec, c(readBin(handle, double(), n = 1, size = 4)))
      #}

      vec
    },
    read_bin_vector = function(handle, vec, vectorlength) {
      lapply(c(1:vectorlength), function(x) {
        vec <- append(vec, c(readBin(handle, integer(), n = 1, size = 4)))
      })
    },
    read_HSNE_binary = function(filename, verbose=TRUE) {
      "Read HSNE binary from file and construct HSNE object with top- and subscales."
      logger <- Logger(verbose)
      longtic <- Sys.time()

      # open filename in read only binary
      handle <- file(filename, "rb")

      # read unused line of hsne file, to skip line
      readBin(handle, integer(), n = 2)

      numscales <- readBin(handle, double(), n = 1, size = 4)
      scalesize <- readBin(handle, double(), n = 1, size = 4)

      logger$log(paste("Number of scales", numscales))
      # store HSNE object in global environment as ha
      # ha <<- HSNE(numscales)
      assign("ha", HSNE(numscales), envir = .GlobalEnv)

      logger$log(paste("Start reading first scale of size", scalesize))
      tmatrix <- read_sparse_matrix(handle)
      logger$log("Done reading first scale..")
      ha$setItem(1, DataScale(num_scales = numscales, tmatrix = tmatrix))

      #lapply(c(1:(numscales-1)), function(x) {
      #  ha$setItem((x+1), build_subscale(handle, x, numscales, logger))
      #})

      for (i in 1:(numscales-1)) {
        ha$setItem((i+1), build_subscale(handle, i, numscales, logger))
      }

      message(paste("Total time spent parsing hierarchy and building objects:", (Sys.time()-longtic), "seconds"))
      # close connection to binary scale
      close(handle)
      ha
    },
    read_sparse_matrix = function(handle) {
      cols <- vector()
      rows <- vector()
      weights <- vector()
      numrows <- readBin(handle, integer(), n = 1)

      #for (rownum in 1:numrows) {  #lapply(c(1:numrows), function(x) {
      #  rowlen <- readBin(handle, integer(), n = 1)
      #  r <- vector()
      #  #r <- read_bin_matrix(handle, r, rowlen)
      #  for (rep in 1:rowlen) {
      #    r <- c(r, readBin(handle, integer(), n = 1, size = 4))
      #    r <- c(r, readBin(handle, double(), n = 1, size = 4))
      #  }
      #  cols <- c(cols, r[seq(1, 2)])
      #  weights <- c(weights, r[seq(2, 2)])
      #  rows <- c(rows, rep(rownum-1, rowlen))
      #} #)


      for (rownum in 1:numrows) {
        rowlen <- readBin(handle, integer(), n = 1)
        r <- vector()
        #----------------------------------------------------------------------------------
        for (rep in 1:rowlen) {
          r <- c(r, readBin(handle, integer(), n = 1, size = 4))
          r <- c(r, readBin(handle, double(), n = 1, size = 4))
        }
        cols <- c(cols, r[seq(1, length(r), 2)])
        weights <- c(weights, r[seq(2, length(r), 2)])
        rows <- c(rows, rep(rownum-1, rowlen))
        #----------------------------------------------------------------------------------
        #for (rep in 1:rowlen) {
        #  r <- append(r, c(readBin(handle, integer(), n = 1, size = 4)))
        #  r <- append(r, c(readBin(handle, double(), n = 1, size = 4)))
        #}
        #cols <- append(cols, r[seq(1, length(r), 2)])
        #weights <- append(weights, r[seq(2, length(r), 2)])
        #rows <- append(rows, rep(rownum-1, rowlen))
        #----
      }

      matrix(data = c(rows, cols, weights), ncol = 3)
    },
    read_bin_matrix = function(handle, r, rowlen) {
      lapply(c(1:rowlen), function(y) {
        r <- c(r, readBin(handle, integer(), n = 1, size = 4))
        r <- c(r, readBin(handle, double(), n = 1, size = 4))
      })
      r
    },
    build_subscale = function(handle, i, numscales, logger) {
      "Initialize a subscale with extracted sparse matrices and vectors."
      logger$log(paste("Next scale:", i))
      scalesize <- readBin(handle, double(), n = 1, size = 4)
      logger$log(paste("Scale size:", scalesize))
      logger$log(paste("Reading transmatrix..."))
      tmatrix <- read_sparse_matrix(handle)
      logger$log(paste("Reading landmarks of scale to original data..."))
      lm_to_original <- read_unit_vector(handle)
      logger$log(paste("Reading landmarks to previous scale..."))
      lm_to_previous <- read_unit_vector(handle)
      logger$log(paste("Reading landmark weights..."))
      lm_weights <- read_scalar_vector(handle)
      logger$log(paste("Reading previous scale to current scale..."))
      previous_to_current <- read_unit_vector(handle)
      logger$log(paste("Reading area of influence..."))
      area_of_influence <- read_sparse_matrix(handle)
      # initialize subscale
      subscale <- SubScale(
        scalenum <- i,
        num_scales <- numscales,
        tmatrix <- tmatrix,
        lm_to_original <- lm_to_original,
        lm_to_previous <- lm_to_previous,
        lm_weights <- lm_weights,
        previous_to_current <- previous_to_current,
        area_of_influence <- area_of_influence
      )
      subscale
    }
  )
)

#' A Reference Class to represent a Logger
#'
#' Printing messages to console
#'
#' @field enabled A boolean to enable printing.
Logger <- setRefClass(
  "Logger",
  fields = list(
    enabled = "logical"
  ),
  methods = list(
    initialize = function(enabled) {
      "Initialize a Logger."
      enabled <<- enabled
    },
    enable = function() {
      "Enable Logger."
      enabled <<- TRUE
    },
    disable = function() {
      "Disable Logger."
      enabled <<- FALSE
    },
    log = function(message) {
      "Show message."
      if(enabled==TRUE) {
        message(message)
      }
    }
  )
)
