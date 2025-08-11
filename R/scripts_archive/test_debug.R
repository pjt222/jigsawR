# Debug script to test tab extraction step by step

cat("=== Debug Test ===\n")
cat("Step 1: Testing basic environment\n")

# Test if we can create the jigsaw environment
.jigsaw_env <- new.env()
cat("Environment created successfully\n")

# Test basic functions one by one
cat("Step 2: Testing random functions\n")

init_jigsaw <- function(seed = NULL, tabsize = 20, jitter = 4,
                        width = 300, height = 200, radius = 2.0,
                        xn = 15, yn = 10) {

  if (is.null(seed)) {
    seed <- as.integer(runif(1) * 10000)
  }

  # Store all parameters in environment
  .jigsaw_env$seed_initial <- seed
  .jigsaw_env$seed <- seed
  .jigsaw_env$tabsize <- tabsize
  .jigsaw_env$jitter <- jitter
  .jigsaw_env$width <- width
  .jigsaw_env$height <- height
  .jigsaw_env$radius <- radius
  .jigsaw_env$xn <- xn
  .jigsaw_env$yn <- yn
  .jigsaw_env$offset <- 0.0

  # Parse input
  .jigsaw_env$t <- tabsize / 200.0
  .jigsaw_env$j <- jitter / 100.0
}

cat("init_jigsaw function defined\n")

# Test initialization
init_jigsaw(seed = 1234, xn = 2, yn = 2)
cat("Initialization completed\n")
cat("Seed:", .jigsaw_env$seed, "\n")
cat("Grid size:", .jigsaw_env$xn, "x", .jigsaw_env$yn, "\n")

cat("=== Test Complete ===\n")