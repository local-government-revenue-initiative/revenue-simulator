# R/utils.R
# Shared Utility Functions for Property Tax Revenue Simulator
# This file should be sourced first by app.R

# ==============================================================================
# NULL COALESCING OPERATOR
# ==============================================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ==============================================================================
# DEBUG LOGGING
# ==============================================================================

DEBUG_MODE <- Sys.getenv("DEBUG_MODE", "FALSE") == "TRUE"

debug_log <- function(..., sep = "") {
  if (DEBUG_MODE) {
    cat(..., sep = sep)
  }
  invisible(NULL)
}

debug_logn <- function(...) {
  if (DEBUG_MODE) {
    cat(..., "\n")
  }
  invisible(NULL)
}

set_debug_mode <- function(enabled = TRUE) {
  old_value <- DEBUG_MODE
  DEBUG_MODE <<- enabled
  if (enabled) {
    Sys.setenv(DEBUG_MODE = "TRUE")
  } else {
    Sys.setenv(DEBUG_MODE = "FALSE")
  }
  invisible(old_value)
}

# ==============================================================================
# ENVIRONMENT VARIABLE HELPERS
# ==============================================================================

get_env <- function(var_name, default = NULL) {
  value <- Sys.getenv(var_name, unset = NA)
  if (is.na(value)) {
    return(default)
  }
  return(value)
}

has_env <- function(var_name) {
  !is.na(Sys.getenv(var_name, unset = NA))
}
