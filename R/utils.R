# Wrapper for file.copy that throws if any files fail to copy. Use name
# file.copy2 to make clear that it's a wrapper for a base function, and not for
# fs::file_copy.
file.copy2 <- function(from, to, ...) {
  res <- file.copy(from, to, ...)
  if (!all(res)) {
    stop("Error copying files: ", paste0(from[!res], collapse = ", "))
  }
}

dir.create2 <- function(path, ...) {
  res <- dir.create(path, ...)
  if (!res) {
    stop("Error creating directory: ", path)
  }
}


.fixCountries <- function(countries) {
  cntrs <- .pkgenv$country_codes
  sapply(countries, function(country) {
    tmp <- which(toupper(as.matrix(cntrs)) == toupper(country), arr.ind = T)
    if(nrow(tmp))  return(cntrs$ISO3166Alpha2[tmp[1,1]])
    NA
  })
}

find_cache_dir <- function(pkg) {
  # In R 4.0 and above, CRAN wants us to use the new tools::R_user_dir().
  # If not present, fall back to rappdirs::user_cache_dir().
  R_user_dir <- getNamespace('tools')$R_user_dir
  if (!is.null(R_user_dir)) {
    R_user_dir(pkg, which = "cache")
  } else {
    rappdirs::user_cache_dir(pkg, "R")
  }
}