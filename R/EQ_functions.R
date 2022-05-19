

#' @title toEQ5DIndex
#' @description Generate EQ-5D state vector from data.frame/matrix or named vector with dimensions.
#' @param x A data.frame, matrix, or vector containing dimension levels. Should have column names corresponding to the dim.names argument.
#' @param dim.names A vector of dimension names in data.frame/matrix/vector x
#' @return A vector of 5-digit EQ-5D health state indexes.
#' @examples 
#' toEQ5Dindex(c(1,2,3,4,5))
#' 
#' example_data <- as.data.frame(matrix(data = c(1, 2, 3, 4, 5, 
#'                                               5, 4, 3, 2, 1, 
#'                                               3, 2, 1, 2, 3), 
#'                                      ncol = 5, 
#'                                      byrow = TRUE, 
#'                                      dimnames = list(NULL, c("mo", "sc", "ua", "pd", "ad"))))
#' example_data$irrelevant <- c(6,5,3)
#' toEQ5Dindex(example_data)
#' @export
toEQ5Dindex <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  # Matrix or data.frame
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    colnames(x) <- tolower(colnames(x))
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    if(!all(dim.names %in% colnames(x))) stop("Provided dimension names not available in matrix/data.frame.")
    return(as.integer(as.matrix(x[, dim.names]) %*% (10^(4:0))))
  }
  if(length(x)<5) stop("Too short vector provided. Stopping")
  if(is.null(names(x))) {
    message("No names provided in vector.")
    if(length(x) == 5) {
      message("Vector of 5 provided, assuming dimensions in conventional order: MO, SC, UA, PD, AD.")
      names(x) <- dim.names
    }
  } else {
    names(x) <- tolower(names(x))
  }
  if(!all(dim.names %in% names(x))) stop("Provided dimension names not in vector names. Stopping.")
  as.integer(x[dim.names] %*% (10^(4:0)))
}

#' @title toEQ5Ddims
#' @description Generate dimension vectors based on state index
#' @param x A vector of 5-digit EQ-5D state indexes.
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A data.frame with 5 columns, one for each EQ-5D dimension, with names from dim.names argument.
#' @examples 
#' toEQ5Ddims(c(12345, 54321, 12321))
#' @export
toEQ5Ddims <- function(x, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  # coerce to integer
  x <- as.integer(x)
  # Remove items outside of bounds
  x[!regexpr("^[1-5]{5}$", x)==1] <- NA
  if(sum(is.na(x))) warning(paste0("Provided vector contained ", sum(is.na(x)), " items not conforming to 5-digit numbers with exclusively digits in the 1-5 range."))
  as.data.frame(outer(X = x, Y = structure(.Data = 10^(4:0), .Names = dim.names), FUN = "%/%") %% 10)
}

#' @title make_all_EQ_states
#' @description Make a data.frame with all health states defined by dimensions
#' @param version Either "3L" or "5L", to signify whether 243 or 3125 states should be generated
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param append_index Boolean to indicate whether a column of 5-digit EQ-5D health state indexes should be added to output.
#' @return A data.frame with 5 columns and 243 (-3L) or 3125 (-5L) health states
#' @examples 
#' make_all_EQ_states('3L')
#' @export
make_all_EQ_states <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), append_index = FALSE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!is.vector(version)) stop("version argument is not a vector of length 1")
  if(length(version)>1) {
      message("version argument provided length of more than 1, first element is used.")
      version <- version[[1]]
    }
  if(!toupper(version) %in% c("5L", "3L")) stop('version argument should be either "3L" or "5L"')
  xout <- do.call(expand.grid, structure(rep(list(1:ifelse(version == "5L", 5, 3)), 5),.Names = dim.names[5:1]))[,5:1]
  if(append_index) xout$state <- toEQ5Dindex(xout, dim.names)
  xout
}

#' @title make_all_EQ_indexes
#' @description Make a vector containing all 5-digit EQ-5D indexes for -3L or -5L version.
#' @param version Either "3L" or "5L", to signify whether 243 or 3125 states should be generated
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @return A vector with 5-digit state indexes for all 243 (-3L) or 3125 (-5L) EQ-5D health states
#' @examples 
#' make_all_EQ_indexes('3L')
#' @export
make_all_EQ_indexes <- function(version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  toEQ5Dindex(do.call(make_all_EQ_states, as.list(match.call()[-1])))
}

#' @title EQ_dummies
#' @description Make a data.frame of all EQ-5D dummies relevant for e.g. regression modeling. 
#' @param df data.frame containing EQ-5D health states.
#' @param version Either "3L" or "5L", to signify EQ-5D instrument version
#' @param dim.names A vector of dimension names to be used as names for output columns.
#' @param drop_level_1 If set to FALSE, dummies for level 1 will be included. Defaults to TRUE.
#' @param add_intercept If set to TRUE, a column containing 1s will be appended. Defaults to FALSE.
#' @param incremental If set to TRUE, incremental dummies will be produced (e.g. MO = 3 will give mo2 = 1, mo3 = 1). Defaults to FALSE.
#' @param append Optional string to be appended to column names.
#' @param prepend Optional string to be prepended to column names.
#' @param return_df If set to TRUE, data.frame is returned, otherwise matrix. Defaults to TRUE.
#' @return A data.frame of dummy variables 
#' @examples 
#' make_dummies(make_all_EQ_states('3L'), '3L')
#' 
#' make_dummies(df = make_all_EQ_states('3L'), 
#'              version =  '3L', 
#'              incremental = TRUE, 
#'              add_intercept = TRUE, 
#'              prepend = "d_")
#' @export
make_dummies <- function(df, version = "5L", dim.names = c("mo", "sc", "ua", "pd", "ad"), drop_level_1 = TRUE, add_intercept = FALSE, incremental = FALSE, prepend = NULL, append = NULL, return_df = TRUE) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(!length(dim(df) == 2)) stop("Need to provide matrix or data.frame of 2 dimensions.")

  colnames(df) <- tolower(colnames(df))
  if(is.null(colnames(df))) {
    message("No column names")
    if(NCOL(df) == 5) {
      message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
      colnames(df) <- dim.names
    } 
  }
  if(!all(dim.names %in% colnames(df))) stop("Provided dimension names not available in matrix/data.frame.")
  # intercept_col <- NULL
  # if(add_intercept) intercept_col <- data.frame(intercept = rep(1, NROW(df)))
  
  vers <- ifelse(version == '5L', 5, 3)
  
  startlevel <- ifelse(drop_level_1, 2, 1)
  if(incremental) {
    tmp <- 1*lower.tri(diag(vers), TRUE)[,startlevel:vers]  
  } else {
    tmp <- diag(vers)[,startlevel:vers]
  }
  
  tmpout <- do.call(cbind, lapply(df[,dim.names], function(x) tmp[x,      ]))
  colnames(tmpout) <- as.vector(t(outer(paste0(prepend, dim.names), paste0(startlevel:vers, append), FUN = paste0)))
  if(add_intercept) tmpout <- cbind(intercept = 1, tmpout)
  if(return_df) tmpout <- as.data.frame(tmpout)
  tmpout
}

#' @title .pstate3t5
#' @description Takes a N x 25 matrix with probabilities per level/dimension, and creates an N * 3125 matrix with probabilities per state
#' @param PPP N x 25 matrix with probabilities per level/dimension, typically created by .EQrxwprobs
#' @return An N * 3125 matrix with probabilities per state
.pstate3t5 <-function(PPP) {
  vallst <- as.vector(as.matrix(expand.grid(21:25, 16:20, 11:15, 6:10, 1:5))[, 5:1])
  t1l<- lapply(seq_len(dim(PPP)[2]), function(x) PPP[, x])
  do.call(cbind, lapply(seq_len(3125), function(i) {
      t1l[[vallst[[i]]]]*
      t1l[[vallst[[i+3125]]]]*
      t1l[[vallst[[i+6250]]]]*
      t1l[[vallst[[i+9375]]]]*
      t1l[[vallst[[i+12500]]]]
  }))
}



#' @title .EQxwrprob
#' @description Takes a matrix of parameters for reverse crosswalk model, returns 243 x 25 matrix of state/level transition probabilities.
#' @param par Matrix of model parameters 
#' @return An 243 * 25 matrix with probabilities for state level transitions.
.EQxwrprob <- function(par = .EQrxwmod7) {
  
  par <- as.matrix(par)
  
  # Logistic function by way of hyperbolic tangent
  lp <- function(eta) 0.5+0.5*tanh(eta*0.5)
  
  # Parameters for transitions between levels 1-->2, 2-->3, 3-->4, and 4-->5, per dimension
  Zx<-t(par[1:4,])            
  # Parameters for adjacent dimensions (10), age, age^2, and gender
  Bx<-par[5:14,]
  
  tmp <- diag(3)[,2:3]
  Y <- - do.call(cbind, lapply(as.list(expand.grid(1:3, 1:3, 1:3, 1:3, 1:3)[,5:1]), function(x) tmp[x,]))
  
  # Model matrix: dummies for EQ-5D-3L (i.e. mo2, mo3, sc2 ...ad2, ad3), age, age^2, gender
  # Y <- - as.matrix(cbind(inp[, 1:10], inp[,11]/10, (inp[,11]/10)^2, inp[,12])) # Note the "-" first; these are negatives
  do.call(cbind, lapply(1:5, function(i) {
    # inverse logit, first the level transitions
    # t(Zx[, rep(i, NROW(inp)])) creates a N * 4 matrix with the corresponding level transition parameters in the four columns
    tmp <- lp(Zx[rep.int(i,243),] + 
                # Y %*% Bx[,i] is a matrix multiplication of the model matrix and the corresponding coefficients
                (Y %*% Bx[,i])[,1])
    # subtract previous level from next, creating a matrix with column 1 representing p for level 1, column 2 for level 2, etc.
    cbind(tmp,1)-cbind(0, tmp)
  }))
}


#' @title eqxwr
#' @description Get reverse crosswalk values
#' @param x A vector of 5-digit EQ-5D state indexes or a matrix/data.frame with columns corresponding to EQ-5D state dimensions
#' @param country String vector indicating country name(s) or country code(s) according to ISO3166 Alpha 2 or Alpha 3
#' @param dim.names A vector of dimension names to identify dimension columns
#' @return A vector of reverse crosswalk values or data.frame with one column per reverse crosswalk set requested.
#' @examples 
#' eqxwr(c(11111, 12321, 32123, 33333), 'US')
#' eqxwr(make_all_EQ_states('3L'), c('DK', 'US'))
#' @export
eqxwr <- function(x, country = NULL, dim.names = c("mo", "sc", "ua", "pd", "ad")) {
  if(!length(dim.names) == 5) stop("Argument dim.names not of length 5.")
  if(length(dim(x)) == 2) {
    colnames(x) <- tolower(colnames(x))
    if(is.null(colnames(x))) {
      message("No column names")
      if(NCOL(x) == 5) {
        message("Given 5 columns, will assume dimensions in conventional order: MO, SC, UA, PD, AD.")
        colnames(x) <- dim.names
      } 
    }
    if(!all(dim.names %in% colnames(x))) stop("Provided dimension names not available in matrix/data.frame.")
    x <- toEQ5Dindex(x = x, dim.names = dim.names)
  }
  
  country <- .fixCountries(country)
  if(any(is.na(country))) {
    isnas <- which(is.na(country))
    for(i in isnas)  warning('Country ', names(country)[i], ' not found. Removed.')
    country <- country[!is.na(country)]
  }
  
  if(length(country)==0) {
    message('No valid countries listed. These countries are currently available.')
      print(.pkgenv$country_codes)
    stop('No valid countries listed.')
    }
  
  x <- as.integer(x)
  x[!regexpr("^[1-3]{5}$", x)==1] <- NA
  
  if(length(country)>1) {
    names(country) <- country
    return(do.call(cbind, lapply(country, function(count) eqxwr(x, count, dim.names))))
  }
  
  xout <- rep(NA, length(x))
  
  xout[!is.na(x)] <- .pkgenv$xwsets[match(x[!is.na(x)], .pkgenv$states_3L$state), country]
  xout
  
}


#' @title eqxwr_add
#' @description Add user-defined EQ-5D-5L value set to reverse crosswalk options.
#' @param df A data.frame or file name pointing to csv file. The contents of the data.frame or csv file should be exactly two olumns: state, containing a list of all 3125 EQ-5D-5L health state vectors, and a column of corresponding utility values, with a suitable name.
#' @param country Optional string. If not NULL, will override name from second column of df argument
#' @return True/False, indicating success or error.
#' @examples 
#' \donttest{
#' eqxwr_add('C:/SOMEFOLDER/thisfile.csv')
#' eqwxr_add(some.data.frame, 'FANTASIA')
#' }
#' @export
eqxwr_add <- function(df, country = NULL) {
  if(class(df) == 'character') {
    if(!file.exists(df)) stop('File named ', df, ' does not appear to exist. Exiting.')
    df <- read.csv(file = df, stringsAsFactors = F)
  } 
  if(!NCOL(df) == 2) stop('df should have exactly two columns.')
  class(df[,1]) <- 'integer'
  if(!NROW(df) == 3125) stop('df should have exactly 3125 rows.')
  if(!all(df[,1] %in% .pkgenv$states_5L$state)) stop('First column of df should contain all EQ-5D-5L health state indexes exactly once.')
  df <- df(match(.pkgenv$states_5L$state, df[,1]))
  if(!is.null(country)) {
    if(length(country)>1) {
      warning('Length of country argument > 1, first item used.')
      country <- country[1]
    }
    colnames(df)[2] <- country
  }
  thisName <- colnames(df)[2]
  if(thisname %in% colnames(.uservsets5L)) stop('New country name already in user-defined EQ-5D-5L value set list.')
  
  if(any(is.na(df[,2]*-1.1))) stop("Non-numeric values in second column of df.")
  # No problems
  
  .uservsets5L[, thisName] <- df[,2]
  assign(x = 'xwsets', value = .pkgenv$probs %*% cbind(as.matrix(.vsets5L[, -1, drop = F]), as.matrix(.uservsets5L[,-1, drop = F])), envir = .pkgenv)
  cntrs <- .cntrcodes[.cntrcodes$ISO3166Alpha2 %in% c(colnames(.vsets5L)[-1], colnames(.uservsets5L)[-1]),]
  rownames(cntrs) <- NULL
  assign(x = "country_codes", value = cntrs, envir = .pkgenv)
  
  yesno <- 'n'
  if(file.exists(file.path(.pkgenv$cache_path, "cache.Rdta"))) yesno <- 'y'
  else {
    message('If you wish for the user-defined reverse value set to be persistent, a file needs to be saved to the hard-drive.')
    message('The file would be saved to: ', .pkgenv$cache_path)
    message('Please type "y", "Y", "Yes", or "yes" to allow this file to be saved. All other inputs will be interpreted as "no".')
    yesno = readline(prompt = "Save? (Yes/No) : ")  
  }
  
  if(tolower(yesno) %in% c("yes", "y")) {
    message('Persistent data will be saved.')
    save(list = ls(envir = .pkgenv), envir = .pkgenv, file = file.path(.pkgenv$cache_path, 'cache.Rdta'))
  } else {
    message('Persistent data will not be saved.')
  }
  1
}


