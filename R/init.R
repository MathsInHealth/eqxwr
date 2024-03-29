##  Reverse crosswalk for R. Using EQ-5D-5L value sets for EQ-5D-3L data.
## 
##  This file is part of eqxwr.
##
##  eqxwr is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##
##  eqxwr is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with eqxwr If not, see <http://www.gnu.org/licenses/>.



.onLoad <- function(libname, pkgname) {
  if('eq.env' %in% names(.Options)) {
    pkgenv <- getOption('eq.env')
  } else {
    options("eq.env" = (pkgenv <- new.env(parent=emptyenv())))
  }
  assign(x = "cache_path", value = find_cache_dir('eq5d-suite'), envir = pkgenv)
  
  fexist <- F
  if(dir.exists(pkgenv$cache_path)) {
    if(file.exists(file.path(pkgenv$cache_path, "cache.Rdta"))) {
      fexist <- T
      load(file.path(pkgenv$cache_path, "cache.Rdta"), envir = pkgenv)
      # load(file.path(pkgenv$cache_path, "cache_global.Rdta"), envir = globalenv())
    }
  }
  .fixPkgEnv()
}

.fixPkgEnv <- function() {
  pkgenv <- getOption('eq.env')
  
  # if(!'EQrxwmod7' %in% names(pkgenv)) assign(x = "EQrxwmod7", .EQrxwmod7, envir = pkgenv)
  # if(!'vsets5L' %in% names(pkgenv)) assign(x = "vsets5L", .vsets5L, envir = pkgenv)
  # if(!'cntrcodes' %in% names(pkgenv)) assign(x = "cntrcodes", .cntrcodes, envir = pkgenv)
  
  
  if(!'states_3L' %in% names(pkgenv)) assign(x = "states_3L", value = make_all_EQ_states(version = '3L', append_index = T), envir = pkgenv)
  if(!'states_5L' %in% names(pkgenv)) assign(x = "states_5L", value =  make_all_EQ_states(version = '5L', append_index = T), envir = pkgenv)
  if(!'uservsets5L' %in% names(pkgenv)) assign(x = "uservsets5L", value =  pkgenv$states_5L[,"state", drop = F], envir = pkgenv)
  
  if(!'PPP' %in% names(pkgenv)) assign(x = "PPP", value = .EQxwrprob(par = .EQrxwmod7), envir = pkgenv)
  if(!'probs' %in% names(pkgenv)) assign(x = "probs", value = .pstate3t5(pkgenv$PPP), envir = pkgenv)
  
  if('probs' %in% names(pkgenv)) assign(x = 'xwrsets', value = pkgenv$probs %*% cbind(as.matrix(.vsets5L[, -1, drop = F]), if("uservsets5L" %in% names(pkgenv)) as.matrix(pkgenv$uservsets5L[,-1, drop = F]) else NULL), envir = pkgenv)
  
  
  
  EQvariants <- c('5L' = '5L')
  assign(x = 'country_codes', envir = pkgenv, value = lapply(EQvariants, function(EQvariant) {
    # message(EQvariant)
    tmp <- .cntrcodes[.cntrcodes$ISO3166Alpha2 %in% colnames(get(paste0('.vsets', EQvariant)))[-1],]
    rownames(tmp) <- NULL
    tmp
  }))
  if(file.exists(file.path(pkgenv$cache_path, 'cache.Rdta'))) save(list = ls(envir = pkgenv), envir = pkgenv, file = file.path(pkgenv$cache_path, 'cache.Rdta'))
  
}

