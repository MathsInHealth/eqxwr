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

.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  assign(x = "cache_path", value = find_cache_dir(pkgname), envir = .pkgenv)
  
  fexist <- F
  if(dir.exists(.pkgenv$cache_path)) {
    if(file.exists(file.path(.pkgenv$cache_path, "cache.Rdta"))) {
      fexist <- T
      load(file.path(.pkgenv$cache_path, "cache.Rdta"), envir = .pkgenv)
    }
    
  }
  if(!fexist) {
    assign(x = "PPP", value = .EQxwrprob(), envir = .pkgenv)
    assign(x = "probs", value = .pstate3t5(.pkgenv$PPP), envir = .pkgenv)
  }
  tmp <- make_all_EQ_states('3L')
  tmp$state <- toEQ5Dindex(tmp)
  assign(x = "states_3L", value = tmp, envir = .pkgenv)
  tmp <- make_all_EQ_states('5L')
  tmp$state <- toEQ5Dindex(tmp)
  assign(x = "states_5L", value = tmp, envir = .pkgenv)
  
  assign(x = 'xwsets', value = .pkgenv$probs %*% cbind(as.matrix(.vsets5L[, -1, drop = F]), as.matrix(.uservsets5L[,-1, drop = F])), envir = .pkgenv)
  cntrs <- .cntrcodes[.cntrcodes$ISO3166Alpha2 %in% c(colnames(.vsets5L)[-1], colnames(.uservsets5L)[-1]),]
  rownames(cntrs) <- NULL
  assign(x = "country_codes", value = cntrs, envir = .pkgenv)
}
