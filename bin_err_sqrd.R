# this formula breaks down as efficiency goes to zero or 1

bin_err_sqrd <- function(npass, nfail) {
  ntotal = npass + nfail
  effic = npass / ntotal
  effic * (1 - effic) / ntotal
}

bin_err <- function(npass, nfail) {
  ntotal = npass + nfail
  effic = npass / ntotal
  sqrt(effic * (1 - effic) / ntotal)
}
