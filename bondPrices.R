
discount.fn.semiannual <- function(annual_rate,years) {
  1/( (1 + annual_rate/2)^(2*years) )
}
