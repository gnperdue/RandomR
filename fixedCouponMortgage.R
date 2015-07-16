require(plyr)
require(ggplot2)

mortgage.calc <- function(price, annualizedInterestRate=0.05, lengthInYears=30, 
    down.payment.factor=0.2, tax.rate=0.03, annual.insurance=1000) {
  down.payment <- price*down.payment.factor
  loan <- price - down.payment
  monthly.payment <- fixed.coupon(loan, annualizedInterestRate, lengthInYears)
  taxes <- price * tax.rate
  monthly.taxes <- taxes / 12
  closing <- price*0.06
  cat("Price          = ", price, "\n")
  cat("Loan           = ", loan, "\n")
  cat("Down Payment + Closing = ", down.payment, " + " , 
    closing, " = ", down.payment + closing, "\n")
  cat("Monthly Loan + Taxes = ", 
    monthly.payment, " + ", monthly.taxes, " = ", 
    monthly.payment + monthly.taxes, "\n")
  cat("Monthly Loan + Taxes + Insurance = ", 
    monthly.payment, " + ", monthly.taxes, " + ", annual.insurance/12, " = ", 
    monthly.payment + monthly.taxes + annual.insurance/12, "\n")
  cat("Annual Loan + Taxes = ", 
    monthly.payment*12, " + ", taxes, " = ", 
    monthly.payment*12 + taxes, "\n")
  cat("Annual Loan + Taxes + Insurance = ", 
    monthly.payment*12, " + ", taxes, " + ", annual.insurance, " = ", 
    monthly.payment*12 + taxes + annual.insurance, "\n")

  requisite.salary <- (monthly.payment + monthly.taxes + annual.insurance/12)/0.28
  cat("Requisite Monthly Income = ", requisite.salary, "\n")
  
  ret_list <- list(price, down.payment, annualizedInterestRate, lengthInYears, closing,
                   annual.insurance/12, monthly.payment, monthly.taxes)
  names(ret_list) <- c("Purchase Price", "Down Payment", "Annualized Interest Rate", 
                       "Mortgage Length in Years", "Closing Costs (est.)",
                       "Monthly Insurance (est.)", "Monthly Coupon Payment", 
                       "Monthly Taxes (est.)")
  ret_list
}

fixed.coupon <- function(principle, annualizedInterestRate=0.05, lengthInYears=30) {
  vv = 1:(lengthInYears*12)
  discountsVec = sapply(vv, function(i){ 1 / (1 + annualizedInterestRate/12)^i })
  discountsSum = sum(discountsVec)
  principle/discountsSum
}

make.mortgage.df <- function() {
  prices <- seq(250000, 350000, by=5000)
  dp.factors <- 80000/prices
  
  l <- list()
  for (i in 1:length(prices)) {
    p <- prices[i]
    dp <- dp.factors[i]
    results_list <- mortgage.calc(price=p, down.payment.factor=dp)
    l <- append(l, list(results_list))  
  } 
  mydf <- ldply(l, data.frame)
}

make.plot <- function(mydf) {
  ggplot(mydf, aes(x=Purchase.Price, 
                   y=Monthly.Insurance..est.. + Monthly.Taxes..est.. + 
                     Monthly.Coupon.Payment)) + geom_line()
} 