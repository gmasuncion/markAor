#' @title Calculate Odds Ratios with 95\% Confidence Intervals
#' @description This function computes the OR and its 95\% confidence intervals from a logistic regression model.
#' @param coef Coefficients from a logistic regression model.
#' @param se Standard errors associated with the coefficients.
#' @param siglevel The significance level
#' @param roundto The precision for rounding the odds ratios and confidence intervals.
#' @return A formatted string
#' @author Mark Asuncion
#' @examples
#' coefs = c(-1.5, 0.02, 0.8, 0.1, 0.5, 0.03)
#' ses = c(0.3, 0.01, 0.2, 0.05, 0.15, 0.01)
#' OR_95CI(coefs, ses, 0.05, 2)
#' @export

OR_95CI <- function(coef, se, siglevel, roundto) {
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
