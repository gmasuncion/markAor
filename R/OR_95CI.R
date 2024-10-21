#' @title Calculate Odds Ratios with 95\% Confidence Intervals
#' @description This function computes the OR and its 95\% confidence intervals from a logistic regression model.
#' @param coef Coefficients from a logistic regression model.
#' @param se Standard errors associated with the coefficients.
#' @param siglevel The significance level
#' @param roundto The precision for rounding the odds ratios and confidence intervals.
#' @return A formatted string
#' @author Mark Asuncion
#' @examples OR_95CI(0.5, 0.1, 0.05, 2)
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
