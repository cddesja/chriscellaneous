#' Makes univariate probability plots
#' @param x is the independent variable
#' @param xlab is the x-axis label
#' @param ylab is the x-axis label
#' @param outcome the dependent variable
prob_plot <- function(x, outcome, xlab){
  resp <- seq(min(x, na.rm = T), max(x, na.rm = T), by = .1)
  pred <- exp(coef(mod0)[[1]] + coef(mod0)[[2]]*resp) / (1 + exp(coef(mod0)[[1]] + coef(mod0)[[2]]*resp))
  obs_points <- aggregate(outcome, by = list(round(x)), mean, na.rm = T)
  plot(pred ~ resp, type = "l", xlab = xlab, ylab = ylab, ylim = c(min(obs_points$x, pred), max(obs_points$x, pred)))
  points(x = obs_points$Group.1, y = obs_points$x)
}
