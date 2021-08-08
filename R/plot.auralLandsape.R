#' Plot an aural landscape from a structured dataset
#'
#' `plot.auralLandscape` currently does a simple 2-d plot
#'   of observations by time of day and day of year. Eventually
#'   should have functions to (1) accommodate point observations
#'   and ranges, (2) subset by observers and areas, (3) allow
#'   for plotting customization.
#'
#' @param x `data.frame` of observations, with taxon, day, month, year, time (in 24 hr notation)
#' @param dateRange Vector of months to include
#' @param timeRange Vector of hours to include (24 hr notation)
#' @param observers Character vector of length 1; observers to include, or 'all'
#' @param obsGrep Boolean; use `grep` with `observers`?
#' @param taxa Character vector of length 1; taxa to include, or 'all'
#' @param taxaGrep Boolean; use `grep` with `taxa`?
#' @param showPlot Boolean; plot or just return object?
#' @import tidyverse
#' @import lubridate
#' @export
plot.auralLandscape <- function(
  x, plotType = c('hull', 'points'),
  dateRange = c(1:12), timeRange = c(3:22),
  observers = 'all', obsGrep = FALSE,
  taxa = 'all', taxaGrep = FALSE,
  show = TRUE
) {
  x <- x[month(x$date), ] %in% dateRange
  x <- x[hour(hms(x$time)), ] %in% timeRange
  if(observers != 'all') {
    if(!obsGrep) x <- x[x$observer %in% observers, ]
    if(obsGrep) x <- x[grep(observers, x$observer), ]
  }
  if(taxa != 'all') {
    if(!taxaGrep) x <- x[x$taxon %in% taxa, ]
    if(taxaGrep) x <- x[grep(taxa, x$taxon), ]
  }
  out <- ggplot(x, aes(x=date, y=time, colour = taxon)) +
    geom_point() +
    stat_chull(fill = NA)
  if(showPlot) print(out)
  return(out)
}
