#' Plot an aural landscape from a structured dataset
#'
#' `plotAuralLandscape` currently does a simple 2-d plot
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
#' @param taxonPalette Color palette for taxa; default is color-blind with black
#' @param aggregateYears Ignore year when plotting, aggregate all dates to yr = 1
#' @param showPlot Boolean; plot or just return object?
#'
#' @import tidyverse
#' @import lubridate
#' @import dplyr
#'
#' @examples
#' data(auralObservations)
#' plotAuralLandscape(auralObservations,
#'        taxa = 'robin|cardinal',
#'        taxaGrep = TRUE)
#'
#' @export
plotAuralLandscape <- function(
  x, plotType = c('hull', 'points'),
  dateRange = c(1:12), timeRange = c(3:22),
  observers = 'all', obsGrep = FALSE,
  taxa = 'all', taxaGrep = FALSE,
  aggregateYears = TRUE,
  taxonPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  showPlot = TRUE
) {
  require(ggplot2); require(magrittr); require(dplyr)
  x$date <- ymd(x$date)
  if(aggregateYears) year(x$date) <- 1
  # x$time <- hm(x$time)
  x <- x[month(x$date) %in% dateRange, ]
  x <- x[hour(hm(x$time)) %in% timeRange, ]
  if(observers != 'all') {
    if(!obsGrep) x <- x[x$observer %in% observers, ]
    if(obsGrep) x <- x[grep(observers, x$observer), ]
  }
  if(taxa != 'all') {
    if(!taxaGrep) x <- x[x$taxon %in% taxa, ]
    if(taxaGrep) x <- x[grep(taxa, x$taxon), ]
  }
  # x_hull <- x %>%
  #    group_by(taxon) %>%
  #    slice(chull(date, time))
  out <- ggplot(x, aes(x=date, y=time, color = taxon)) +
  out <- out + geom_point(size = 3)
  if(length(unique(x$taxon)) <= length(taxonPalette)) {
    scale_color_manual(values=taxonPalette)
  }
    # +
    # geom_polygon(data = x_hull, alpha = 0.5)
  if(showPlot) print(out)
  return(out)
}

#
# # Update the plot with a fill group, and overlay the new hulls
# p + aes(fill = factor(cyl)) + geom_polygon(data = hull_cyl, alpha = 0.5)
