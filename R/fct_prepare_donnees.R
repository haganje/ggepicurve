#' Create an object df_epidemic
#'
#' @param dates Vector of dates
#' @param id Vector of character. The id of each obs. If NULL, then id will be autoassigned in the order of the dates.
#' @param group_color Vector of character
#' @param group_facet_vertical By default NA.
#' @param group_facet_horizontal By default NA.
#' @param period_limit A character vector of size 1. Either "day", "week" (default) or "month".
#'
#' @return A data.frame
#' @export

prepare_df <- function(
  dates,
  id = NULL,
  group_color = NA,
  group_facet_vertical = "all",
  group_facet_horizontal = "all",
  period_limit = "week"
) {

  # Check input ----------------------------------------------------------

  stopifnot(
    # dates must be a Date class
    class(dates) == "Date",
    # no NA in dates
    !any(is.na(dates))
  )

  # Create id of not given
  if(is.null(id)) id <- rank(dates, ties.method = "first")

  # Create an ordered data frame
  df <- data.frame(
    id,
    dates,
    group_color,
    group_facet_horizontal,
    group_facet_vertical
  )

  # Assign a period for each obs ----------------------------------------------

  # Instead of nested ifelse or case funtion, use a list a dictionnary
  time_period_formats <- list(
    "day"   = "%m-%d",
    "week"  = "%W"   ,
    "month" = "%m"
  )

  # Get the proper format for strftime function
  period_format <- time_period_formats[[period_limit]]

  # format with Year to avoid a same period on different year
  period_full_format <- paste0("%Y-", period_format)

  # Get all the possible levels for the range of dates
  periods_levels <- create_all_period_levels(dates, period_full_format)

  # Assign name of the period for each obs
  period_name <- strftime(x = dates ,format = period_full_format)

  df$period_num <- strftime(x = dates ,format = period_format)

  # Ordered factor with all levels to plot correctly
  df$period <- ordered(period_name, levels = periods_levels)

  # Rank based on subgroups
  df$rank <- rank_by(dates, df$period, group_facet_horizontal, group_facet_vertical)

  class(df) <- c(class(df), "epidemic_df")

  df
}

#' Rank by group
#'
#' @description Rank the observations (e.g. dates) for each group.
#' @seealso This function is a wrapper around the \code{\link{rank}} and \code{\link{ave}} functions.
#' @param x A vector that could be coerced to numeric
#' @param ... Vectors of the same length than x, with groups
#' @return A vector with integer
rank_by <- function(x, ...){
  ave(x = as.numeric(x), ... = ..., FUN = function(x) rank(x, ties.method = "first"))
}

# For faceting, just stack prepare_df for each group (Keep prepare_df simple)


#' Make a ggplot from outbreak data
#'
#' @param df A data.frame prepared with the prepare_df function
#' @return A ggplot object
#' @import ggplot2
#' @export
#' @examples
#' df_outbreak <- prepare_df(dates = outbreak_sim$dates, group_color = outbreak_sim$yard)
#' plot_ggcurve(df_outbreak)

plot_ggcurve <- function(df, color_name = "", period_numeric = F){

  if(period_numeric)
    df$period <- as.integer(df$period_num)

  ggp <- ggplot(df) +
    aes(
      x = period,
      fill = group_color,
      y = rank,
      label = id
    ) +
    geom_label(show.legend = TRUE, vjust = "top", size = 4) +
    scale_y_continuous(
      name = "Nombre de cas",
      limits = c(0, NA),
      minor_breaks = NULL
    ) +
    scale_fill_discrete(name = color_name)

  if(period_numeric) {
    ggp <- ggp + scale_x_continuous(minor_breaks = F)
  } else {
    ggp <- ggp +
      scale_x_discrete(drop = FALSE) +
      theme(axis.text.x = element_text(angle = 90))
  }

  # Prepare faceting
  h_formula <- "."
  v_formula <- "."

  if(length(unique(df$group_facet_horizontal)) > 1) h_formula <- "group_facet_horizontal"
  if(length(unique(df$group_facet_vertical)) > 1) v_formula <- "group_facet_vertical"
  if(h_formula != "." | v_formula != ".") {
    facet_formula <- as.formula(paste0(v_formula, "~", h_formula))
    ggp <- ggp + facet_grid(facet_formula)
  }

  # TODO : ajouter l'année en trouvant à chaque fois le x mini pour chaque année
  ggp
}

#' Helper fonction
create_all_period_levels <- function(dates, format){
  date_min <- as.Date(min(dates))
  date_max <- as.Date(max(dates)) + 1
  seq_day <- seq(from = date_min, to = date_max, by = "day")
  unique(strftime(seq_day, format))
}