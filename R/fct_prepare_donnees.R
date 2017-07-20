#' Create an object df_epidemic
#'
#' @param dates Vector of dates
#' @param group_color Vector of character
#' @param groupe_facet Vector of character
#' @param period_limit A character vector of size 1. Either "day", "week" (default) or "month".
#' @param id Vector of character. The id of each obs. If NULL, then id will be autoassigned in the order of the dates.
#'
#' @return A data.frame
#' @export

prepare_df <- function(
  dates,
  group_color,
  groupe_facet = NA,
  period_limit = "week",
  id = NULL
  ) {

  # Check input ----------------------------------------------------------

  stopifnot(
    # dates must be a Date class
    class(dates) == "Date",
    # no NA in dates
    !any(is.na(dates))
    )

  # Create id of not given
  date_order <- rank(dates, ties.method = "first")

  if(is.null(id)) id <- date_order

  # Create an ordered data frame
  df <- data.frame(
    id,
    date_order,
    dates,
    group_color
  )

  time_period_formats <- list(
    "day"   = "%m-%d",
    "week"  = "%W"   ,
    "month" = "%m"
  )

  period_format <- time_period_formats[[period_limit]]

  period_full_format <- paste0("%Y-", period_format)

  periods_levels <- create_all_period_levels(dates, period_full_format)

  period_name <- strftime(x = dates ,format = period_full_format)

  # Ordered factor with all levels to plot correctly
  df$period <- ordered(period_name, levels = periods_levels)

  df <- df[order(df$dates), ]

  #TODO: Refactor code (as a function)
  df$ordre_periode <- as.integer(unlist(aggregate(date_order ~ period, data = df, FUN = order)[["date_order"]]))

  # Placeholder for faceting group
  df$group_facet <- "All"
  df$group_facet_rank <- df$ordre_periode

  class(df) <- c(class(df), "epidemic_df")

  df
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

plot_ggcurve <- function(df){
  ggp <- ggplot(df) +
    aes(
      x = period,
      fill = group_color,
      y = group_facet_rank,
      label = id
      ) +
    geom_label(show.legend = TRUE, vjust = "top", size = 4) +
    scale_y_continuous(
      name = "Nombre de cas\n par semaine",
      limits = c(0, max(df$group_facet_rank)),
      minor_breaks = NULL
    ) +
    scale_x_discrete(drop = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(group_facet~.)

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