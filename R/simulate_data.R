#' Simulate outbreak data
#'
#' @return A data.frame
#' @export

simulate_outbreak <- function(
  n_obs = 60,
  date_range = as.Date(x = c("2016-12-01", "2017-01-30"))
){
  id <- seq_len(n_obs)
  dates_int_range <- date_range[1]:date_range[2]
  dates <- as.Date(sample(dates_int_range, size = n_obs, replace = TRUE), origin = as.Date("1970-01-01"))
  yard <- sample(c("y1", "y2", "y3"), size = n_obs, replace = T)
  type <- sample(c("case", "suspect", "unknown"), size = n_obs, replace = T)

  return(data.frame(stringsAsFactors = FALSE, id, dates, yard, type))
}
