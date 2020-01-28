#' Add Remaining VVM Life annotations to a temperature time series
#'
#' This function adds reaction constants, instantaneous VVM Life Lost, and
#' cumulative VVM Life Lost columns to a
#' temperature time series. The calculations are done for the four most common
#' VVM types:
#' VVM2, VVM7, VVM14, and VVM30.
#'
#' @param df a data frame containing a temperature time series. Must include a \code{date} column containing the datetime, and a \code{temp} column containing the temperature in degrees Celsius.
#' @return the data frame, with extra columns for reaction constants and instantaneous and cumulative VVM life lost
#' @export
annotate_vvm = function(df) {
  date_diffs = diff(as.numeric(df$date))
  # How much time does the last sample represent? In case sampling is irregular,
  # calculate sampling interval as the median of the inter-sample distances.
  sampling_interval = median(date_diffs)
  df = df[order(df$date), ]
  df$temp_k = df$temp + 273.15
  df$seconds_represented = c(sampling_interval, date_diffs)
  df$days_represented = df$seconds_represented / 24 / 60 / 60
  df$k2 = 1.4422e17 * exp(-12429 / df$temp_k)
  df$k7 = 2.1532e18 * exp(-13652 / df$temp_k)
  df$k14 = 1.0766e18 * exp(-13652 / df$temp_k)
  df$k30 = 5.1131e17 * exp(-13657 / df$temp_k)
  df$pll2 = 100 * df$k2 * df$days_represented
  df$pll7 = 100 * df$k7 * df$days_represented
  df$pll14 = 100 * df$k14 * df$days_represented
  df$pll30 = 100 * df$k30 * df$days_represented
  df$cpll2 = cumsum(df$pll2)
  df$cpll7 = cumsum(df$pll7)
  df$cpll14 = cumsum(df$pll14)
  df$cpll30 = cumsum(df$pll30)
  return(df)
}
