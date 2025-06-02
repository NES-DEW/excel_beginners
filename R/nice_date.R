#' @title Nice date
#'
#' @description Create appropriately formatted dates for KIND work
#'
#' @param date A date- or Posix class date/date time
#'
#' @return A text string in the format (10:00) Mon 10th June 2024
#' @export
#'
#' @examples
#' nice_date(today())
nice_date <- function(date){
  
  if(lubridate::is.Date(date)) {
    paste(lubridate::wday(date, label = T),
          scales::ordinal(lubridate::day(date)),
          lubridate::month(date, label = T, abbr = F),
          lubridate::year(date))
  } else if(lubridate::is.POSIXct(date)) {
    paste(format(date, "%H:%M"),
          lubridate::wday(date, label = T),
          scales::ordinal(lubridate::day(date)),
          lubridate::month(date, label = T, abbr = F),
          lubridate::year(date))
  } else {
    "Dunno what to do with that"
  }
  
  
}
