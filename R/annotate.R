#' Annotate a cleaned dataset
#'
#' Add time interval in hour from the oldest timestamp
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param by 'col' or 'row', default is 'col'. See ?g5h.gather_col for more info.
#'
#' @return data.frame
#' @export
#' @examples
#' \donttest{
#' # suppose "gen5_export.txt" is the export from Gen5 2.06
#'
#' g5h.clean2("gen5_export.txt") %>%
#'     g5h.annotate()
#' }
g5h.annotate <- function(.data, by='col'){
    .data %>%
        g5h.set_time2('hours')
}

#' Add time intervals to cleaned dataset
#'
#' g5h.set_time() preserves existing variables and add new variable,
#' time, which are time intervals in hours.
#'
#' @param .data data.frame cleaned by g5h.clean()
#' @param units "hours" or "minutes"
#'
#' @return input data.frame appended with time
g5h.set_time2 <- function(.data, units='hours') {
    #NULLing
    realTime <- time <- NULL
    .data %>%
        arrange(desc(realTime)) %>%
        mutate(time = as.numeric(difftime(realTime,
                                          realTime[length(realTime)],
                                          units = units)))
}
