#' fars_read
#'
#' Reads .csv format data from the filepath and returns a tibble with the data.
#'
#' @param filename Filepath of the file to be read.
#'
#' @return data in tibble format.
#' @export
#'
#' @importFrom readr read_csv
#'
#' @examples
#'
#' fars_read(filename='C:/Users/myname/Data/file2read.csv')
#'
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' make_filename
#'
#' Creates a valid file name for csv.bz2 format with accident prefix using the year introduced as an input.
#'
#'
#' @param year year to be pasted in the output name
#'
#' @return Valid file name with the input year.
#' @export
#'
#' @examples
#'
#' make_filename('1990')
#'
make_filename <- function(year) {

  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' fars_read_years
#'
#' @param years Reads the data from input years
#'
#' @return A list with the data from input years, and an extra column with year
#' @export
#' @importFrom dplyr mutate select
#'
#' @examples
#' fars_read_years(c('1990', '1991', '2010'))
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' @param years Vector with the years to be summarized
#'
#' @return Summarized table of the input years
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#'
#' @examples
#' fars_summarize_years(c('1990', '1991', '2010'))
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' Maps the data belonging to input state in input year.
#'
#' @param state.num number of the state to be mapped
#' @param year year to get the data from
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'
#' fars_map_state('0001', '1999
#' ')
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
