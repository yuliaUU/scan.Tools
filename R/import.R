#' @title s::can spectrolyzer output dataset import and processing
#'
#' @description This function selects physically meaningful variables (Date/Time, turbidity, nitrate, toc, doc)
#' from the default output table of s::can spectrolyzers. This function will process them in a tidy way when
#' given the serial number of the spectroylyzer you want to import.Ensure the data file ".par" is present in
#' the Raw_data folder
#'
#' @param SerialNumber
#' Provide the serial number of the spectrolyzer wrapped in "".
#' This name is chosen because the default  output data file is name after the serial number of the spectroylyzer.
#'
#' @return A tibble of 6 columns with length equal the number of measurement the spectrolyzer performed
#'
#' @examples import("09210014")
#'
#' @export

import <- function(SerialNumber) {
  # Import required packages
  library(tidyverse) # for working with data-time objects
  library(lubridate) # for working with data-time objects
  library(here)      # for importing via relative path

  if (!nchar(SerialNumber) == 8){
    stop ("Expected serial number string length of 8. Your serial number input has the string length of ", nchar(SerialNumber))}

  if (file.exists(here::here("Raw_data")) == TRUE) {
    x <- read_delim(here("Raw_data", paste(SerialNumber, ".par", sep="")),show_col_types = FALSE)} else {
      stop("Raw_data folder is not presend in current project directory.
           Please ensure folder is present with data file named after the serial
           number")}

  spec_tidy <- x  %>%
    #follows sensor's default output format ordering and neglect system operation parameters
    select (1,3,5,7,9)  %>%
    mutate (SN = SerialNumber, date = ymd_hms(`Date/Time`), 'Date/Time' = NULL)  %>%
    rename ('turbidity' = starts_with('Turbid.'),
            'nitrate' = starts_with('NO3-Neq'),
            'toc' = starts_with('TOCeq'),
            'doc' = starts_with('DOCeq'))
  return (spec_tidy)}
