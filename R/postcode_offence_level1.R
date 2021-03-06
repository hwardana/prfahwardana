#' postcode_offencelvl1
#'
#' \code{<postcode_offencelvl1>} a function that returns a graph of monthly total offences
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of "offence_level_1"
#' @param postcodes A two-element character vector. Each element is an SA postcode.
#' @export
#' @return  A ggplot object showing the monthly total offences of the input offence_level_1 in the two input postcodes.
#' @examples
#' offence_description <- "ACTS INTENDED TO CAUSE INJURY"
#' postcodes <- c("5006", "5082")
#' crime_data_12 <- setDT(read_excel("data/crime-statistics-2012-13.xlsx"))
#' setnames(crime_data_12, c("date", "suburb", "postcode", "offence_level_1",
#'                          "offence_level_2", "offence_level_3", "offence_count"))
#'
#' postcode_offencelvl1(crime_data, offence_description, postcodes)
#'
#'

postcode_offencelvl1 <- function(crime_data, offence_description, postcodes) {
  require(data.table)
  require(ggplot2)

  # Error catching

  if (length(postcodes) != 2) {
    stop("Please enter two postcodes")
  }

  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  if (!all.equal(colnames(crime_data), expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input suburbs and offence description exist in crime_data
  if (any(!postcodes %in% crime_data$postcode) |
      !offence_description %in% crime_data$offence_level_1) {
    stop("Please enter the correct format of postcodes and offence description")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "postcode", "total_offence_count"
  plot_data <- crime_data[,
                          list(total_offence_count = sum(offence_count)),
                          by = list(month(date), postcode)]

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, postcode := plyr::mapvalues(postcode, postcodes, c("x", "y"))]

  plot_data <- dcast(plot_data, month ~ postcode, fun = sum,
                     fill = 0, value.var = "total_offence_count")


  # Plotting and combining two postcodes
  plot_data_x <- rbind(plot_data$month, plot_data$x, postcodes[1])
  plot_data_y <- rbind(plot_data$month, plot_data$y, postcodes[2])
  plot_data_xy <- data.frame(t(cbind(plot_data_x,plot_data_y)))

  # Set column names for the combined plot_data_xy
  setnames(plot_data_xy, c("Month", "Total_Offences", "Postcode"))
  plot_data_xy$Month <- factor(plot_data_xy$Month, level = c("7","8","9","10","11","12","1","2","3","4","5","6"))

  # Generate the plot
  ggplot(plot_data_xy, aes(x = Month, y = Total_Offences, color = Postcode)) + geom_point()

}
