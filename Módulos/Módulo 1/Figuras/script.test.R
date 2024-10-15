
# Create a matrix with some NA values
data_matrix <- matrix(c(1, 2, NA, 4, 5, 6, NA, NA, NA), 
                      nrow = 3, byrow = TRUE)

# Use rowSums() but return NA if all values in the row are NA
row_sums_custom <- ifelse(rowSums(is.na(data_matrix)) == ncol(data_matrix), 
                          NA, 
                          rowSums(data_matrix, na.rm = TRUE))

# Display the result
row_sums_custom

gous <- structure(list(V1 = c(0, 28.44), V2 = c(1, 28.44), 
                       V3 = c(2,28.44), V4 = c(3, 28.39), 
                       V5 = c(4, 28.22), V6 = c(5, 27.72), 
                       V7 = c(6, 24.56), V8 = c(7, 18.78), 
                       V9 = c(8, 18.5), V10 = c(9,18.56), 
                       V11 = c(10, 18.5), V12 = c(11, 18.72)), 
                  .Names = c("V1", "V2", "V3", "V4", "V5", "V6", 
                             "V7", "V8", "V9", "V10", "V11",
                             "V12"), class = "data.frame", 
                  row.names = c(NA, -2L))
