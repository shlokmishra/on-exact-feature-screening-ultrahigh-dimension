# Function to check if a string contains pairs (indicated by '-')
is_pair <- function(s) {
  return(grepl("-", s))
}

# Function to extract pairs or marginals from a string
extract_elements <- function(s) {
  return(unlist(strsplit(s, "[, {}]+")))
}

# Read and process each CSV file
csv_files <- c('signals_1.csv', 'signals_2.csv', 'signals_4.csv', 'signals_5.csv',
               'signals_6.csv', 'signals_7.csv', 'signals_8.csv')

for (csv_file in csv_files) {
  cat(paste0("\nProcessing ", csv_file, ":\n"))
  
  # Read the CSV file
  df <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  for (i in 1:nrow(df)) {
    listS_elements <- extract_elements(df$listS[i])
    
    # Check if listS contains pairs
    if (is_pair(df$listS[i])) {
      pairs_in_df <- extract_elements(df$pair[i])
      matched_pairs <- intersect(listS_elements, pairs_in_df)
      if (length(matched_pairs) > 0) {
        cat(paste0("Matched Pairs (Row ", i, "): ", paste(matched_pairs, collapse = ", "), "\n"))
      }
      
    } else {
      # listS contains marginals
      marginals_in_df <- extract_elements(df$marg[i])
      matched_marginals <- intersect(listS_elements, marginals_in_df)
      if (length(matched_marginals) > 0) {
        cat(paste0("Matched Marginals (Row ", i, "): ", paste(matched_marginals, collapse = ", "), "\n"))
      }
    }
  }
}
