#' Preprocess a text corpus for embedding.
#'
#' Reads a text file, performs bigram and trigram substitution (using the
#' function \code{create_bigrams}), and tokenizes the resulting text. Optionally, it
#' subsamples the data.
#'
#' @param data A speakers_text object generated with \code{prepare_data()} or a character string specifying the path to the text file
#'   to be read. The file is read line-by-line.
#' @param share_data A numeric value between 0.0 and 1.0 (inclusive) indicating
#'   the proportion of the data to keep. Defaults to \code{1.0} (keeping all data).
#' @param stopwords_language A character string specifying the language for
#'   the default stop words, as supported by the \code{stopwords} package.
#'   Defaults to \code{"en"} (English). Use \code{NULL} to skip loading default stop words.
#' @param custom_stopwords A character vector of additional stop words to
#'   exclude from bigram formation. Defaults to \code{NULL}.
#' @return A list where each element is a character vector representing a
#'   tokenized line (i.e., a list of words). If \code{share_data < 1.0},
#'   this list will be a random subsample of the lines.
#'
#' @details
#' The function relies heavily on Tidyverse packages and requires an external function
#' named \code{create_bigrams} to be defined in the search path.
#'
#' \code{create_bigrams} is called twice to perform bigram and then trigram substitution.
#'
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' # Assuming 'text_data.txt' exists and 'create_bigrams' is defined:
#' # The function requires an external helper function, e.g.:
#' # create_bigrams <- function(data, text_col, n_bigrams) {
#' #   # This is a placeholder and would contain the actual logic
#' #   # for finding and replacing n-grams.
#' #   return(data)
#' # }
#'
#' # Full dataset
#' preprocessed_full <- preprocess_data("path/to/text_data.txt")
#'
#' # Subsample 10% of the data
#' preprocessed_sample <- preprocess_data("path/to/text_data.txt", share_data = 0.1)
#' }
#'
#' @export
preprocess_data <- function(data, share_data = 1.0, stopwords_language = "en", custom_stopwords=NULL) {

  # Read the entire file into a tibble
  if(class(data) == "speakers_text"){
    corpus = data
  } else {
    corpus = readr::read_lines(data)
  }

  corpus = tibble::tibble(text = corpus)

  # find and replace bigrams
  print("Making bigrams.....please wait.....")
  corpus = create_bigrams(corpus, text, n_bigrams = 10, stopwords_language = stopwords_language, custom_stopwords = custom_stopwords)

  # find and replace trigrams (by running create_bigrams again on the already processed text)
  print("Making trigrams.....please wait.....")
  corpus = create_bigrams(corpus, text, n_bigrams = 10, stopwords_language = stopwords_language, custom_stopwords = custom_stopwords)

  print("Preparing data for embedding.....")
  data_tbl = corpus$text %>%
    # Split each line into words
    purrr::map(~ stringr::str_split(., " ")[[1]]) %>%
    # Optional: If you need to subsample
    {
      if (share_data < 1.0) {
        sample(., size = length(.) * share_data)
      } else {
        .
      }
    }

  return(data_tbl)
}
