#' Get Most Frequent Tokens from a Token List (with Stopword Removal using quanteda)
#'
#' Takes a list of character vectors, removes stopwords,
#' and returns a tibble of the most frequent tokens and their counts. 
#' Words to be removed can be standard stopwords (from stopwords/quanteda) or a custom list.
#'
#' @param token_list A list of character vectors (tokenized documents).
#' @param n_top The number of top tokens to return. Defaults to 100.
#' @param min_count The minimum number of times a token must appear. Defaults to 100.
#' @param stopword_language The language for the stopwords. Defaults to "en".
#' @param custom_stopwords A character vector of additional stop words to remove.
#'
#' @return A tibble with columns 'token' and 'n' (count), sorted by frequency.
#'
#' @importFrom quanteda tokens tokens_remove dfm topfeatures
#' @importFrom dplyr %>% rename filter select
#' @importFrom tibble tibble rownames_to_column
#' @importFrom stringr str_detect
#' @importFrom stopwords stopwords
#'
#' @examples
#' \dontrun{
#' docs <- list(
#'   c("new", "york", "is", "a", "city", "city"),
#'   c("new", "york", "is", "a", "big", "city"),
#'   c("the", "city", "is", "great", "city")
#' )
#' # Note: For a real test, min_count should be low for this small example.
#' top_tokens <- get_most_frequent_tokens(docs, n_top = 5, min_count = 1)
#' print(top_tokens)
#' # Expected output (excluding "is", "a", "the", etc.):
#' # token     n
#' # <chr> <dbl>
#' # city      4
#' # new       2
#' # york      2
#' }
get_most_frequent_tokens <- function(token_list, n_top = 100, min_count = 100, stopword_language = "en",
                                     custom_stopwords = NULL) {

  # 1. Convert the list of vectors into a quanteda tokens object
  # quanteda::tokens() is designed to handle lists of character vectors efficiently.

  # Check if the list is empty
  if (length(token_list) == 0 || all(sapply(token_list, length) == 0)) {
    warning("Input list is empty or contains no tokens.")
    return(tibble::tibble(token = character(0), n = integer(0)))
  }

  # Convert to a quanteda tokens object
  toks <- quanteda::tokens(token_list)

  # 2. Remove Stopwords
  # Combine default and custom stopwords
  all_stopwords <- c(stopwords::stopwords(stopword_language), custom_stopwords)

  # Use quanteda::tokens_remove for efficient stopword filtering
  toks_clean <- quanteda::tokens_remove(
    toks,
    pattern = all_stopwords
  )

  # 3. Create a Document-Feature Matrix (DFM) and get feature frequencies
  dfmat <- quanteda::dfm(toks_clean)

  # Get feature (token) frequencies, sorted
  token_frequencies <- quanteda::topfeatures(dfmat, n = n_top) %>%
    as.data.frame() %>%
    dplyr::rename(n = '.') %>%
    tibble::rownames_to_column("token") %>%
    # Apply minimum count filter
    dplyr::filter(n >= min_count) %>%

    # Remove obvious noise (URLs, single-letter/number tokens, etc.)
    dplyr::filter(!stringr::str_detect(token, "^http|^@|\\d$|\\s|\\W$|^[[:punct:]]$|^[[:digit:]]$|^.$")) %>%

    # Select the final top N tokens
    dplyr::select(token, n)

  return(token_frequencies)
}
