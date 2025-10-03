#' Identify and combine the most frequent, non-stopword bigrams in a text column.
#'
#' It analyzes a text column in a data frame, identifies the top \code{n_bigrams}
#' most frequent bigrams (two consecutive words), filters out those containing
#' common stop words, and replaces the identified bigrams in the text with a single
#' token where the words are joined by an underscore (e.g., "new york" becomes "new_york").
#' The input text is converted to lowercase prior to processing.
#'
#' @param data A data frame (or tibble) containing the text data.
#' @param text_col The unquoted column name in \code{data} that contains the
#'   text strings (e.g., \code{text} or \code{document}). This uses tidy-selection
#'   syntax (\code{\{\{...\}\}}).
#' @param n_bigrams An integer specifying the number of top bigrams to identify
#'   and combine into single tokens. Defaults to \code{10}.
#' @param stopwords_language A character string specifying the language for
#'   the default stop words, as supported by the \code{stopwords} package (e.g., \code{"en"}, \code{"de"}).
#'   Defaults to \code{"en"}. Use \code{NULL} if you only want to use \code{custom_stopwords}.
#' @param custom_stopwords A character vector of additional stop words to
#'   exclude from bigram formation. Defaults to \code{NULL}.
#'
#' @return A data frame (tibble) with the original data plus a new column named
#'   \code{text} containing the modified text with the most frequent, valid bigrams
#'   combined. The original text column is overwritten with the new, lowercased,
#'   and combined text. If no valid bigrams are found after filtering, the original
#'   data frame is returned with a warning, and a column named \code{text_combined}
#'   is created instead.
#'
#' @details
#' This function is a key preprocessing step for embedding models as it helps them
#' capture multi-word concepts (like "data science") as single, cohesive tokens,
#' which improves the quality of the resulting vectors.
#'
#' **Internal Workflow:**
#' 1. Text is tokenized into bigrams.
#' 2. The bigrams are split, and any pair containing a stop word (either default or custom)
#'    is removed via `anti_join`.
#' 3. The remaining bigrams are combined with an underscore and used as a lookup
#'    vector for replacement using \code{stringr::str_replace_all()}.
#'
#' @importFrom dplyr %>% mutate row_number anti_join filter count select
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#' @importFrom stopwords stopwords
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_corpus' is a tibble with a column named 'document_content'
#' my_corpus <- tibble::tibble(
#'   id = 1:2,
#'   document_content = c(
#'     "natural language processing is great fun",
#'     "i love natural language processing in r"
#'   )
#' )
#'
#' # Combine the top 5 bigrams, excluding default English stopwords
#' result <- create_bigrams(
#'   data = my_corpus,
#'   text_col = document_content,
#'   n_bigrams = 5
#' )
#'
#' # Expected output column 'text' will contain "natural_language" and/or "language_processing"
#' print(result$text)
#' }
#'
#' @export
create_bigrams <- function(data, text_col, n_bigrams = 10, stopwords_language="en",custom_stopwords=NULL) {

  # Note: The 'require()' calls are replaced by @importFrom tags for package development.

  # Prepare combined stopword list
  stopword_list <- stopwords::stopwords(language = stopwords_language)
  stopwords_df <- tibble::tibble(word = c(stopword_list, custom_stopwords)) %>%
    dplyr::filter(!is.na(word))

  # 1. Prepare Data and Create Bigram Counts
  bigram_counts <- data %>%
    # Add a temporary doc_id column to satisfy tidytext requirements
    dplyr::mutate(doc_id_temp = dplyr::row_number()) %>%

    # Tokenize into bigrams
    tidytext::unnest_tokens(
      bigram,
      {{text_col}}, # Use {{ }} here for tidy-selection
      token = "ngrams",
      n = 2
    ) %>%

    dplyr::count(bigram, sort = TRUE) %>%
    head(n_bigrams) %>%
    # Separate words to allow stop word filtering
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%

    # --- Stopword Removal using anti_join ---
    # Remove bigrams where word1 is a stop word
    dplyr::anti_join(stopwords_df, by = c("word1" = "word")) %>%
    # Remove bigrams where word2 is a stop word
    dplyr::anti_join(stopwords_df, by = c("word2" = "word")) %>%

    # Filter out NA/invalid bigrams
    dplyr::filter(!is.na(word1) & !is.na(word2)) %>%

    # Re-combine the words and count (to get final top N after filtering)
    dplyr::mutate(bigram = paste(word1, word2, sep = " ")) %>%
    dplyr::count(bigram, sort = TRUE) %>%
    head(n_bigrams) %>%

    # Separate back for replacement vector creation
    tidyr::separate(bigram, c("word1", "word2"), sep = " ", fill = "right")


  # Check if any bigrams were found
  if (nrow(bigram_counts) == 0) {
    warning("No valid bigrams found to combine. Returning original data.")
    # Return original data with an alternative column name to avoid accidental overwrites
    return(data %>% dplyr::mutate(text_combined = {{ text_col }}))
  }

  # 2. Prepare Replacement Vectors
  bigrams_to_combine <- bigram_counts %>%
    dplyr::mutate(old_value = paste(word1, word2, sep = " "),
                  new_value = paste(word1, word2, sep = "_"))


  # Create a named vector for str_replace_all: c("old string" = "new string")
  replacement_vector <- setNames(
    bigrams_to_combine$new_value,
    bigrams_to_combine$old_value
  )

  # 3. Apply Replacement to the Original Text

  # Apply replacement, converting string to lowercase first
  result_df <- data %>%
    dplyr::mutate(
      text = stringr::str_replace_all(
        string = tolower({{ text_col }}),
        pattern = replacement_vector
      )
    )

  # 4. Return the result
  return(result_df)
}
