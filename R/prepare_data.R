#' Prepare Text Data by Prefixing Speaker ID to the Text
#'
#' This function takes a data frame containing text and agent/speaker IDs,
#' constructs a unique speaker token (e.g., "speaker_authorID"), and prepends
#' this token to the text of each entry. This format is often required when
#' training language models to associate text segments with a specific speaker.
#'
#' @param data A data frame (or tibble) containing the text and speaker identifier columns.
#' @param text_column The unquoted column name in \code{data} containing the
#'   text content (e.g., \code{quote} or \code{text}). Uses tidy-selection syntax (\code{\{\{...\}\}}).
#'   Defaults to \code{"text"}.
#' @param speaker_id_column The unquoted column name in \code{data} containing the
#'   unique identifier for the agent or speaker (e.g., \code{author} or \code{user_id}).
#'   Defaults to \code{"author"}. Note: it is converted to lower case.
#' @param speaker_identifier A character string prefix used to construct the
#'   final speaker token. For example, if this is \code{"speaker"} and the
#'   \code{speaker_id_column} value is \code{"ID_001"}, the final token will be
#'   \code{"speaker_ID_001"}. Defaults to \code{"speaker"}.
#' @param remove_punctuation Logical. If \code{TRUE} (default), removes all punctuation
#'   from the text content while preserving hashtag (\code{#}) and mention (\code{@}) symbols.
#'
#' @return A character vector where each element is a single string composed of
#'   the generated speaker token, followed by a space, and then the original text.
#'
#' @importFrom dplyr %>% select mutate
#' @importFrom stringr str_replace_all str_squish
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' dummy_data <- tibble::tibble(
#'   author_id = c("userA", "userB", "userA"),
#'   message_text = c(
#'     "hello, world! #trending",
#'     "how are you doing? @userA",
#'     "i am fine..."
#'   )
#' )
#'
#' # Result: "speaker_userA hello world #trending", "speaker_userB how are you doing @userA", etc.
#' processed_text <- prepare_data(
#'   data = dummy_data,
#'   text_column = message_text,
#'   speaker_id_column = author_id,
#'   speaker_identifier = "speaker",
#'   remove_punctuation = TRUE
#' )
#' print(processed_text)
#' }
#' @export
prepare_data <- function(data,
                         text_column = "text",
                         speaker_id_column = "author",
                         speaker_identifier = "speaker",
                         remove_punctuation = TRUE) {

  speaker_identifier <- tolower(speaker_identifier)

  # Use the column names dynamically and ensure they are selected first
  data_processed <- data %>%
    dplyr::select(text = {{text_column}}, author = {{speaker_id_column}}) %>%
    # Convert author to lower case so it matches downstream landscape extraction
    dplyr::mutate(author = tolower(author))

  # Remove punctuation from text, preserving '#' and '@'
  if (isTRUE(remove_punctuation)) {
    data_processed <- data_processed %>%
      dplyr::mutate(
        text = stringr::str_squish(
          stringr::str_replace_all(text, "[^[:alnum:][:space:]#@_]", " ")
        )
      )
  }

  data_processed <- data_processed %>%
    # 1. Create the unique speaker token (e.g., "speaker_authorid")
    dplyr::mutate(speaker = paste(speaker_identifier, author, sep = "_")) %>%
    # 2. Concatenate the speaker token with the text
    dplyr::mutate(text = paste(speaker, text, sep = " ")) %>%
    # 3. Select only the final combined text column
    dplyr::select(text)

  # Return the text column as a character vector with speakers_text class
  text <- data_processed$text
  class(text) <- c("speakers_text", "character")
  return(text)
}
