#' Load, Process, and Prepare Speaker Landscapes with UMAP
#'
#' This function performs a comprehensive pipeline: loading speaker data from a file
#' path or character vector, aggregating quotes by speaker, filtering speakers based
#' on quote count, retrieving their combined vector embeddings, and applying UMAP
#' for dimensionality reduction. It processes and returns the UMAP coordinates for
#' both the filtered speakers and a set of input tokens for visualization.
#'
#' @param data The input source. Can be either a character string specifying the path to
#'   the file containing text (The first token is assumed to be the speaker ID, and the rest forms the text.) OR an object of class \code{"speakers_text"} as created with \code{prepare_data()}.
#' @param embedding A pre-trained vector embeddings object (e.g., a matrix of
#'   word2vec embeddings) where \code{rownames} contain the tokens/speakers being mapped.
#' @param speaker_identifier A character string prefix used to identify the
#'   speaker tokens. This is converted to lower case. Defaults to \code{"speaker"}.
#' @param retain_threshold An integer threshold for the minimum number of
#'   quotes a speaker must have to be included in the final plot. Defaults to \code{1}.
#' @param umap_seed An integer seed for the random number generator used by UMAP
#'   to ensure reproducibility. Defaults to \code{42}.
#' @param n_neighbors UMAP hyperparameter. An integer specifying the number of nearest neighbors for
#'   the UMAP algorithm. Defaults to \code{40}. This parameter controls how UMAP balances local versus global structure in the data.
#'   Low values of n_neighbors will force UMAP to concentrate on very local structure, while large values may lose fine detail structure.
#'   For more information see \url{https://umap-learn.readthedocs.io/en/latest/parameters.html}.
#' @param min_dist UMAP hyperparameter. A numeric value specifying the minimum distance parameter for
#'   the UMAP algorithm. Defaults to \code{0.01}. It provides the minimum distance that points are allowed to be in the low dimensional representation.
#'   Low values of min_dist will result in clumpier embeddings. Larger values of min_dist will prevent UMAP from packing points together.
#'   For more information see \url{https://umap-learn.readthedocs.io/en/latest/parameters.html}.
#' @return A \code{list} containing two tibbles:
#'   \itemize{
#'     \item \code{speakers}: Data for the filtered speakers, including \code{author},
#'       \code{n_quotes}, \code{quotes} (list of original text segments for each speaker), and UMAP coordinates (\code{X1}, \code{X2}).
#'     \item \code{all_tokens}: Data for all mapped tokens (words and speakers) including \code{token}
#'       and UMAP coordinates (\code{X1}, \code{X2}).
#'   }
#' @details
#' The function relies on \code{tidyverse}, \code{umap}, and requires the
#' \code{embedding} object to be a matrix compatible with the UMAP input.
#'
#' **Input Flexibility:** If \code{inherits(data, "speakers_text")}, the
#' input is treated as an inline character vector; otherwise, if a valid file path, it is read line-by-line.
#'
#' @importFrom umap umap
#' @importFrom dplyr %>% mutate group_by summarise n ungroup select rename filter left_join
#' @importFrom readr read_lines
#' @importFrom tibble tibble as_tibble
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(umap)
#'
#' # 1. Setup Dummy Data
#' dummy_text_vector <- c(
#'   "speaker_A this is the first quote",
#'   "speaker_B second quote here",
#'   "speaker_A third line quote",
#'   "speaker_A another quote",
#'   "speaker_C fifth quote"
#' )
#' class(dummy_text_vector) <- c("speakers_text", "character")
#'
#' # Create dummy embeddings (assuming 5 tokens/authors and 10 dimensions)
#' vocab <- c("speaker_a", "speaker_b", "speaker_c", "word1", "word2")
#' dummy_embedding <- matrix(
#'   rnorm(length(vocab) * 10),
#'   nrow = length(vocab),
#'   dimnames = list(vocab, NULL)
#' )
#'
#' # 2. Run the processing function
#' landscape_data <- make_speaker_landscapes(
#'   data = dummy_text_vector,
#'   embedding = dummy_embedding,
#'   speaker_identifier = "speaker",
#'   retain_threshold = 2
#' )
#' print(landscape_data$speakers)
#' print(landscape_data$all_tokens)
#' }
#' @export
make_speaker_landscapes <- function(
    data,
    embedding = NULL,
    speaker_identifier = "speaker",
    retain_threshold = 1,
    umap_seed = 42,
    n_neighbors = 40,
    min_dist = 0.01
) {
  speaker_identifier <- tolower(speaker_identifier)

  # --- Load and process the data ---
  if (inherits(data, "speakers_text") || (is.character(data) && length(data) > 1)) {
    raw_lines <- as.character(data)
  } else if (is.character(data) && length(data) == 1 && file.exists(data)) {
    raw_lines <- readr::read_lines(data)
  } else {
    raw_lines <- as.character(data)
  }

  raw_lines <- raw_lines[nzchar(trimws(raw_lines))]

  # Fast, robust parsing of author (first token) and quote (remaining text)
  split_pos <- regexpr("\\s+", raw_lines)
  authors <- ifelse(split_pos > 0, substr(raw_lines, 1, split_pos - 1), raw_lines)
  quotes <- ifelse(split_pos > 0, substring(raw_lines, split_pos + attr(split_pos, "match.length")), "")

  df <- tibble::tibble(
    author = tolower(authors),
    quote = trimws(quotes)
  )

  # Summarise quotes for each speaker
  df_grouped <- df %>%
    dplyr::group_by(author) %>%
    dplyr::summarise(
      n_quotes = dplyr::n(),
      quotes = list(quote),
      .groups = "drop"
    )

  message(paste("Number of speakers in training set:", nrow(df_grouped)))

  # Filter based on the retention threshold
  df_filtered <- df_grouped %>%
    dplyr::filter(n_quotes >= retain_threshold)

  message(
    paste(
      "Number of speakers with at least",
      retain_threshold,
      "texts:",
      nrow(df_filtered)
    )
  )

  # --- Prepare Embeddings for UMAP ---
  all_vecs <- extract_embedding_matrix(embedding)

  # --- Apply UMAP for dimensionality reduction ---
  set.seed(umap_seed)
  message("Starting UMAP dimensionality reduction. This might take a while...")
  reducer <- umap::umap(
    d = all_vecs,
    metric = "cosine",
    n_neighbors = n_neighbors,
    min_dist = min_dist
  )

  # Store the low-dimensional vectors in a data frame
  low_dim_vecs <- tibble::as_tibble(reducer$layout, .name_repair = ~ c("X1", "X2")) %>%
    dplyr::mutate(token = rownames(all_vecs))

  # Separate reduced dimensions for speakers
  low_dim_speakers <- low_dim_vecs %>%
    dplyr::rename(author = token) %>%
    dplyr::mutate(author = tolower(author))

  # Final speaker Data (for plotting points)
  df_final <- df_filtered %>%
    dplyr::left_join(low_dim_speakers, by = "author") %>%
    dplyr::filter(!is.na(X1))

  # Return a list of both speaker and all token UMAP data
  return(list(speakers = df_final, all_tokens = low_dim_vecs))
}
