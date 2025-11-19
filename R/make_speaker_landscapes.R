#' Load, Process, and Prepare Speaker Landscapes with UMAP
#'
#' This function performs a comprehensive pipeline: loading speaker data from a file
#' path or character vector, aggregating quotes by speaker, filtering speakers based
#' on quote count, retrieving their combined vector embeddings, and applying UMAP
#' for dimensionality reduction. It processes and returns the UMAP coordinates for
#' both the filtered speakers and a set of input tokens for visualization.
#'
#' @param data The input source. Can be either a character string specifying the path to
#'   the file contianing text (The first token is assumed to be the speaker ID, and the rest forms the text.) OR an object of class \code{"speakers_text"} as create with \code{prepare_data()}).
#'
#' @param embedding A pre-trained vector embeddings object (e.g., a matrix of
#'   word2vec embeddings) where \code{rownames} contain the tokens/speakers being mapped.
#' @param tokens_to_plot A data frame or tibble with a column named \code{token}
#'   containing a character vector of tokens (e.g., important words) whose
#'   embeddings should also be reduced and plotted as labels.
#' @param speaker_identifier A character string prefix used to identify the
#'   speaker tokens. This is used in the internal filtering step and it is transposed to low case. Defaults to \code{"speaker"}.
#' @param retain_threshold An integer threshold for the minimum number of
#'   quotes a speaker must have to be included in the final plot. Defaults to \code{1}.
#' @param umap_seed An integer seed for the random number generator used by UMAP
#'   to ensure reproducibility. Defaults to \code{42}.
#' @param n_neighbors UMAP hyperparameter. An integer specifying the number of nearest neighbors for
#'   the UMAP algorithm. Defaults to \code{10}. This parameter controls how UMAP balances local versus global structure in the data.
#'   It does this by constraining the size of the local neighborhood UMAP will look at when attempting to learn the manifold structure of the data.
#'   Low values of n_neighbors will force UMAP to concentrate on very local structure, potentially to the detriment of the big picture
#'   , while large values may lose fine detail structure. For more information see \ref{https://umap-learn.readthedocs.io/en/latest/parameters.html}.
#' @param min_dist UMAP hyperparameter. A numeric value specifying the minimum distance parameter for
#'   the UMAP algorithm. Defaults to \code{0.01}. It provides the minimum distance that points are allowed to be in the low dimensional representation.
#'   Low values of min_dist will result in clumpier embeddings. This can be useful if you are interested in clustering, or in finer topological structure.
#'   Larger values of min_dist will prevent UMAP from packing points together and will focus on the preservation of the broad topological structure instead. For more information see \ref{https://umap-learn.readthedocs.io/en/latest/parameters.html}.
#'
#' @return A \code{list} containing two tibbles:
#'   \itemize{
#'     \item \code{speakers}: Data for the filtered speakers, including \code{author},
#'       \code{n_quotes}, and UMAP coordinates (\code{X1}, \code{X2}).
#'     \item \code{words}: Data for the \code{tokens_to_plot}, including \code{words}
#'       (the token) and UMAP coordinates (\code{X1}, \code{X2}).
#'   }
#'
#' @details
#' The function relies on \code{tidyverse}, \code{umap}, and requires the
#' \code{embedding} object to be a matrix compatible with the UMAP input.
#'
#' **Input Flexibility:** If \code{class(data)} is \code{"speakers_text"}, the
#' input is treated as an inline character vector; otherwise, it is treated as a file path.
#'
#' @importFrom umap umap
#' @importFrom dplyr %>% mutate rowwise c_across group_by summarise n ungroup select rename filter left_join
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(umap)
#'
#' # 1. Setup Dummy Data
#' # Create a dummy text vector and set its class for inline processing
#' dummy_text_vector <- c(
#'   "speaker_A this is the first quote",
#'   "speaker_B second quote here",
#'   "speaker_A third line quote",
#'   "speaker_A another quote",
#'   "speaker_C fifth quote"
#' )
#' class(dummy_text_vector) <- "speakers_text"
#'
#' # Create dummy embeddings (assuming 5 tokens/authors and 10 dimensions)
#' vocab <- c("speaker_A", "speaker_B", "speaker_C", "word1", "word2")
#' dummy_embedding <- matrix(
#'   rnorm(length(vocab) * 10),
#'   nrow = length(vocab),
#'   dimnames = list(vocab, NULL)
#' )
#'
#' # Create dummy tokens to plot
#' dummy_tokens_to_plot <- tibble(token = c("word1", "word2"))
#'
#' # 2. Run the processing function
#' landscape_data <- speaker_landscapes(
#'   data = dummy_text_vector,
#'   embedding = dummy_embedding,
#'   tokens_to_plot = dummy_tokens_to_plot,
#'   retain_threshold = 2
#' )
#' print(landscape_data$speakers)
#' print(landscape_data$words)
#'
#' # 3. Plot the result (requires plot_speaker_landscapes function)
#' # plot_speaker_landscapes(landscape_data)
#' }
#' @export
make_speaker_landscapes <- function(
    data,
    embedding = NULL,
    tokens_to_plot = NULL,
    speaker_identifier = "speaker",
    retain_threshold = 1,
    umap_seed = 42,
    n_neighbors = 40,
    min_dist = 0.01
) {
  speaker_identifier = tolower(speaker_identifier)
  # --- Load and process the data ---
  if (class(data) == "speakers_text") {
    # Treat data as an inline character vector
    df <- readr::read_delim(
      I(data),
      delim = " ",
      col_names = FALSE,
      trim_ws = TRUE,
      skip_empty_rows = TRUE,
      show_col_types = FALSE
    )
  } else {
    # Assume data is a file path
    df <- read_delim(
      data, # Fixed: uses 'data' parameter as file path
      delim = " ",
      col_names = FALSE,
      trim_ws = TRUE,
      skip_empty_rows = TRUE,
      show_col_types = FALSE
    )
  }

  df = df %>%
    rename(author = X1) %>%
    # Combine the remaining columns into a single string for each quote
    rowwise() %>%
    mutate(
      quote = paste(c_across(X2:last_col()), collapse = " ")
    ) %>%
    ungroup() %>%
    select(author, quote) %>%
    mutate(author=tolower(author),
           quote=tolower(quote)
           )

  # Summarise quotes for each speaker
  df_grouped <- df %>%
    group_by(author) %>%
    summarise(
      quotes_list = list(quote),
      n_quotes = n()
    ) %>%
    ungroup()

  print(paste("Number of speakers in training set:", nrow(df_grouped)))

  # Filter based on the retention threshold
  df_filtered <- df_grouped %>%
    select(author, n_quotes) %>%
    filter(n_quotes >= retain_threshold)

  print(
    paste(
      "Number of speakers with at least",
      retain_threshold,
      "texts:",
      nrow(df_filtered)
    )
  )

  # --- Prepare Embeddings for UMAP ---
  all_vecs = as.matrix(embedding)

  # Filter authors (keeping the original filtering logic)
  filtered_authors <- df_filtered %>%
    # Use speaker_identifier to filter, adding TRUE to ensure it doesn't fail if no match
    filter(grepl(speaker_identifier, author) | TRUE) %>%
    select(author)

  # Combine author names and word tokens for a single UMAP run
  all_tokens_to_map <- unique(c(filtered_authors$author, tokens_to_plot$token))

  # Filter the embedding matrix
  filtered_vecs = all_vecs[rownames(all_vecs) %in% all_tokens_to_map, ]

  # --- Apply UMAP for dimensionality reduction ---
  set.seed(umap_seed)
  message("Starting UMAP dimensionality reduction. This might also take a while....")
  reducer <- umap(
    #d = filtered_vecs,
    d = all_vecs,
    metric = "cosine",
    n_neighbors = n_neighbors,
    min_dist = min_dist
  )

  # Store the low-dimensional vectors in a data frame
  low_dim_vecs = data.frame(reducer$layout) %>%
    mutate(token = rownames(.))

  # Separate reduced dimensions for speakers and words
  low_dim_speakers <- low_dim_vecs %>%
    filter(tolower(token) %in% tolower(df_filtered$author)) %>%
    rename(author = token)

  low_dim_words <- low_dim_vecs %>%
    filter(tolower(token) %in% tolower(tokens_to_plot$token)) %>%
    rename(words = token)

  # Final speaker Data (for plotting points)
  df_final <- df_filtered %>%
    left_join(low_dim_speakers, by = "author") %>%
    # Remove speakers that might not have had an embedding vector
    filter(!is.na(X1))

  # Return a list of both speaker and word UMAP data
  return(list(speakers = df_final, words = low_dim_words))
}
