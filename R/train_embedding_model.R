#' Train a Word2Vec Embedding Model from Raw Text Data
#'
#' This function orchestrates the entire training pipeline for a skip-gram Word2Vec
#' model. It first preprocesses the raw text data (using the \code{preprocess_data}
#' function), trains the model, and finally saves the resulting
#' vector embeddings to a specified file path.
#'
#' @param file_path A character string specifying the path to the raw input text file. The file should contain speaker identifier as the first token of each line in. Speakers identifiers should start with a non equivocal token (e.g., speaker_ID1, speaker_ID2, ...), see \code{prepare_data()}
#' @param output_file A character string specifying the file path where the
#'   trained Word2Vec model (\code{.emb} format) will be saved.
#'   Defaults to \code{"embeddings.emb"}.
#' @param dim An integer specifying the dimensionality (vector size) of the
#'   word embeddings. Defaults to \code{250}.
#' @param window An integer specifying the context window size for the skip-gram model.
#'   Defaults to \code{10}.
#' @param min_count An integer specifying the minimum frequency required for a
#'   token to be included in the vocabulary. Defaults to \code{5}.
#' @param iter An integer specifying the number of training iterations (epochs).
#'   Defaults to \code{15}.
#' @param threads An integer specifying the number of CPU threads to use for
#'   parallel computing during training. Defaults to \code{7}.
#' @param seed An integer used to set the random seed for reproducibility.
#'   Defaults to \code{123}.
#' @param stopwords_language A character string specifying the language for
#'   the default stop words, as supported by the \code{stopwords} package.
#'   Defaults to \code{"en"} (English). Use \code{NULL} to skip loading default stop words.
#' @param custom_stopwords A character vector of additional stop words to
#'   exclude from bigram formation. Defaults to \code{NULL}.
#'
#' @return The trained \code{word2vec} model object (an S3 object of class \code{word2vec}).
#'   The model is also saved to disk at the path specified by \code{output_file}.
#'
#' @details
#' This function assumes the existence of two helper functions in the current R environment:
#' 1. \code{preprocess_data()}: Loads and tokenizes the raw data. It must return a
#'    list of character vectors (tokenized sentences).
#' 2. \code{get_most_frequent_tokens()}: Used to identify the top tokens (though not directly
#'    called in the training flow, it is typically used for downstream analysis).
#'
#' @importFrom word2vec word2vec write.word2vec
#'
#' @examples
#' \dontrun{
#' # Define placeholder helpers for a runnable example context
#' preprocess_data <- function(data, share_data, stopwords_language, custom_stopwords) {
#'   # Reads file and returns tokenized data
#'   list(c("this", "is", "a", "test", "sentence"), c("another", "test", "sentence", "here"))
#' }
#'
#' # Example file path (replace with actual path)
#' dummy_file_path <- "corpus.txt"
#'
#' # Train the model with default parameters, saving to "my_embeddings.emb"
#' trained_model <- train_embedding_model(
#'   file_path = dummy_file_path,
#'   output_file = "my_embeddings.emb",
#'   dim = 100,
#'   iter = 10
#' )
#'
#' # You can then access the embeddings:
#' # embedding_matrix <- as.matrix(trained_model)
#' }
#' @export
train_embedding_model <- function(
    file_path,
    output_file = "embeddings.emb",
    dim = 250,
    window = 10,
    min_count = 5,
    iter = 20,
    threads = 7,
    seed = 123,
    stopwords_language = "en",
    custom_stopwords = NULL,
    ...
) {

  message("Starting preprocessing data")
  # Preprocessing function
  training_data <- preprocess_data(file_path, share_data = 1.0, stopwords_language = stopwords_language, custom_stopwords = custom_stopwords)

  # --- Model Training ---
  # Set seeds for reproducibility
  set.seed(seed)

  message("Starting Word2Vec model training...this might take a while, sit tight and have a cup of tea!")

  # Train the Word2Vec model
  model <- word2vec::word2vec(
    x = training_data,
    type = "skip-gram", # sg = 1
    dim = dim,          # vector_size
    window = window,
    min_count = min_count,
    iter = iter,        # epochs
    threads = threads,   # CPU cores for parallel computing
    verbose = T
  )

  # Save the model
  word2vec::write.word2vec(model, output_file)
  message(paste("Word2Vec model successfully saved to:", output_file))

  return(model)
}
