#' Import Dictionary and Compute Subset Centroid in Embedding Space
#'
#' Takes a dictionary (as a character vector of tokens, a file path, or a data frame),
#' subselects the tokens that are present in the embedding vocabulary, and calculates
#' the centroid (mean vector, optionally normalized) of this subset in the embedding space.
#'
#' @param dictionary A character vector of words/tokens, a path to a text/csv file containing
#'   tokens, a data frame with a token column, or a list of token vectors.
#' @param embedding A pre-trained vector embeddings object: either an object of class \code{word2vec},
#'   or a numeric matrix where \code{rownames} contain the vocabulary tokens.
#' @param normalize Logical. If \code{TRUE} (default), unit-normalizes the resulting centroid vector
#'   so that its Euclidean (L2) norm equals 1.
#' @param normalize_tokens Logical. If \code{TRUE} (default), unit-normalizes each dictionary token's
#'   vector before computing the centroid, preventing high-frequency tokens with large vector magnitudes
#'   from dominating the direction of the centroid.
#' @param case_insensitive Logical. If \code{TRUE} (default), matches dictionary tokens to embedding
#'   vocabulary in a case-insensitive manner.
#' @param category Optional character string. If \code{dictionary} is a named list of categories
#'   (e.g., \code{list(anger = c(...), fear = c(...))}), specifies which category to extract.
#'   Defaults to \code{NULL} (uses all tokens in the list).
#'
#' @return An object of class \code{c("dictionary_centroid", "list")} containing:
#'   \itemize{
#'     \item \code{centroid}: A numeric vector representing the centroid of the matched dictionary tokens.
#'     \item \code{tokens_found}: A character vector of dictionary tokens found in the embedding.
#'     \item \code{tokens_missing}: A character vector of dictionary tokens not present in the embedding.
#'     \item \code{n_found}: Integer count of matched tokens.
#'     \item \code{n_missing}: Integer count of missing tokens.
#'     \item \code{total_tokens}: Total number of unique input dictionary tokens.
#'   }
#'
#' @importFrom utils head read.csv
#' @importFrom word2vec read.word2vec
#'
#' @examples
#' \dontrun{
#' # Dummy embedding matrix
#' vocab <- c("anger", "angry", "fury", "rage", "happy", "joy")
#' dummy_emb <- matrix(rnorm(length(vocab) * 50), nrow = length(vocab), dimnames = list(vocab, NULL))
#'
#' # Anger dictionary
#' anger_dict <- c("anger", "fury", "rage", "irritated")
#'
#' # Compute centroid
#' anger_centroid <- compute_dictionary_centroid(anger_dict, dummy_emb)
#' print(anger_centroid)
#' }
#' @export
compute_dictionary_centroid <- function(dictionary,
                                        embedding,
                                        normalize = TRUE,
                                        normalize_tokens = TRUE,
                                        case_insensitive = TRUE,
                                        category = NULL) {

  # 1. Extract embedding matrix
  emb_mat <- extract_embedding_matrix(embedding)

  # 2. Extract tokens from dictionary input
  if (is.character(dictionary) && length(dictionary) == 1 && file.exists(dictionary)) {
    # File path provided
    if (grepl("\\.(csv|tsv|txt)$", tolower(dictionary))) {
      lines_try <- tryCatch({
        df_read <- utils::read.csv(dictionary, stringsAsFactors = FALSE, header = FALSE)
        as.character(df_read[[1]])
      }, error = function(e) {
        readLines(dictionary, warn = FALSE)
      })
      dict_tokens <- lines_try
    } else {
      dict_tokens <- readLines(dictionary, warn = FALSE)
    }
  } else if (is.data.frame(dictionary)) {
    token_col <- intersect(c("token", "tokens", "word", "words", "term", "terms"), tolower(colnames(dictionary)))
    if (length(token_col) > 0) {
      dict_tokens <- as.character(dictionary[[token_col[1]]])
    } else {
      dict_tokens <- as.character(dictionary[[1]])
    }
  } else if (is.list(dictionary)) {
    if (!is.null(category) && category %in% names(dictionary)) {
      dict_tokens <- as.character(dictionary[[category]])
    } else {
      dict_tokens <- unlist(dictionary, use.names = FALSE)
    }
  } else if (is.character(dictionary)) {
    dict_tokens <- dictionary
  } else {
    stop("`dictionary` must be a character vector, file path, data frame, or list.")
  }

  # Clean dictionary tokens
  dict_tokens <- unique(trimws(as.character(dict_tokens)))
  dict_tokens <- dict_tokens[nzchar(dict_tokens)]

  if (length(dict_tokens) == 0) {
    stop("The provided dictionary does not contain any valid tokens.")
  }

  # 3. Subselect tokens present in the embedding
  vocab <- rownames(emb_mat)

  if (isTRUE(case_insensitive)) {
    vocab_compare <- tolower(vocab)
    dict_compare <- tolower(dict_tokens)

    matched_indices <- which(vocab_compare %in% dict_compare)
    # Deduplicate in case multiple rows match case-insensitively
    matched_indices <- matched_indices[!duplicated(vocab_compare[matched_indices])]

    tokens_found <- vocab[matched_indices]
    matched_dict_lower <- vocab_compare[matched_indices]
    tokens_missing <- dict_tokens[!dict_compare %in% matched_dict_lower]
  } else {
    matched_indices <- which(vocab %in% dict_tokens)
    tokens_found <- vocab[matched_indices]
    tokens_missing <- setdiff(dict_tokens, vocab)
  }

  if (length(tokens_found) == 0) {
    stop(paste(
      "None of the", length(dict_tokens),
      "dictionary tokens were found in the embedding vocabulary."
    ))
  }

  message(sprintf(
    "Dictionary tokens matched: %d of %d (%.1f%%).",
    length(tokens_found), length(dict_tokens),
    100 * length(tokens_found) / length(dict_tokens)
  ))

  if (length(tokens_missing) > 0) {
    preview <- paste(utils::head(tokens_missing, 6), collapse = ", ")
    message(sprintf(
      "Missing tokens (%d): %s%s",
      length(tokens_missing), preview,
      if (length(tokens_missing) > 6) "..." else ""
    ))
  }

  # 4. Extract token vectors and compute centroid
  sub_matrix <- emb_mat[matched_indices, , drop = FALSE]

  if (isTRUE(normalize_tokens)) {
    token_norms <- sqrt(rowSums(sub_matrix^2))
    token_norms[token_norms == 0] <- 1
    sub_matrix <- sub_matrix / token_norms
  }

  centroid <- colMeans(sub_matrix)

  if (isTRUE(normalize)) {
    c_norm <- sqrt(sum(centroid^2))
    if (c_norm > 0) {
      centroid <- centroid / c_norm
    }
  }

  result <- list(
    centroid = centroid,
    tokens_found = tokens_found,
    tokens_missing = tokens_missing,
    n_found = length(tokens_found),
    n_missing = length(tokens_missing),
    total_tokens = length(dict_tokens)
  )

  class(result) <- c("dictionary_centroid", "list")
  return(result)
}

#' @export
print.dictionary_centroid <- function(x, ...) {
  cat("Dictionary Centroid Vector\n")
  cat(sprintf("  Embedding dimensions : %d\n", length(x$centroid)))
  cat(sprintf("  Tokens matched       : %d / %d (%.1f%%)\n",
              x$n_found, x$total_tokens, 100 * x$n_found / x$total_tokens))
  if (length(x$tokens_found) > 0) {
    sample_tokens <- utils::head(x$tokens_found, 8)
    cat(sprintf("  Matched tokens sample: %s%s\n",
                paste(sample_tokens, collapse = ", "),
                if (length(x$tokens_found) > 8) "..." else ""))
  }
  if (length(x$tokens_missing) > 0) {
    cat(sprintf("  Missing tokens count : %d\n", x$n_missing))
  }
  invisible(x)
}

#' Extract Embedding Matrix from Various Input Types
#'
#' Internal helper to extract a numeric matrix with row names from a word2vec model,
#' an existing matrix, a data frame, or a file path to an .emb file.
#'
#' @param embedding A word2vec model, matrix, data.frame, or file path string.
#' @return A numeric matrix with row names representing token identifiers.
#' @noRd
extract_embedding_matrix <- function(embedding) {
  if (is.null(embedding)) {
    stop("`embedding` is NULL. Please provide a trained word2vec model, a numeric matrix with row names, or a file path to an .emb file.")
  }

  if (is.character(embedding) && length(embedding) == 1 && file.exists(embedding)) {
    message(paste("Loading embedding model from file:", embedding))
    embedding <- word2vec::read.word2vec(embedding)
  }

  if (inherits(embedding, "word2vec")) {
    emb_mat <- as.matrix(embedding)
  } else if (is.matrix(embedding) && is.numeric(embedding)) {
    emb_mat <- embedding
  } else if (is.data.frame(embedding)) {
    if (is.character(embedding[[1]]) || is.factor(embedding[[1]])) {
      tokens <- as.character(embedding[[1]])
      emb_mat <- as.matrix(embedding[, -1, drop = FALSE])
      rownames(emb_mat) <- tokens
    } else {
      emb_mat <- as.matrix(embedding)
    }
  } else {
    mat_try <- tryCatch(as.matrix(embedding), error = function(e) NULL)
    if (!is.null(mat_try) && is.numeric(mat_try) && !is.null(rownames(mat_try))) {
      emb_mat <- mat_try
    } else {
      stop(paste0(
        "`embedding` must be a matrix, data.frame, word2vec model, or file path to an .emb file. ",
        "Received object of class: '", paste(class(embedding), collapse = ", "), "'."
      ))
    }
  }

  if (is.null(rownames(emb_mat))) {
    stop("`embedding` must have rownames corresponding to tokens/words.")
  }

  return(emb_mat)
}

