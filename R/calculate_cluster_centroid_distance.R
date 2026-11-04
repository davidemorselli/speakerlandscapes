#' Calculate Normalized Cosine Proximity from Cluster Centroids to a Dictionary Centroid
#'
#' Computes the cosine proximity (semantic similarity) between each speaker cluster's centroid
#' and a target dictionary centroid vector (such as one computed with \code{\link{compute_dictionary_centroid}}).
#' By default, proximity values are normalized so that higher values indicate greater alignment
#' and semantic closeness to the concept centroid. Also supports 2D landscape distance/proximity.
#'
#' @param speaker_landscape A \code{list} object returned by \code{\link{make_speaker_landscapes}},
#'   optionally processed with \code{\link{extract_speaker_clusters}}.
#' @param centroid A numeric vector representing the concept centroid, or an object of class
#'   \code{"dictionary_centroid"} returned by \code{\link{compute_dictionary_centroid}}.
#'   Optional if \code{dictionary} is provided.
#' @param dictionary Optional dictionary of tokens (character vector, file path, data frame, or list).
#'   If provided along with \code{embedding}, the dictionary centroid is computed automatically
#'   via \code{\link{compute_dictionary_centroid}}.
#' @param embedding The vector embeddings object: an object of class \code{word2vec},
#'   a numeric matrix where \code{rownames} contain the speaker identifiers, or a file path to an .emb file.
#'   Required if \code{space = "embedding"} or if \code{dictionary} is provided.
#' @param space Character string specifying the space in which to compute proximity:
#'   \itemize{
#'     \item \code{"embedding"} (default): Computes semantic proximity in the full high-dimensional
#'       embedding space between each cluster's centroid vector and the dictionary centroid vector.
#'     \item \code{"umap_2d"}: Computes 2D Euclidean distance/proximity on the landscape between each cluster's
#'       2D center (\code{center_x}, \code{center_y}) and the 2D centroid of matched dictionary tokens
#'       in \code{speaker_landscape$all_tokens}.
#'     \item \code{"both"}: Computes both high-dimensional embedding proximity and 2D landscape metrics.
#'   }
#' @param metric Character string specifying the proximity/distance metric when \code{space = "embedding"} or \code{"both"}:
#'   \code{"normalized_proximity"} (default, min-max normalized where 1 is maximum closeness),
#'   \code{"cosine_similarity"} (raw cosine similarity in -1 to 1), or
#'   \code{"cosine_distance"} (1 - cosine similarity).
#' @param rescale Character string specifying how to normalize cosine proximity when \code{metric = "normalized_proximity"}:
#'   \code{"minmax"} (default, linearly scales observed similarities to 0 to 1 across clusters),
#'   \code{"unit"} (theoretical scaling via \code{(sim + 1) / 2}), or
#'   \code{"none"} (raw cosine similarity).
#' @param minPts Integer specifying \code{minPts} for \code{\link{extract_speaker_clusters}} if clusters
#'   have not yet been extracted from \code{speaker_landscape}. Defaults to \code{5}.
#'
#' @return A \code{tibble} containing the cluster summary with proximity columns appended:
#'   \itemize{
#'     \item \code{cluster}: Cluster ID integer.
#'     \item \code{cluster_label}: Cluster name (e.g., \code{"Cluster 1"}).
#'     \item \code{n_speakers}: Number of speakers in the cluster.
#'     \item \code{center_x}, \code{center_y}, \code{radius}: 2D coordinates and circle radius.
#'     \item \code{cosine_similarity}: Raw cosine similarity between cluster vector centroid and dictionary centroid (when \code{space \%in\% c("embedding", "both")}).
#'     \item \code{centroid_proximity} or \code{centroid_distance}: Embedding proximity (or distance if \code{metric = "cosine_distance"}).
#'     \item \code{dist_2d}: 2D Euclidean distance between cluster 2D center and dictionary 2D centroid (when \code{space \%in\% c("umap_2d", "both")}).
#'   }
#'   If \code{speaker_landscape} was passed as a full list, the updated list is also accessible via the
#'   attribute \code{"speaker_landscape"}, and \code{speaker_landscape$clusters} is updated with these columns.
#'
#' @importFrom dplyr %>% mutate left_join filter rename arrange pull
#' @importFrom tibble tibble as_tibble
#' @importFrom stats dist quantile
#'
#' @examples
#' \dontrun{
#' # 1. Extract landscape and dictionary centroid
#' anger_centroid <- compute_dictionary_centroid(c("anger", "rage", "fury"), embedding)
#'
#' # 2. Calculate normalized cosine proximity between cluster centroids and anger centroid
#' cluster_prox <- calculate_cluster_centroid_proximity(
#'   speaker_landscape = landscape,
#'   centroid = anger_centroid,
#'   embedding = embedding
#' )
#' print(cluster_prox)
#' }
#' @export
calculate_cluster_centroid_proximity <- function(speaker_landscape,
                                                 centroid = NULL,
                                                 dictionary = NULL,
                                                 embedding = NULL,
                                                 space = c("embedding", "umap_2d", "both"),
                                                 metric = c("normalized_proximity", "cosine_similarity", "cosine_distance"),
                                                 rescale = c("minmax", "unit", "none"),
                                                 minPts = 5) {
  space <- match.arg(space)
  metric <- match.arg(metric)
  rescale <- match.arg(rescale)

  # 1. If dictionary is provided, compute dictionary centroid
  if (!is.null(dictionary)) {
    if (is.null(embedding)) {
      stop("`embedding` must be provided when `dictionary` is specified.")
    }
    centroid <- compute_dictionary_centroid(dictionary, embedding)
  }

  if (is.null(centroid)) {
    stop("Please provide either `centroid` or `dictionary` (with `embedding`).")
  }

  # Extract centroid vector & matched tokens
  if (inherits(centroid, "dictionary_centroid") || (is.list(centroid) && "centroid" %in% names(centroid))) {
    c_vec <- as.numeric(centroid$centroid)
    dict_tokens_found <- if (!is.null(centroid$tokens_found)) tolower(as.character(centroid$tokens_found)) else character(0)
  } else if (is.numeric(centroid)) {
    c_vec <- as.numeric(centroid)
    dict_tokens_found <- character(0)
  } else {
    stop("`centroid` must be a numeric vector or an object returned by compute_dictionary_centroid().")
  }

  # 2. Extract or compute speaker clusters
  is_landscape_list <- is.list(speaker_landscape) && "speakers" %in% names(speaker_landscape)
  if (is_landscape_list) {
    if (is.null(speaker_landscape$clusters) || nrow(speaker_landscape$clusters) == 0) {
      speaker_landscape <- extract_speaker_clusters(speaker_landscape, minPts = minPts, embedding = embedding)
    }
    clusters_df <- speaker_landscape$clusters
    df_speakers <- speaker_landscape$speakers
  } else if (is.data.frame(speaker_landscape)) {
    df_speakers <- speaker_landscape
    if (!"cluster" %in% colnames(df_speakers)) {
      df_speakers <- extract_speaker_clusters(df_speakers, minPts = minPts, embedding = embedding)
    }
    clusters_df <- attr(df_speakers, "clusters")
    if (is.null(clusters_df)) {
      clustered_sub <- df_speakers %>% dplyr::filter(cluster > 0)
      if (nrow(clustered_sub) > 0) {
        clusters_df <- clustered_sub %>%
          dplyr::group_by(cluster, cluster_label) %>%
          dplyr::summarise(
            n_speakers = dplyr::n(),
            center_x = mean(X1, na.rm = TRUE),
            center_y = mean(X2, na.rm = TRUE),
            radius = as.numeric(
              stats::quantile(
                sqrt((X1 - mean(X1, na.rm = TRUE))^2 + (X2 - mean(X2, na.rm = TRUE))^2),
                probs = 0.95,
                na.rm = TRUE
              )
            ) * 1.05,
            .groups = "drop"
          )
      } else {
        clusters_df <- tibble::tibble()
      }
    }
  } else {
    stop("`speaker_landscape` must be a list returned by make_speaker_landscapes() or a data frame.")
  }

  if (is.null(clusters_df) || nrow(clusters_df) == 0) {
    message("No non-noise clusters found in `speaker_landscape`.")
    return(clusters_df)
  }

  res_df <- clusters_df

  # 3. High-dimensional embedding proximity / similarity / distance
  if (space %in% c("embedding", "both")) {
    if (is.null(embedding)) {
      stop("`embedding` must be provided when `space` is 'embedding' or 'both'.")
    }
    emb_mat <- extract_embedding_matrix(embedding)

    if (ncol(emb_mat) != length(c_vec)) {
      stop(sprintf(
        "Dimensionality mismatch: embedding has %d dimensions, but centroid has %d dimensions.",
        ncol(emb_mat), length(c_vec)
      ))
    }

    # Normalize centroid vector
    c_norm <- sqrt(sum(c_vec^2))
    if (c_norm == 0) c_norm <- 1

    emb_authors_lower <- tolower(rownames(emb_mat))

    clustered_speakers <- df_speakers %>%
      dplyr::filter(cluster > 0)

    # Compute high-dimensional centroid for each cluster
    sims <- rep(NA_real_, nrow(res_df))

    for (i in seq_len(nrow(res_df))) {
      cid <- res_df$cluster[i]
      c_authors <- clustered_speakers %>%
        dplyr::filter(cluster == cid) %>%
        dplyr::pull(author)

      match_idx <- match(tolower(as.character(c_authors)), emb_authors_lower)
      match_idx <- match_idx[!is.na(match_idx)]

      if (length(match_idx) > 0) {
        sub_mat <- emb_mat[match_idx, , drop = FALSE]
        # unit-normalize member speaker vectors before averaging
        row_norms <- sqrt(rowSums(sub_mat^2))
        row_norms[row_norms == 0] <- 1
        sub_mat_norm <- sub_mat / row_norms

        cluster_vec <- colMeans(sub_mat_norm)
        cl_norm <- sqrt(sum(cluster_vec^2))
        if (cl_norm == 0) cl_norm <- 1

        sims[i] <- as.numeric((cluster_vec %*% c_vec) / (cl_norm * c_norm))
      }
    }

    res_df$cosine_similarity <- sims

    if (metric == "normalized_proximity") {
      if (rescale == "minmax") {
        min_sim <- min(sims, na.rm = TRUE)
        max_sim <- max(sims, na.rm = TRUE)
        if (max_sim > min_sim) {
          res_df$centroid_proximity <- (sims - min_sim) / (max_sim - min_sim)
        } else {
          res_df$centroid_proximity <- rep(1.0, length(sims))
        }
      } else if (rescale == "unit") {
        res_df$centroid_proximity <- (sims + 1) / 2
      } else {
        res_df$centroid_proximity <- sims
      }
    } else if (metric == "cosine_similarity") {
      res_df$centroid_proximity <- sims
    } else if (metric == "cosine_distance") {
      res_df$centroid_distance <- 1 - sims
    }
  }

  # 4. 2D Euclidean distance on the landscape
  if (space %in% c("umap_2d", "both")) {
    all_tokens_df <- if (is_landscape_list) speaker_landscape$all_tokens else NULL

    if (!is.null(all_tokens_df) && nrow(all_tokens_df) > 0 && length(dict_tokens_found) > 0) {
      dict_tokens_lower <- tolower(dict_tokens_found)
      matched_tokens_2d <- all_tokens_df %>%
        dplyr::filter(tolower(token) %in% dict_tokens_lower)

      if (nrow(matched_tokens_2d) > 0) {
        dict_2d_x <- mean(matched_tokens_2d$X1, na.rm = TRUE)
        dict_2d_y <- mean(matched_tokens_2d$X2, na.rm = TRUE)

        res_df$dict_2d_x <- dict_2d_x
        res_df$dict_2d_y <- dict_2d_y
        res_df$dist_2d <- sqrt((res_df$center_x - dict_2d_x)^2 + (res_df$center_y - dict_2d_y)^2)
      } else {
        message("Dictionary tokens were not found in `all_tokens` to compute 2D dictionary centroid.")
      }
    } else if (space == "umap_2d") {
      message("`all_tokens` or dictionary token names not available to compute 2D landscape distance.")
    }
  }

  # Update speaker_landscape$clusters if list
  if (is_landscape_list) {
    speaker_landscape$clusters <- res_df
    attr(res_df, "speaker_landscape") <- speaker_landscape
  }

  return(res_df)
}

#' @rdname calculate_cluster_centroid_proximity
#' @export
calculate_cluster_centroid_distance <- function(speaker_landscape,
                                                centroid = NULL,
                                                dictionary = NULL,
                                                embedding = NULL,
                                                space = c("embedding", "umap_2d", "both"),
                                                metric = c("normalized_proximity", "cosine_similarity", "cosine_distance"),
                                                rescale = c("minmax", "unit", "none"),
                                                minPts = 5) {
  calculate_cluster_centroid_proximity(
    speaker_landscape = speaker_landscape,
    centroid = centroid,
    dictionary = dictionary,
    embedding = embedding,
    space = space,
    metric = metric,
    rescale = rescale,
    minPts = minPts
  )
}
