#' Extract Speaker Clusters using HDBSCAN
#'
#' Applies Hierarchical Density-Based Spatial Clustering of Applications with Noise
#' (HDBSCAN) via \code{\link[dbscan]{hdbscan}} to speaker embeddings (either the 2D UMAP
#' coordinates from \code{\link{make_speaker_landscapes}} or high-dimensional embedding vectors)
#' and computes 2D cluster summary statistics (center and radius based on speaker locations
#' on the 2D landscape plot).
#'
#' @param speaker_landscape A \code{list} object returned by \code{\link{make_speaker_landscapes}},
#'   containing \code{$speakers} with \code{author}, \code{X1}, and \code{X2} columns,
#'   or a data frame with those columns.
#' @param minPts Integer specifying the minimum size of clusters (\code{minPts} passed to
#'   \code{\link[dbscan]{hdbscan}}). Defaults to \code{5}.
#' @param cluster_on Character string specifying which representation to cluster on:
#'   \code{"umap"} (default, clusters on the 2D UMAP coordinates \code{X1} and \code{X2},
#'   matching Schuld, Durrheim & Mafunda, 2024) or \code{"embedding"} (clusters on the
#'   full high-dimensional speaker vectors using cosine distance).
#' @param embedding Optional vector embeddings matrix, \code{word2vec} model, or file path to an .emb file.
#'   Required only when \code{cluster_on = "embedding"}.
#' @param radius_quantile Numeric value in \code{(0, 1]} specifying the quantile of member distances
#'   from the cluster center used to define each cluster circle's radius on the 2D landscape plot.
#'   Defaults to \code{0.95} (enclosing 95 percent of speakers in the cluster; use \code{1} to enclose 100 percent).
#' @param radius_padding Numeric multiplier applied to the computed cluster radius to add visual
#'   breathing room around the speakers on the 2D plot. Defaults to \code{1.05}.
#' @param ... Additional arguments passed to \code{\link[dbscan]{hdbscan}}.
#'
#' @return If \code{speaker_landscape} is a list, returns the updated \code{speaker_landscape} list with:
#'   \itemize{
#'     \item \code{$speakers}: Updated with \code{cluster} (integer cluster ID; \code{0} indicates noise),
#'       \code{cluster_label} (character label such as \code{"Cluster 1"} or \code{"Noise"}),
#'       \code{membership_prob} (HDBSCAN cluster membership probability), and
#'       \code{outlier_score} (GLOSH outlier score).
#'     \item \code{$clusters}: A \code{tibble} summarizing each non-noise cluster on the 2D landscape,
#'       including \code{cluster}, \code{cluster_label}, \code{n_speakers}, \code{center_x},
#'       \code{center_y}, \code{radius} (based on speaker locations on the 2D plot),
#'       \code{mean_dist}, \code{sd_dist}, and \code{max_dist}.
#'     \item \code{$hdbscan_model}: The raw \code{hdbscan} object returned by \code{dbscan::hdbscan()}.
#'   }
#'   If \code{speaker_landscape} is a data frame, returns the updated data frame with
#'   \code{"clusters"} and \code{"hdbscan_model"} attached as attributes.
#'
#' @importFrom dbscan hdbscan
#' @importFrom dplyr %>% mutate filter group_by summarise n arrange
#' @importFrom tibble tibble
#' @importFrom stats dist quantile sd
#'
#' @examples
#' \dontrun{
#' # Given a speaker landscape generated with make_speaker_landscapes():
#' landscape <- extract_speaker_clusters(landscape, minPts = 5)
#' print(landscape$clusters)
#'
#' # Plot the landscape with cluster circles
#' plot_speaker_landscapes(landscape, show_clusters = TRUE)
#' }
#' @export
extract_speaker_clusters <- function(speaker_landscape,
                                     minPts = 10,
                                     cluster_on = c("umap", "embedding"),
                                     embedding = NULL,
                                     radius_quantile = 0.95,
                                     radius_padding = 1.05,
                                     ...) {
  cluster_on <- match.arg(cluster_on)

  is_landscape_list <- is.list(speaker_landscape) && "speakers" %in% names(speaker_landscape)
  if (is_landscape_list) {
    df_speakers <- speaker_landscape$speakers
  } else if (is.data.frame(speaker_landscape)) {
    df_speakers <- speaker_landscape
  } else {
    stop("`speaker_landscape` must be a list returned by make_speaker_landscapes() or a data frame.")
  }

  if (!all(c("X1", "X2") %in% colnames(df_speakers))) {
    stop("`speaker_landscape` must contain 2D coordinates 'X1' and 'X2' in the speakers table.")
  }

  if (nrow(df_speakers) < minPts) {
    stop(sprintf(
      "Number of speakers (%d) is smaller than `minPts` (%d).",
      nrow(df_speakers), minPts
    ))
  }

  if (cluster_on == "umap") {
    coords_mat <- as.matrix(df_speakers[, c("X1", "X2")])
    hdb_res <- dbscan::hdbscan(coords_mat, minPts = minPts, ...)
  } else {
    if (is.null(embedding)) {
      stop("`embedding` must be provided when `cluster_on = 'embedding'`.")
    }
    if (!"author" %in% colnames(df_speakers)) {
      stop("The speakers table must contain an 'author' column when `cluster_on = 'embedding'`.")
    }
    emb_mat <- extract_embedding_matrix(embedding)
    emb_authors_lower <- tolower(rownames(emb_mat))
    speaker_authors_lower <- tolower(as.character(df_speakers$author))
    match_idx <- match(speaker_authors_lower, emb_authors_lower)

    if (any(is.na(match_idx))) {
      stop("Some speakers in `speaker_landscape` were not found in `embedding`.")
    }

    matched_vecs <- emb_mat[match_idx, , drop = FALSE]
    vec_norms <- sqrt(rowSums(matched_vecs^2))
    vec_norms[vec_norms == 0] <- 1
    norm_vecs <- matched_vecs / vec_norms

    # Cosine distance matrix (1 - cosine_similarity)
    sim_mat <- norm_vecs %*% t(norm_vecs)
    cos_dist <- stats::as.dist(pmax(0, 1 - sim_mat))
    hdb_res <- dbscan::hdbscan(cos_dist, minPts = minPts, ...)
  }

  df_speakers$cluster <- as.integer(hdb_res$cluster)
  df_speakers$cluster_label <- ifelse(
    df_speakers$cluster == 0,
    "Noise",
    paste0("Cluster ", df_speakers$cluster)
  )
  df_speakers$membership_prob <- if (!is.null(hdb_res$membership_prob)) {
    as.numeric(hdb_res$membership_prob)
  } else {
    rep(NA_real_, nrow(df_speakers))
  }
  df_speakers$outlier_score <- if (!is.null(hdb_res$outlier_scores)) {
    as.numeric(hdb_res$outlier_scores)
  } else {
    rep(NA_real_, nrow(df_speakers))
  }

  # Compute 2D cluster summary circles based on speakers' 2D locations (X1, X2)
  clustered_df <- df_speakers %>%
    dplyr::filter(cluster > 0)

  if (nrow(clustered_df) > 0) {
    clusters_summary <- clustered_df %>%
      dplyr::group_by(cluster, cluster_label) %>%
      dplyr::summarise(
        n_speakers = dplyr::n(),
        center_x = mean(X1, na.rm = TRUE),
        center_y = mean(X2, na.rm = TRUE),
        mean_dist = mean(sqrt((X1 - mean(X1, na.rm = TRUE))^2 + (X2 - mean(X2, na.rm = TRUE))^2), na.rm = TRUE),
        sd_dist = ifelse(
          dplyr::n() > 1,
          stats::sd(sqrt((X1 - mean(X1, na.rm = TRUE))^2 + (X2 - mean(X2, na.rm = TRUE))^2), na.rm = TRUE),
          0
        ),
        max_dist = max(sqrt((X1 - mean(X1, na.rm = TRUE))^2 + (X2 - mean(X2, na.rm = TRUE))^2), na.rm = TRUE),
        radius = as.numeric(
          stats::quantile(
            sqrt((X1 - mean(X1, na.rm = TRUE))^2 + (X2 - mean(X2, na.rm = TRUE))^2),
            probs = radius_quantile,
            na.rm = TRUE
          )
        ) * radius_padding,
        .groups = "drop"
      ) %>%
      dplyr::arrange(cluster)
  } else {
    clusters_summary <- tibble::tibble(
      cluster = integer(),
      cluster_label = character(),
      n_speakers = integer(),
      center_x = numeric(),
      center_y = numeric(),
      mean_dist = numeric(),
      sd_dist = numeric(),
      max_dist = numeric(),
      radius = numeric()
    )
  }

  n_clusters <- nrow(clusters_summary)
  n_noise <- sum(df_speakers$cluster == 0, na.rm = TRUE)
  message(sprintf(
    "HDBSCAN found %d cluster%s across %d speakers (%d noise points).",
    n_clusters,
    if (n_clusters == 1) "" else "s",
    nrow(df_speakers),
    n_noise
  ))

  if (is_landscape_list) {
    speaker_landscape$speakers <- df_speakers
    speaker_landscape$clusters <- clusters_summary
    speaker_landscape$hdbscan_model <- hdb_res
    return(speaker_landscape)
  } else {
    attr(df_speakers, "clusters") <- clusters_summary
    attr(df_speakers, "hdbscan_model") <- hdb_res
    return(df_speakers)
  }
}

#' Generate Polygon Coordinates for Cluster Circles on a 2D Landscape
#'
#' Internal helper to generate circle boundary coordinates `(x, y)` for each cluster
#' in `clusters_df` so they can be plotted cleanly with `ggplot2::geom_polygon` or `plotly`.
#'
#' @param clusters_df A tibble of cluster summaries as stored in `speaker_landscape$clusters`.
#' @param n_points Number of vertices per circle. Defaults to 100.
#' @return A tibble with columns `cluster`, `cluster_label`, `n_speakers`, `center_x`, `center_y`, `radius`, `x`, `y`.
#' @noRd
build_cluster_circle_polygons <- function(clusters_df, n_points = 100) {
  if (is.null(clusters_df) || nrow(clusters_df) == 0) {
    return(tibble::tibble(
      cluster = integer(),
      cluster_label = character(),
      n_speakers = integer(),
      center_x = numeric(),
      center_y = numeric(),
      radius = numeric(),
      x = numeric(),
      y = numeric()
    ))
  }

  angles <- seq(0, 2 * pi, length.out = n_points)
  poly_list <- lapply(seq_len(nrow(clusters_df)), function(i) {
    row <- clusters_df[i, ]
    tibble::tibble(
      cluster = row$cluster,
      cluster_label = row$cluster_label,
      n_speakers = row$n_speakers,
      center_x = row$center_x,
      center_y = row$center_y,
      radius = row$radius,
      x = row$center_x + row$radius * cos(angles),
      y = row$center_y + row$radius * sin(angles)
    )
  })

  dplyr::bind_rows(poly_list)
}

#' Generate Discrete Color Mapping for Clusters
#'
#' Internal helper to build a consistent named vector of colors for cluster labels,
#' ensuring the 'Noise' cluster is always colored gray, while non-noise clusters
#' receive distinct palette colors.
#'
#' @param cluster_labels Character or factor vector of cluster labels.
#' @param cluster_palette Optional custom vector of colors for non-noise clusters.
#' @param noise_color Color string used for the 'Noise' cluster. Defaults to \code{"#9ca3af"} (gray).
#' @return A named character vector of hex colors matching unique cluster labels.
#' @noRd
get_cluster_colors <- function(cluster_labels, cluster_palette = NULL, noise_color = "#9ca3af") {
  labs <- unique(as.character(cluster_labels))
  non_noise <- setdiff(labs, "Noise")

  # Order non-noise naturally (e.g. "Cluster 1", "Cluster 2", ..., "Cluster 10")
  numeric_part <- suppressWarnings(as.numeric(gsub("^.*?([0-9]+)$", "\\1", non_noise)))
  if (length(numeric_part) == length(non_noise) && !any(is.na(numeric_part))) {
    non_noise <- non_noise[order(numeric_part)]
  } else {
    non_noise <- sort(non_noise)
  }

  k <- length(non_noise)

  if (!is.null(cluster_palette)) {
    if (!is.null(names(cluster_palette)) && all(non_noise %in% names(cluster_palette))) {
      cols <- cluster_palette[non_noise]
    } else {
      cols <- rep_len(cluster_palette, max(1, k))[seq_len(k)]
      names(cols) <- non_noise
    }
  } else {
    default_pal <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#bcbd22", "#17becf", "#393b79",
      "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5"
    )
    if (k <= length(default_pal)) {
      cols <- default_pal[seq_len(k)]
    } else {
      cols <- grDevices::hcl(h = seq(15, 375, length.out = k + 1)[seq_len(k)], l = 65, c = 100)
    }
    names(cols) <- non_noise
  }

  if ("Noise" %in% labs) {
    all_cols <- c(cols, setNames(noise_color, "Noise"))
  } else {
    all_cols <- cols
  }

  return(all_cols)
}
