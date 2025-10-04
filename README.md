# Speaker Landscape Analysis: Vector Space Mapping and Visualization

This R packge ouline the pipeline for analyzing speaker similarities based on their language, as outlined in *Schuld, M., Durrheim, K., & Mafunda, M. (2024). Speaker landscapes: Machine learning opens a window on the everyday language of opinion. Communication Methods and Measures, 18(4), 315-331.*

The procedure transforms raw text into a high-dimensional vector space using Word2Vec and subsequently reduces the data to a 2D "landscape" using UMAP for visual interpretation.

## 1. Methodology Overview

The core goal of this procedure is to map the linguistic space of different speakers/authors alongside key topical tokens. This allows for a visualization of proximity, where speakers who use similar language or discuss similar topics are placed closer together.

The pipeline consists of four main phases: **Data Preprocessing**, **Advanced Tokenization**, **Embedding Training**, and **Dimensionality Reduction & Visualization**.

## 2. Data Preparation and Speaker Encoding

Before training, the raw text data is structured to ensure that the machine learning model can learn a vector representation for each speaker.

### Speaker Encoding (Using `prepare_data`)

The initial text file (or character vector) is assumed to contain a speaker ID followed by their quote. We explicitly encode the speaker's identity directly into the text stream:

1. **Data Loading:** The input data is parsed, separating the `author` ID (first token) from the rest of the text (`quote`).

2. **Speaker Token Creation:** A dedicated speaker token is created (e.g., `speaker_userA`) for each unique speaker ID.

3. **Corpus Preparation:** The speaker token is prepended to every line of text, creating an input format like: `speaker_userA the quick brown fox`. This ensures that when the Word2Vec model is trained, it treats the`speaker_userA` as a valid token and learns a vector embedding for it based on the words that follow it. A speaker vector is thus created based on the language used by the speaker, and speakers are positioned in a vectorial space accroding to the semantic the refer to. 

### Advanced Tokenization (N-gram Substitution)

Before training, the text undergoes a critical step to identify and replace high-frequency multi-word expressions (N-grams) that function as single semantic units (e.g., "new york" or "machine learning").

The `create_bigrams` function:

1. Identifies the top $N$ most frequent bigrams.

2. Filters out bigrams containing common stopwords.

3. Replaces the original space (e.g., "new york") with an underscore (e.g., "**new\_york**").

## 3. Embedding Training (Word2Vec)

The prepared, encoded, and tokenized corpus is used to train a **Skip-Gram Word2Vec model** (via the `train_embedding_model` function).

The Skip-Gram model is configured to:

* Learn a vector for every token in the vocabulary, including words (like "vaccine") and speaker IDs (like "speaker\_userA").

* Use a large **context window** (e.g., 10) to capture long-range contextual relationships.

* Filter out low-frequency words using a **minimum** count **threshold** (e.g., 5) to ensure robust vector quality.

The result is a high-dimensional embedding matrix where the distance between any two vectors (word/word, word/speaker, or speaker/speaker) represents their semantic or contextual similarity.

## 4. Dimensionality Reduction (UMAP)

To visualize and interpret the high-dimensional space, the `speaker_landscapes` function applies **Uniform Manifold Approximation and Projection (UMAP)**.

### UMAP Projection

1. The embedding vectors for all filtered speakers (those exceeding the `retain_threshold`) are extracted, along with the vectors for a curated set of `tokens_to_plot`.

2. UMAP is run on this combined set of vectors using **cosine similarity** as the distance metric. This ensures that the two-dimensional layout reflects the true angular distance between the original high-dimensional vectors.

3. The coordinates are returned as separate data frames for speakers and words.

## 5. Visualization (`plot_speaker_landscapes`)

The final plot is generated to display the resulting **speaker landscape** in an interpretable format:

* **Speaker Distribution:** Speakers are plotted as low-opacity points. The cluster density and position reveal groups of speakers with similar linguistic patterns.

* **Thematic Labeling:** The selected `tokens_to_plot` (key words) are plotted as text labels on the same canvas. The position of these word labels indicates the thematic focus associated with nearby speaker clusters.

* **Categorical Grouping (Optional):** If a separate `categories` data frame is provided, speakers are colored by their defined category, aiding in the interpretation of how pre-defined groups map onto the learned linguistic space.

## 6. Detailed Example and Walkthrough

To see a complete, runnable demonstration of the full speaker landscape pipeline, including data preparation, Word2Vec training, UMAP reduction, and visualization, please view our detailed walkthrough notebook.


| Notebook View (Rendered HTML) | Source Code (`.Rmd`) |
| :---------------------------- | :------------------- |
| **[View Full Walkthrough]** | **[View Source Code]** |

[View Full Walkthrough]: [https://htmlpreview.github.io/?https://github.com/davidemorselli/speakerlandscapes/vignette_example.html](https://htmlpreview.github.io/?https://github.com/davidemorselli/speakerlandscapes/vignette_example.html)
[View Source Code]: [https://github.com/davidemorselli/speakerlandscapes/vignette_example.Rmd](https://github.com/davidemorselli/speakerlandscapes/vignette_example.Rmd)
