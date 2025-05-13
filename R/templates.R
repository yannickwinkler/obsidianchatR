#' Template configurations for Obsidian notes
#'
#' Predefined and user-defined template configurations for extracting
#' content from different Obsidian note templates
#'
#' @export
template_configs <- list(
  # Default configuration for my template
  zotero_annotated = list(
    citekey_pattern = "^citekey:\\s*\"?([^\"]+)\"?",
    abstract_start_pattern = "^>\\s*\\[!Abstract\\]",
    abstract_end_pattern = "^>\\s*\\[!quote\\]",
    commented_annotations_header = "## Commented Annotations",
    commented_annotations_footer = "## All Annotations",
    all_annotations_header = "## All Annotations",
    all_annotations_footer = "^%% Import Date:",
    note_separator_pattern = "\\s*\\*\\*Note:\\*\\*\\s*",
    annotation_block_marker = "^---$"
  )
)


#' Creates a custom template configuration
#'
#' @param citekey_pattern Regex pattern for the citekey
#' @param abstract_patterns List with start and end patterns for the abstract
#' @param annotation_headers List with header definitions for different annotation types
#' @param separator_pattern Pattern to separate annotation text and note
#' @param block_marker Pattern to mark annotation blocks
#'
#' @return A list with template configuration parameters
#' @export
create_template_config <- function(
  citekey_pattern = "^citekey:\\s*\"?([^\"]+)\"?",
  abstract_patterns = list(
    start = "^>\\s*\\[!Abstract\\]",
    end = "^>\\s*\\[!quote\\]"
  ),
  annotation_headers = list(
    commented_header = "## Commented Annotations",
    commented_footer = "## All Annotations",
    all_header = "## All Annotations", 
    all_footer = "^%% Import Date:|^$"
  ),
  separator_pattern = "\\s*\\*\\*Note:\\*\\*\\s*",
  block_marker = "^---$"
) {
  list(
    citekey_pattern = citekey_pattern,
    abstract_start_pattern = abstract_patterns$start,
    abstract_end_pattern = abstract_patterns$end,
    commented_annotations_header = annotation_headers$commented_header,
    commented_annotations_footer = annotation_headers$commented_footer,
    all_annotations_header = annotation_headers$all_header,
    all_annotations_footer = annotation_headers$all_footer,
    note_separator_pattern = separator_pattern,
    annotation_block_marker = block_marker
  )
}