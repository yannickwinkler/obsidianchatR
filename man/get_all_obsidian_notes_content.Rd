% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obisidian.R
\name{get_all_obsidian_notes_content}
\alias{get_all_obsidian_notes_content}
\title{Find and Extract Content from Multiple Obsidian Notes}
\usage{
get_all_obsidian_notes_content(
  base_folder = "~/Documents/obsidian",
  subfolder = "master vault/1. Source Notes",
  patterns = NULL,
  use_wildcard = FALSE,
  template = "zotero_annotated",
  process_blocks_params = list()
)
}
\arguments{
\item{base_folder}{Base directory of Obsidian vault.}

\item{subfolder}{Subfolder within vault to search.}

\item{patterns}{Search patterns to match filenames.}

\item{use_wildcard}{Whether to treat patterns as wildcards.}

\item{template}{Template configuration to use. Can be either a string naming a
predefined template from \code{template_configs} or a list with custom configuration.}

\item{process_blocks_params}{A list of additional arguments to be passed directly to the internal
\code{\link{process_annotation_blocks}} function (via \code{\link{extract_obsidian_notes}}).
This is primarily useful for providing a \code{content_filter} regex pattern
(e.g., \code{list(content_filter = "your_regex_pattern")}) to filter annotations
based on their content during processing for all found notes.}
}
\value{
A tibble where each row corresponds to a note. Columns include
\code{file_path}, \code{citekey}, \code{abstract}, \code{all_annotations},
\code{commented_annotations}, and \code{template_used}.
}
\description{
This function finds Obsidian notes based on specified patterns and then
extracts structured content (citekey, abstract, annotations) from each note.
}
