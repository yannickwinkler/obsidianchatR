% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obisidian.R
\name{find_obsidian_notes}
\alias{find_obsidian_notes}
\title{Find Obsidian Notes}
\usage{
find_obsidian_notes(
  base_folder = "~/Documents/obsidian",
  subfolder = "master vault/1. Source Notes",
  patterns = NULL,
  use_wildcard = FALSE
)
}
\arguments{
\item{base_folder}{Base directory of Obsidian vault}

\item{subfolder}{Subfolder within vault to search}

\item{patterns}{Search patterns to match filenames}

\item{use_wildcard}{Whether to treat patterns as wildcards}
}
\value{
Character vector of matching file paths
}
\description{
Find Obsidian Notes
}
