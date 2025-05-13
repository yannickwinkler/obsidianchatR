#' Find Obsidian Notes
#' 
#' @param base_folder Base directory of Obsidian vault
#' @param subfolder Subfolder within vault to search
#' @param patterns Search patterns to match filenames
#' @param use_wildcard Whether to treat patterns as wildcards
#' @return Character vector of matching file paths
#' @export
find_obsidian_notes <- function(base_folder = "~/Documents/obsidian",
                                subfolder = "master vault/1. Source Notes",
                                patterns = NULL,
                                use_wildcard = FALSE) {
  if (!dir.exists(file.path(base_folder, subfolder))) {
    stop("Specified directory does not exist")
  }
  
  full_path <- file.path(base_folder, subfolder)
  notes <- list.files(full_path, pattern = "\\.md$", full.names = TRUE, recursive = TRUE)
  
  if (!is.null(patterns) && length(patterns) > 0) {
    if (use_wildcard) {
      wildcard_patterns <- sapply(patterns, glob2rx)
      combined_pattern <- paste(wildcard_patterns, collapse = "|")
    } else {
      combined_pattern <- paste(patterns, collapse = "|")
    }
    notes <- notes[grep(combined_pattern, notes)]
  }
  
  if (length(notes) == 0) {
    warning("No matching notes found.")
    return(NULL)
  }
  
  return(notes)
}

#' Process Annotation Blocks from Obsidian Notes
#' 
#' @param file_content Character vector of file content
#' @param start_label Label marking start of annotation block
#' @param end_label Label marking end of annotation block
#' @param content_filter Optional regex filter for content
#' @param block_marker Pattern that marks the separation between annotation blocks
#' @return Character vector of processed annotations
#' @export
process_annotation_blocks <- function(file_content, 
                                     start_label, 
                                     end_label, 
                                     content_filter = NULL,
                                     block_marker = "^---$") {
  # If start_label or end_label is NULL, return an empty result
  if (is.null(start_label) || is.null(end_label)) {
    return(character(0))
  }
  
  start <- which(stringr::str_detect(file_content, start_label))
  end <- which(stringr::str_detect(file_content, end_label))
  
  if (length(start) > 0 && length(end) > 0) {
    # Find the first end that comes after the start
    end <- end[end > start[1]]
    if (length(end) == 0) {
      end <- length(file_content) + 1  #  If no end is found, take the end of the file
    } else {
      end <- end[1]  # First end after the start
    }
    
    if (start[1] < end) {
      content_section <- file_content[(start[1] + 1):(end - 1)]
      
       # Split the blocks based on the block marker
      blocks <- split(content_section, cumsum(stringr::str_detect(content_section, block_marker)))
      
      annotations <- purrr::map(blocks, function(block) {
        # Remove markers and empty lines
        block <- block[!stringr::str_detect(block, block_marker)]
        block <- block[nzchar(trimws(block))]
        
        if (length(block) > 0) {
          # Join with consideration for multiline content
          paste(block, collapse = " ")
        } else {
          NA_character_
        }
      }) %>%
        unlist() %>%
        purrr::discard(is.na) %>%
        # Clean up: Remove formatting
        stringr::str_remove_all("^.*\\s{2}>\\s") %>%
        stringr::str_remove_all("^>\\s*") %>%
        # Apply filter if present
        (\(x) if(!is.null(content_filter)) x[stringr::str_detect(x, content_filter)] else x)()
      
      return(annotations)
    }
  }
  
  return(character(0))
}

#' Clean text content
#' 
#' @param text Character vector to clean
#' @return Cleaned text
#' @keywords internal
clean_text_content <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return(character(0))
  }
  
  # Remove html-tags and extra spaces
  text %>%
    stringr::str_remove_all("<[^>]+>") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    trimws()
}

#' Extract Obsidian Notes Content
#' 
#' @param file_path Path to markdown file
#' @param template Template configuration to use. Can be either a string naming a 
#'        predefined template from `template_configs` or a list with custom configuration.
#' @param process_blocks_params A list of additional arguments to be passed directly to the internal
#'        \code{\link{process_annotation_blocks}} function (via \code{\link{extract_obsidian_notes}}).
#'        This is primarily useful for providing a \code{content_filter} regex pattern
#'        (e.g., \code{list(content_filter = "your_regex_pattern")}) to filter annotations
#'        based on their content during processing for all found notes.
#' @return List containing citekey, abstract, and annotations
#' @export
extract_obsidian_notes <- function(file_path,
                                   template = "zotero_annotated",
                                   process_blocks_params = list()) {
  if (!file.exists(file_path)) {
    stop("File does not exist")
  }
  
  # Get template configuration
  config <- if (is.character(template) && template %in% names(template_configs)) {
    template_configs[[template]]
  } else if (is.list(template)) {
    template
  } else {
    stop("Template must be either a name of a predefined template or a configuration list.")
  }
  
  file_content <- readLines(file_path, warn = FALSE)
  
  # Extract citekey using the configured pattern
  citekey_pattern <- config$citekey_pattern
  citekey_line <- grep(citekey_pattern, file_content, value = TRUE)[1]
  citekey <- if (!is.na(citekey_line) && length(citekey_line) > 0) {
    # Extract the actual citekey with regex capture
    matches <- regexec(citekey_pattern, citekey_line)
    if (matches[[1]][1] != -1) { # Check if group 2 exists
      regmatches(citekey_line, matches)[[1]][2]
    } else { # Fallback if capture group is not present or match is different
      sub(paste0(sub("^\\^", "", citekey_pattern), "\\s*"), "", citekey_line)
    }
  } else {
    NA_character_
  }
  
  # Extract abstract if patterns are defined
  abstract <- NA_character_
  if (!is.null(config$abstract_start_pattern) && !is.null(config$abstract_end_pattern)) {
    abstract_start_idx <- which(stringr::str_detect(file_content, config$abstract_start_pattern))
    abstract_end_idx <- which(stringr::str_detect(file_content, config$abstract_end_pattern))
    
    if (length(abstract_start_idx) > 0 && length(abstract_end_idx) > 0) {
      first_start <- abstract_start_idx[1]
      possible_ends <- abstract_end_idx[abstract_end_idx > first_start]
      if (length(possible_ends) > 0) {
        first_end <- possible_ends[1]
        if (first_start < first_end && (first_start + 1) <= (first_end - 1)) {
          abstract <- file_content[(first_start + 1):(first_end - 1)] %>%
            stringr::str_remove_all(">") %>%
            paste(collapse = " ") %>%
            stringr::str_trim()
        }
      }
    }
  }
  
  # Process annotations using the process_annotation_blocks function
  all_annots_args <- c(
    list(
      file_content = file_content,
      start_label = config$all_annotations_header,
      end_label = config$all_annotations_footer,
      block_marker = config$annotation_block_marker
    ),
    process_blocks_params
  )
  
  annotation_df <- do.call(process_annotation_blocks, all_annots_args) %>%
    clean_text_content()
  
  commented_annots_args <- c(
    list(
      file_content = file_content,
      start_label = config$commented_annotations_header,
      end_label = config$commented_annotations_footer,
      block_marker = config$annotation_block_marker
    ),
    process_blocks_params
  )
  
  commented_df <- do.call(process_annotation_blocks, commented_annots_args) %>%
    clean_text_content()
  
  # Return all extracted information
  list(
    citekey = citekey,
    abstract = abstract,
    all_annotations = annotation_df,
    commented_annotations = commented_df,
    template_used = if(is.character(template)) template else "custom"
  )
}

#' Find and Extract Content from Multiple Obsidian Notes
#'
#' This function finds Obsidian notes based on specified patterns and then
#' extracts structured content (citekey, abstract, annotations) from each note.
#'
#' @param base_folder Base directory of Obsidian vault.
#' @param subfolder Subfolder within vault to search.
#' @param patterns Search patterns to match filenames.
#' @param use_wildcard Whether to treat patterns as wildcards.
#' @param template Template configuration to use. Can be either a string naming a 
#'        predefined template from `template_configs` or a list with custom configuration.
#' @param process_blocks_params A list of additional arguments to be passed directly to the internal
#'        \code{\link{process_annotation_blocks}} function (via \code{\link{extract_obsidian_notes}}).
#'        This is primarily useful for providing a \code{content_filter} regex pattern
#'        (e.g., \code{list(content_filter = "your_regex_pattern")}) to filter annotations
#'        based on their content during processing for all found notes.
#' @return A tibble where each row corresponds to a note. Columns include
#'         \code{file_path}, \code{citekey}, \code{abstract}, \code{all_annotations},
#'         \code{commented_annotations}, and \code{template_used}.
#' @export
get_all_obsidian_notes_content <- function(base_folder = "~/Documents/obsidian",
                                         subfolder = "master vault/1. Source Notes",
                                         patterns = NULL,
                                         use_wildcard = FALSE,
                                         template = "zotero_annotated",
                                         process_blocks_params = list()) {

  note_files <- find_obsidian_notes(
    base_folder = base_folder,
    subfolder = subfolder,
    patterns = patterns,
    use_wildcard = use_wildcard
  )

  if (is.null(note_files) || length(note_files) == 0) {
    return(dplyr::tibble(
      file_path = character(),
      citekey = character(),
      abstract = character(),
      all_annotations = list(),
      commented_annotations = list(),
      template_used = character()
    ))
  }

  extracted_data <- purrr::map(note_files, function(file) {
    tryCatch({
      # Use the specified template
      used_template <- template
      
      # Extract with the selected template
      content <- extract_obsidian_notes(
        file_path = file,
        template = used_template,
        process_blocks_params = process_blocks_params
      )
      
      # Return results as a tibble
      dplyr::tibble(
        file_path = file,
        citekey = ifelse(is.null(content$citekey), NA_character_, content$citekey),
        abstract = ifelse(is.null(content$abstract), NA_character_, content$abstract),
        all_annotations = list(if (is.null(content$all_annotations)) NA_character_ else content$all_annotations),
        commented_annotations = list(if (is.null(content$commented_annotations)) NA_character_ else content$commented_annotations),
        template_used = content$template_used
      )
    }, error = function(e) {
      warning(sprintf("Error processing file %s: %s", file, e$message))
      dplyr::tibble(
        file_path = file,
        citekey = NA_character_,
        abstract = NA_character_,
        all_annotations = list(NA_character_),
        commented_annotations = list(NA_character_),
        template_used = NA_character_
      )
    })
  })

  dplyr::bind_rows(extracted_data)
}

#' Format Literature Notes to JSON
#' @param lit_notes List of literature notes, or a tibble/data.frame as returned by \code{\link{get_all_obsidian_notes_content}}.
#' @param note_separator_pattern Regex pattern to separate annotation text from its note.
#' @param include_only_commented Logical, if TRUE, only commented annotations are included. Defaults to FALSE.
#' @return JSON formatted string
#' @export
format_lit_notes_to_json <- function(lit_notes, 
                                   note_separator_pattern = "\\s*\\*\\*Note:\\*\\*\\s*",
                                   include_only_commented = FALSE) {
  
  notes_list_processed <- list()

  if (is.data.frame(lit_notes)) {
    if (nrow(lit_notes) == 0) return(jsonlite::toJSON(list(), pretty = TRUE, auto_unbox = TRUE))
    notes_list_processed <- purrr::pmap(lit_notes, function(...) {
      current_row <- list(...)
      all_annots <- if (!is.null(current_row$all_annotations)) unlist(current_row$all_annotations) else character(0)
      all_annots <- all_annots[!is.na(all_annots) & all_annots != ""]
      
      commented_annots <- if (!is.null(current_row$commented_annotations)) unlist(current_row$commented_annotations) else character(0)
      commented_annots <- commented_annots[!is.na(commented_annots) & commented_annots != ""]
      
      list(
        citekey = current_row$citekey,
        # Store them separately to decide later based on include_only_commented
        .all_annotations_internal = all_annots,
        .commented_annotations_internal = commented_annots
      )
    })
  } else if (is.list(lit_notes)) {
    if (length(lit_notes) == 0) return(jsonlite::toJSON(list(), pretty = TRUE, auto_unbox = TRUE))
    temp_notes_list <- if (!is.null(lit_notes$citekey) && !is.list(lit_notes[[1]])) {
       list(lit_notes)
    } else {
       lit_notes
    }
    # Standardize to the internal format used by data.frame path
    notes_list_processed <- purrr::map(temp_notes_list, function(note) {
      list(
        citekey = note$citekey,
        .all_annotations_internal = if (!is.null(note$all_annotations) && is.character(note$all_annotations)) note$all_annotations else character(0),
        .commented_annotations_internal = if (!is.null(note$commented_annotations) && is.character(note$commented_annotations)) note$commented_annotations else character(0)
      )
    })

  } else {
    return(jsonlite::toJSON(list(), pretty = TRUE, auto_unbox = TRUE))
  }

  notes_list_processed %>%
    purrr::map(function(note) {
      
      annotations_to_process <- if (include_only_commented) {
        note$.commented_annotations_internal
      } else {
        # Combine and unique, as commented might also be in all
        unique(c(note$.all_annotations_internal, note$.commented_annotations_internal))
      }
      annotations_to_process <- annotations_to_process[!is.na(annotations_to_process) & annotations_to_process != ""]


      processed_annotations_json <- purrr::map(annotations_to_process, function(annotation_text) {
        parts <- strsplit(annotation_text, note_separator_pattern)[[1]]
        list(
          paper_text = if(length(parts) > 0) trimws(parts[1]) else "",
          note = if(length(parts) > 1) trimws(parts[2]) else ""
        )
      })

      list(
        citekey = if (!is.null(note$citekey)) note$citekey else NA_character_,
        annotations = processed_annotations_json
      )
    }) %>%
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
}