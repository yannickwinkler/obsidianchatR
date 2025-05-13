#' Extract a specific section from an R Markdown file
#'
#' @param rmd_file_path Path to the .Rmd file
#' @param section_identifier Header text of the section to extract (e.g., "Introduction", "## Methods")
#' @return Character string with the section content, or empty string if not found
#' @export
extract_rmd_section <- function(rmd_file_path, section_identifier) {
  if (!file.exists(rmd_file_path)) {
    stop(paste("File not found:", rmd_file_path))
  }
  
  # Lese die Datei ein
  rmd_content <- readLines(rmd_file_path, warn = FALSE)
  
  # Finde den Start der Sektion
  section_start_pattern <- paste0("^", section_identifier, "$")
  start_idx <- which(stringr::str_detect(rmd_content, section_start_pattern))
  
  if (length(start_idx) == 0) {
    warning(paste("Section", section_identifier, "not found in", rmd_file_path))
    return("")
  }
  
  # Die erste Übereinstimmung nehmen
  start_idx <- start_idx[1]
  
  # Finde das Ende der Sektion (nächster Header oder Ende der Datei)
  # Bestimme das Level des Headers
  header_level <- length(stringr::str_extract_all(section_identifier, "^#+")[[1]])
  if (header_level == 0) header_level <- 1  # Falls kein '#' im Identifier
  
  # Suche nach dem nächsten Header auf gleicher oder höherer Ebene
  header_pattern <- paste0("^#{1,", header_level, "} ")
  end_candidates <- which(stringr::str_detect(rmd_content, header_pattern))
  end_candidates <- end_candidates[end_candidates > start_idx]
  
  end_idx <- if (length(end_candidates) > 0) {
    end_candidates[1] - 1
  } else {
    length(rmd_content)
  }
  
  # Extrahiere den Inhalt der Sektion (ohne den Header selbst)
  section_content <- rmd_content[(start_idx + 1):end_idx]
  
  # Entferne leere Zeilen am Anfang und Ende
  while (length(section_content) > 0 && trimws(section_content[1]) == "") {
    section_content <- section_content[-1]
  }
  
  while (length(section_content) > 0 && trimws(section_content[length(section_content)]) == "") {
    section_content <- section_content[-length(section_content)]
  }
  
  # Gebe den Inhalt als eine Zeichenkette zurück
  paste(section_content, collapse = "\n")
}