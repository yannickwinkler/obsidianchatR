#' Validate API Keys
#' @param model_type Type of model ("claude", "openai", "gemini")
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_api_keys <- function(model_type) {
  key_requirements <- list(
    claude = "ANTHROPIC_API_KEY",
    openai = "OPENAI_API_KEY",
    gemini = "GOOGLE_API_KEY"
  )

  required_key <- key_requirements[[model_type]]
  if (is.null(Sys.getenv(required_key)) || Sys.getenv(required_key) == "") {
    stop(sprintf(
      "API Key missing: %s not found in .Renviron\n
      Please add your API key using:\n
      usethis::edit_r_environ()",,
      required_key
    ))
  }
  return(required_key)
}

#' Initialize LLM Chat
#' @param model_type Type of model ("claude", "openai", "gemini")
#' @param system_prompt System context
#' @param api_args Additional API arguments
#' @return Chat object
#' @export
initialize_chat <- function(model = "gpt-4-0125-preview",
                          system_prompt = NULL,
                          api_args = list()) {

  # Default API Argumente
  #default_args <- list(temperature = 0)
  # Overwrite with user-provided args
  #api_args <- modifyList(default_args, api_args)


  # Bestimme Model-Typ basierend auf Model-Namen
  model_type <- if (grepl("claude", model, ignore.case = TRUE)) {
    "claude"
  } else if (grepl("gpt|o[1-9]|openai", model, ignore.case = TRUE)) {
    "openai"
  } else if (grepl("gemini", model, ignore.case = TRUE)) {
    "gemini"
  } else {
    stop("Unknown model. Please choose a claude/openai/gemini model.")
  }

  # API Key validieren
  validate_api_keys(model_type)

  # Chat-Objekt erstellen
  chat <- switch(model_type,
    claude = ellmer::chat_claude(
      system_prompt = system_prompt,
      model = model,
      api_args = api_args
    ),
    openai = ellmer::chat_openai(
      model = model,
      api_args = api_args
    ),
    gemini = ellmer::chat_gemini(
      system_prompt = system_prompt,
      model = model,
      api_args = api_args
    )
  )

  return(chat)
}


#' Create a Contextual LLM Prompt from Obsidian Notes and/or RMD Manuscript
#'
#' @param user_query The user's question or instruction for the LLM.
#' @param obsidian_notes Extracted Obsidian notes as tibble from get_all_obsidian_notes_content.
#'        If NULL, no Obsidian content will be included.
#' @param rmd_content Pre-extracted content from RMD file. If NULL, no RMD content is included.
#' @param obsidian_explainer Text to explain the Obsidian notes context.
#' @param rmd_explainer Text to explain the RMD content.
#' @param overall_context_header Optional text to prepend to the entire prompt.
#' @param include_only_commented Include only commented annotations if TRUE.
#' @return A formatted prompt string for LLM.
#' @export
create_contextual_prompt <- function(user_query,
                                     # Direct passing of obs and rmd data
                                     obsidian_notes = NULL,
                                     rmd_content = NULL,
                                     # Explanatory text
                                     obsidian_explainer = "The following JSON data contains structured literature notes. Each entry represents an annotation and includes the original 'paper_text' (the annotated text from the source) and a 'note' (personal comment on the annotation). The 'note' field may be empty if no personal comment was added to that specific annotation. Please review these annotations for key points.",
                                     rmd_explainer = "The following text is a section from the R Markdown manuscript that may require attention:",
                                     overall_context_header = NULL,
                                     # Select notes to be included
                                     include_only_commented = FALSE) {
  
  prompt_parts <- c()
  
  # Overall context (optional)
  if (!is.null(overall_context_header) && nzchar(overall_context_header)) {
    prompt_parts <- c(prompt_parts, trimws(overall_context_header), "\n")
  }
  
  # --- Obsidian Notes Workflow ---
  if (!is.null(obsidian_notes)) {
    # Use directly passed notes
    if (nrow(obsidian_notes) > 0) {
      # Format notes to JSON with adjusted separator pattern from metadata
      # Determine the most frequently used template for note_separator_pattern
      template_counts <- table(obsidian_notes$template_used)
      main_template <- names(template_counts)[which.max(template_counts)]
      
      note_separator_pattern <- if (main_template %in% names(template_configs)) {
        template_configs[[main_template]]$note_separator_pattern
      } else {
        "\\s*\\*\\*Note:\\*\\*\\s*"  # Default pattern as fallback
      }
      
      json_notes <- format_lit_notes_to_json(
        lit_notes = obsidian_notes,
        note_separator_pattern = note_separator_pattern,
        include_only_commented = include_only_commented
      )
      
      if (nzchar(json_notes)) {
        obsidian_context <- paste0(
          obsidian_explainer, "\n\n",
          "```json\n",
          json_notes,
          "\n```"
        )
        prompt_parts <- c(prompt_parts, obsidian_context, "\n--- END OBSIDIAN NOTES CONTEXT ---\n")
      }
    } else {
      # No Obsidian notes provided or found
      prompt_parts <- c(prompt_parts, "[No relevant Obsidian notes provided or found.]\n")
    }
  }
  
  # --- RMD Manuscript Workflow ---
  if (!is.null(rmd_content) && nzchar(trimws(rmd_content))) {
    # Use direct RMD content
    rmd_context <- paste0(
      rmd_explainer, "\n\n",
      "```markdown\n",
      trimws(rmd_content),
      "\n```"
    )
    
    prompt_parts <- c(prompt_parts,
                      rmd_context, 
                      "--- END RMD MANUSCRIPT SECTION CONTEXT ---\n")
  } else {
    # No RMD content provided
    prompt_parts <- c(prompt_parts, "[No RMD manuscript content provided.]\n")
  }
  
  # --- Add the User Query ---
  prompt_parts <- c(prompt_parts, paste0("User Query: ", user_query))
  
  # Combine all parts and clean up empty lines
  final_prompt <- paste(prompt_parts, collapse = "\n\n")
  final_prompt <- gsub("\n{3,}", "\n\n", final_prompt) # Clean up multiple newlines
  final_prompt <- gsub("(\n\n)+$", "\n", final_prompt) # Ensure single newline at the end if any
  
  return(final_prompt)
}