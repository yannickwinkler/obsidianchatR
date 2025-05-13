# obsidianchatR: Chat with your Obsidian Notes

`obsidianchatR` is an R package primarily developed for my personal workflow to extract content from [Obsidian](https://obsidian.md/) literature notes (imported from [Zotero](https://www.zotero.org)) and format it for use with Large Language Models (LLMs). It allows combining insights from structured notes with sections of a manuscript for quickly revising or generating academic texts.

This package is shared to inspire others or for anyone wishing to experiment with similar workflows. Please note that it's tailored to a specific setup and might require adjustments for your own needs. For more general PDF data extraction, you might also want to explore tools like [docling](https://github.com/docling-project/docling), although I haven’t used it myself.

## Installation

You can install the development version of `obsidianchatR` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("yannickwinkler/obsidianchatR")
```

## Core Workflow

This package is designed to integrate your Obsidian notes and R Markdown manuscript sections into a cohesive prompt for an LLM.

### Scenario 1: Using the Default Zotero-based Template

If your Obsidian notes are structured similarly to my template that this package was initially designed for you can use the predefined `zotero_annotated` template. My template looks like this:

```markdown
---
category: literature-note
"DOI:": "{{DOI}}"
ISBN: "{{ISBN}}"
authors: "{{authors}}"
year: '{{date | format("YYYY")}}'
itemType: "{{itemType}}"
publisher: "{{publisher}}"
tags: {{allTags}}
citekey: "{{citekey}}"
status: unread 
dateread: 
rating:
---
# {{title}}

> [!Cite] 
> {{bibliography}}

> [!Abstract]
> {%- if abstractNote %}
> {{abstractNote}}
> {%- endif -%}.
> 

> [!quote] Quotable
> .

{{pdfZoteroLink}}
## Notes
---
{% persist "notes" %}{% endpersist %}


## Commented Annotations
---

{% for annotation in annotations-%} 
{%- if annotation.comment -%} 
{%- if annotation.annotatedText -%}
<span style="background-color:{{annotation.colorCategory}}">…</span> **{{annotation.colorCategory}} {{annotation.type}} | Page [{{annotation.page}}](zotero://open-pdf/library/items/{{annotation.attachment.itemKey}}?page={{annotation.page}}&annotation={{annotation.id}})** 

> {{annotation.annotatedText | escape }}

**Note:** {{annotation.comment}} 

---

{% endif -%}
{%- if annotation.imageRelativePath -%} 
📊 **Image Annotation**
> ![[{{annotation.imageRelativePath}}]]

**Note:** {{annotation.comment}}

---

{% endif -%} 
{%- endif -%}
{%- endfor -%}


## All Annotations

{% for annotation in annotations-%} 
{%- if annotation.annotatedText -%}
<span style="background-color:{{annotation.colorCategory}}">…</span> **{{annotation.colorCategory}} {{annotation.type}} | Page [{{annotation.page}}](zotero://open-pdf/library/items/{{annotation.attachment.itemKey}}?page={{annotation.page}}&annotation={{annotation.id}})** 

> {{annotation.annotatedText | escape }}

---

{% endif -%}
{%- endfor -%}

```

I copied most of the template from somewhere, but unfortunately, I no longer have the original. Give me a shout if you know the source, and I will credit it here.

This template works with the [Obsidian Zotero Integration](https://github.com/mgmeyers/obsidian-zotero-integration) plugin, which lets you import annotated PDFs from Zotero into Obsidian.

**My Workflow Example:**

1.  **Set up API Keys:**
    Ensure your API keys for the desired provider (Claude, OpenAI, Gemini) are set in your `.Renviron` file. You can edit this file using:
    ```r
    usethis::edit_r_environ()
    ```
    Add lines like:
    ```
    OPENAI_API_KEY="your_openai_key_here"
    ANTHROPIC_API_KEY="your_anthropic_key_here"
    GOOGLE_API_KEY="your_google_key_here"
    ```
    Restart your R session after saving the `.Renviron` file.

2.  **Extract Content and Create Prompt:**
    ```r
    library(obsidianchatR)

    # 1. Extract content from specific Obsidian notes using the default template
    #    Here, we're looking for notes related to "barbati_2025" and "bormann_2022"
    #    Adjust base_folder and subfolder to match your Obsidian vault structure
    all_notes_data <- get_all_obsidian_notes_content(
      base_folder = "~/Documents/ObsidianVault", # Your Obsidian vault path
      subfolder = "ReferenceNotes",             # Subfolder containing literature notes
      template = "zotero_annotated",
      patterns = c("author_2025", "author2_2022") # Name patterns of your note files
    )

    # 2. Extract a specific section from your R Markdown manuscript
    rmd_section <- extract_rmd_section(
      rmd_file_path = "path/to/your/manuscript.Rmd",
      section_identifier = "# Introduction" # The header of the section you want
    )

    # 3. Create a contextual prompt for the LLM
    #  This combines your user query, the extracted notes, and the RMD section
    prompt <- create_contextual_prompt(
      user_query = "Summarise the texts and improve the script.",
      obsidian_notes = all_notes_data,
      rmd_content = rmd_section,
      overall_context_header = "This is a current project regarding academic topic x."
    )

    # Print the generated prompt to see what will be sent to the LLM
    cat(contextual)

    # 4. Initialize a chat session and send the prompt
    chat <- initialize_chat()
    chat$chat(prompt)
    ```

### Scenario 2: Using Your Own Custom Template

If your Obsidian notes use a different structure, you can define a custom template configuration.

**Methods to Define a Custom Template:**

1.  **Using `create_template_config()`**
    You can create a configuration list on the fly and pass it to the extraction functions.
    ```r
    library(obsidianchatR)

    my_custom_config <- create_template_config(
      citekey_pattern = "^MyCitekeyField:\\s*(\\S+)", # Regex for your citekey
      abstract_patterns = list(
        start = "### Abstract Section",        # Text marking the start of your abstract
        end = "### End of Abstract"            # Text marking the end of your abstract
      ),
      annotation_headers = list(
        commented_header = "#### My Personal Notes", # Header for your commented annotations
        commented_footer = "#### All Raw Annotations", # Text after commented annotations
        all_header = "#### All Raw Annotations",   # Header for all annotations
        all_footer = "---END---"                  # Text marking the end of all annotations
      ),
      separator_pattern = "\\s*MY_NOTE_SEPARATOR\\s*", # Regex to separate annotation from your note
      block_marker = "^\\*\\*\\*$"                     # Regex for the line separating annotation blocks
    )

    # Use this custom configuration
    all_notes_data_custom <- get_all_obsidian_notes_content(
      template = my_custom_config, # Pass the custom config here
      patterns = c("your_note_pattern")
      # ... other arguments ...
    )

    # ... then proceed with extract_rmd_section and create_contextual_prompt as above ...
    ```

2.  **Adding to `template_configs` (For persistent changes or if modifying package code):**
    You can directly add a new named list to the `template_configs` object within the package (e.g., in `R/templates.R`) or add it to the loaded list in your current R session:
    ```r
    # In your R script, for the current session:
    obsidianchatR:::template_configs$my_very_own_template <- list(
      citekey_pattern = "...", # your patterns here
      abstract_start_pattern = "...",
      # ... and so on for all fields as in create_template_config
      note_separator_pattern = "...",
      annotation_block_marker = "..."
    )

    all_notes_data_persistent <- get_all_obsidian_notes_content(
      template = "my_very_own_template", # Use the name you assigned
      patterns = c("your_note_pattern")
      # ... other arguments ...
    )
    ```

**Key Customization Points for Your Template:**

When creating your own template configuration, pay attention to these parameters:

*   `citekey_pattern`: A regex to find and extract the citation key.
*   `abstract_start_pattern`, `abstract_end_pattern`: Regex patterns to identify the beginning and end of the abstract section.
*   `commented_annotations_header`, `commented_annotations_footer`: Patterns to identify the section of your personally commented annotations.
*   `all_annotations_header`, `all_annotations_footer`: Patterns to identify the section containing all annotations (which might include those without personal comments).
*   `note_separator_pattern`: This is **important**. It's a regex used by `format_lit_notes_to_json()` to split the raw text of an annotation from your personal note/comment on that annotation. For example, if your notes look like `> Annotated text from PDF **My Note:** This is my thought.`, your `note_separator_pattern` might be `"\\s*\\*\\*My Note:\\*\\*\\s*"`.
*   `annotation_block_marker`: A regex for the pattern that separates individual annotation blocks (e.g., `^---$`).

**Caution for Custom Templates:**

*   The function `process_annotation_blocks()` includes some default text cleaning logic, such as removing leading `>` characters common in Markdown blockquotes (e.g., `stringr::str_remove_all("^>\\s*")`).
*   If your template formats annotated text very differently (e.g., not using blockquotes), you might need to adjust the cleaning steps within `process_annotation_blocks()` or ensure your `process_blocks_params` with a `content_filter` handles it. This is a more advanced customization and might require modifying the package's internal code if the existing cleaning is too aggressive or insufficient for your template.

## Contributing

Contributions are welcome! Please feel free to submit a pull request or open an issue if you find bugs or have suggestions for improvements. However, please note that this package is primarily a personal project, and there may already be better solutions available. Thus, I may not actively maintain it or respond to all inquiries.