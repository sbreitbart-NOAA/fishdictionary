#' Use Roxygen formatting for a new entry into {FishDictionary}
#'
#' @param acronym Acronym
#' @param meaning Meaning of acronym
#' @param definition Definition of acronym
#' @param source Source of definition
#'
#' @returns A string of Roxygen formatted text for the new entry
#' @export
#'
#' @examples {
#' create_acronym_roxygen(
#'   acronym = "CH",
#'   meaning = "Critical Habitat",
#'   definition = "The specific areas within the geographical area occupied by the species.",
#'   source = "Endangered Species Act sec. 3(5) (16 USC 1532(5))"
#' )
#' }
create_acronym_roxygen <- function(acronym, meaning, definition, source) {
  # check if definition is longer than 80 characters
  if (nchar(definition) > 80) {
    # separate by 80 then paste #' before each line
    definition <- paste0(
      "#' ", strwrap(definition, width = 80, indent = 0),
      collapse = "\n"
    )
  } else {
    definition <- paste0("#'", definition)
  }
  # Vet acronym meaning for assignment at end
  vet_meaning <- gsub(" |-", "", meaning)
  # Create completed template string
  paste0(
    "#' ", meaning, " (", acronym, ")\n",
    "#'\n",
    definition, "\n",
    "#'\n",
    "#' @format\n",
    "#' \\describe{\n",
    "#' }\n",
    "#' @source ", source, "\n",
    vet_meaning, " <- NULL"#, "\n\n"
  )
}

#' Contribute to fish dictionary using csv sheet of new entries
#'
#' @param file csv file containing the acronym (column name = "Acronym"),
#' meaning (column name = "Meaning"), definition (column name = "Definition"),
#' and source (column name = "Source") of each term
#' @param dir file path where the new file is created. Default to the R folder
#' in the FishDictionary repository when working in a project
#' @param FileName A string describing the name of the output file created.
#'
#' @returns A .R file with Roxygen formatted entries for each term in the input csv file.
#' @export
#'
create_contrib_file <- function(
    file,
    dir = "./R",
    FileName = "asarAcronyms.R")
{
  acronym_data <- utils::read.csv(file)
  acronym_file <- c()
  for (i in 1:length(acronym_data$Acronym)) {
    # check if acronym is already in any file
    all_def_files <- list.files(dir, full.names = TRUE)
    if (any(
      unlist(lapply(all_def_files, function(f) {
        content <- readLines(f, warn = FALSE)
        any(grepl(paste0("\\(", acronym_data$Acronym[i], "\\)"), content))
      }))
    )) {
      message("Entry already exists. Skipping: ", acronym_data$Meaning[i])
      next
    }
    # Combine entries
    acronym_file <- paste0(
      acronym_file,
      create_acronym_roxygen(
        acronym = acronym_data$Acronym[i],
        meaning = acronym_data$Meaning[i],
        definition = acronym_data$Definition[i],
        source = acronym_data$Source[i]
      ),
      "\n\n"
    )
  }
  # Export entries
  utils::capture.output(
    cat(acronym_file),
    file = file.path(
      dir,
      ifelse(grepl(".R", FileName), FileName, paste0(FileName, ".R"))
      )
  )
}

# Contribute
# create_contrib_file(
#   file = "~/GitHub/fishdictionary/asar_acronyms_1.csv",
#   FileName = "asarAcronyms1"
# )
