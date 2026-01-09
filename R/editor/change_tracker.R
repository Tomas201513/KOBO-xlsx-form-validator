# Change Tracker for XLS-Validator
# Tracks pending edits to the XLSForm

#' Create a new change tracker
#' @return Empty change tracker list
create_change_tracker <- function() {
  list(
    changes = tibble::tibble(
      id = integer(),
      sheet = character(),
      row = integer(),
      column = character(),
      old_value = character(),
      new_value = character(),
      timestamp = as.POSIXct(character())
    ),
    next_id = 1L
  )
}

#' Add a change to the tracker
#' @param tracker Change tracker from create_change_tracker
#' @param sheet Sheet name
#' @param row Row number (Excel row)
#' @param column Column name
#' @param old_value Original value
#' @param new_value New value
#' @return Updated tracker
add_change <- function(tracker, sheet, row, column, old_value, new_value) {
  # Don't track if values are the same
  if (identical(as.character(old_value), as.character(new_value))) {
    return(tracker)
  }
  
  # Check if there's already a change for this cell
  existing_idx <- which(
    tracker$changes$sheet == sheet &
    tracker$changes$row == row &
    tracker$changes$column == column
  )
  
  if (length(existing_idx) > 0) {
    # Update existing change
    tracker$changes$new_value[existing_idx] <- as.character(new_value)
    tracker$changes$timestamp[existing_idx] <- Sys.time()
    
    # If new value equals original, remove the change
    if (identical(
      as.character(tracker$changes$old_value[existing_idx]),
      as.character(new_value)
    )) {
      tracker$changes <- tracker$changes[-existing_idx, ]
    }
  } else {
    # Add new change
    new_change <- tibble::tibble(
      id = tracker$next_id,
      sheet = as.character(sheet),
      row = as.integer(row),
      column = as.character(column),
      old_value = as.character(old_value),
      new_value = as.character(new_value),
      timestamp = Sys.time()
    )
    tracker$changes <- dplyr::bind_rows(tracker$changes, new_change)
    tracker$next_id <- tracker$next_id + 1L
  }
  
  tracker
}

#' Remove a change from the tracker (revert)
#' @param tracker Change tracker
#' @param change_id ID of change to remove
#' @return Updated tracker
remove_change <- function(tracker, change_id) {
  tracker$changes <- tracker$changes[tracker$changes$id != change_id, ]
  tracker
}

#' Get number of pending changes
#' @param tracker Change tracker
#' @return Integer count
count_changes <- function(tracker) {
  nrow(tracker$changes)
}

#' Get all pending changes
#' @param tracker Change tracker
#' @return Tibble of changes
get_changes <- function(tracker) {
  tracker$changes
}

#' Get changes for a specific sheet
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @return Tibble of changes for that sheet
get_sheet_changes <- function(tracker, sheet_name) {
  tracker$changes[tracker$changes$sheet == tolower(sheet_name), ]
}

#' Get changes for a specific row
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @param row Row number
#' @return Tibble of changes for that row
get_row_changes <- function(tracker, sheet_name, row) {
  tracker$changes[
    tracker$changes$sheet == tolower(sheet_name) &
    tracker$changes$row == row,
  ]
}

#' Check if a specific cell has pending changes
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @param row Row number
#' @param column Column name
#' @return Logical
has_change <- function(tracker, sheet_name, row, column) {
  any(
    tracker$changes$sheet == tolower(sheet_name) &
    tracker$changes$row == row &
    tracker$changes$column == column
  )
}

#' Apply all changes to XLSForm data
#' @param tracker Change tracker
#' @param xlsform_data List from read_xlsform
#' @return Updated xlsform_data with all changes applied
apply_changes <- function(tracker, xlsform_data) {
  if (nrow(tracker$changes) == 0) {
    return(xlsform_data)
  }
  
  for (i in seq_len(nrow(tracker$changes))) {
    change <- tracker$changes[i, ]
    xlsform_data <- set_cell_value(
      xlsform_data,
      sheet_name = change$sheet,
      row = change$row,
      column = change$column,
      value = change$new_value
    )
  }
  
  xlsform_data
}

#' Clear all changes
#' @param tracker Change tracker
#' @return Empty tracker
clear_changes <- function(tracker) {
  create_change_tracker()
}

#' Get summary of changes by sheet
#' @param tracker Change tracker
#' @return Named vector of change counts per sheet
summarize_changes <- function(tracker) {
  if (nrow(tracker$changes) == 0) {
    return(c())
  }
  table(tracker$changes$sheet)
}

#' Get rows with changes (for highlighting in UI)
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @return Integer vector of row numbers with changes
get_changed_rows <- function(tracker, sheet_name) {
  changes <- get_sheet_changes(tracker, sheet_name)
  unique(changes$row)
}


