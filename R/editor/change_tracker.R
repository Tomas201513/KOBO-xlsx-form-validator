# Change Tracker for XLS-Validator
# Tracks pending edits to the XLSForm with comments and row operations

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
      comment = character(),
      timestamp = as.POSIXct(character())
    ),
    row_operations = tibble::tibble(
      id = integer(),
      sheet = character(),
      operation = character(),
      row = integer(),
      target_row = integer(),
      row_data = list(),
      timestamp = as.POSIXct(character())
    ),
    snapshots = list(),
    next_id = 1L,
    next_row_op_id = 1L,
    next_snapshot_id = 1L
  )
}

#' Add a change to the tracker
#' @param tracker Change tracker from create_change_tracker
#' @param sheet Sheet name
#' @param row Row number (Excel row)
#' @param column Column name
#' @param old_value Original value
#' @param new_value New value
#' @param comment Optional comment/reason for the change
#' @return Updated tracker
add_change <- function(tracker, sheet, row, column, old_value, new_value, comment = "") {
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
    # Update comment if provided
    if (nchar(comment) > 0) {
      tracker$changes$comment[existing_idx] <- as.character(comment)
    }
    
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
      comment = as.character(comment),
      timestamp = Sys.time()
    )
    tracker$changes <- dplyr::bind_rows(tracker$changes, new_change)
    tracker$next_id <- tracker$next_id + 1L
  }
  
  tracker
}

#' Update comment for an existing change
#' @param tracker Change tracker
#' @param change_id ID of the change to update
#' @param comment New comment text
#' @return Updated tracker
update_change_comment <- function(tracker, change_id, comment) {
  idx <- which(tracker$changes$id == change_id)
  if (length(idx) > 0) {
    tracker$changes$comment[idx] <- as.character(comment)
  }
  tracker
}

#' Add a row operation to the tracker
#' @param tracker Change tracker
#' @param sheet Sheet name
#' @param operation Operation type: "add", "delete", "duplicate", "move"
#' @param row Row number affected
#' @param target_row Target row (for move operations)
#' @param row_data Row data (for delete/duplicate, stores the row content)
#' @return Updated tracker
add_row_operation <- function(tracker, sheet, operation, row, target_row = NA_integer_, row_data = NULL) {
  new_op <- tibble::tibble(
    id = tracker$next_row_op_id,
    sheet = as.character(sheet),
    operation = as.character(operation),
    row = as.integer(row),
    target_row = as.integer(target_row),
    row_data = list(row_data),
    timestamp = Sys.time()
  )
  tracker$row_operations <- dplyr::bind_rows(tracker$row_operations, new_op)
  tracker$next_row_op_id <- tracker$next_row_op_id + 1L
  tracker
}

#' Get row operations for a sheet
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @return Tibble of row operations
get_row_operations <- function(tracker, sheet_name) {
  tracker$row_operations[tracker$row_operations$sheet == tolower(sheet_name), ]
}

#' Count row operations
#' @param tracker Change tracker
#' @return Integer count
count_row_operations <- function(tracker) {
  nrow(tracker$row_operations)
}

#' Remove a change from the tracker (revert)
#' @param tracker Change tracker
#' @param change_id ID of change to remove
#' @return Updated tracker
remove_change <- function(tracker, change_id) {
  tracker$changes <- tracker$changes[tracker$changes$id != change_id, ]
  tracker
}

#' Remove a row operation from the tracker
#' @param tracker Change tracker
#' @param op_id ID of row operation to remove
#' @return Updated tracker
remove_row_operation <- function(tracker, op_id) {
  tracker$row_operations <- tracker$row_operations[tracker$row_operations$id != op_id, ]
  tracker
}

#' Get number of pending changes
#' @param tracker Change tracker
#' @return Integer count
count_changes <- function(tracker) {
  nrow(tracker$changes)
}

#' Get total operations count (changes + row operations)
#' @param tracker Change tracker
#' @return Integer count
count_total_operations <- function(tracker) {
  nrow(tracker$changes) + nrow(tracker$row_operations)
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

#' Get change for a specific cell
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @param row Row number
#' @param column Column name
#' @return Single row tibble or NULL
get_cell_change <- function(tracker, sheet_name, row, column) {
  changes <- tracker$changes[
    tracker$changes$sheet == tolower(sheet_name) &
    tracker$changes$row == row &
    tracker$changes$column == column,
  ]
  if (nrow(changes) > 0) changes[1, ] else NULL
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

#' Get changed cells as a list for highlighting
#' @param tracker Change tracker
#' @param sheet_name Sheet name
#' @return List with row and column indices for changed cells
get_changed_cells <- function(tracker, sheet_name) {
  changes <- get_sheet_changes(tracker, sheet_name)
  if (nrow(changes) == 0) {
    return(list(rows = integer(), cols = character()))
  }
  list(
    rows = changes$row,
    cols = changes$column
  )
}

#' Apply all changes to XLSForm data
#' @param tracker Change tracker
#' @param xlsform_data List from read_xlsform
#' @return Updated xlsform_data with all changes applied
apply_changes <- function(tracker, xlsform_data) {
  if (nrow(tracker$changes) == 0 && nrow(tracker$row_operations) == 0) {
    return(xlsform_data)
  }
  
  # Apply row operations first (in order)
  if (nrow(tracker$row_operations) > 0) {
    ops <- tracker$row_operations[order(tracker$row_operations$timestamp), ]
    for (i in seq_len(nrow(ops))) {
      op <- ops[i, ]
      xlsform_data <- apply_row_operation(xlsform_data, op)
    }
  }
  
  # Then apply cell changes
  if (nrow(tracker$changes) > 0) {
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
  }
  
  xlsform_data
}

#' Apply a single row operation to XLSForm data
#' @param xlsform_data XLSForm data
#' @param op Row operation tibble row
#' @return Updated xlsform_data
apply_row_operation <- function(xlsform_data, op) {
  sheet_key <- tolower(op$sheet)
  
  if (!sheet_key %in% names(xlsform_data$sheets)) {
    return(xlsform_data)
  }
  
  df <- xlsform_data$sheets[[sheet_key]]
  df_row <- op$row - 1
  
  switch(op$operation,
    "add" = {
      # Insert empty row at position
      if (df_row < 1) df_row <- 1
      if (df_row > nrow(df)) {
        # Append at end
        new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
        names(new_row) <- names(df)
        new_row$.row_num <- nrow(df) + 2
        df <- dplyr::bind_rows(df, new_row)
      } else {
        # Insert at position
        new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
        names(new_row) <- names(df)
        if (df_row == 1) {
          df <- dplyr::bind_rows(new_row, df)
        } else {
          df <- dplyr::bind_rows(
            df[1:(df_row - 1), ],
            new_row,
            df[df_row:nrow(df), ]
          )
        }
      }
      # Recalculate row numbers
      df$.row_num <- seq_len(nrow(df)) + 1
    },
    "delete" = {
      if (df_row >= 1 && df_row <= nrow(df)) {
        df <- df[-df_row, ]
        if (nrow(df) > 0) {
          df$.row_num <- seq_len(nrow(df)) + 1
        }
      }
    },
    "duplicate" = {
      if (df_row >= 1 && df_row <= nrow(df)) {
        dup_row <- df[df_row, ]
        if (df_row == nrow(df)) {
          df <- dplyr::bind_rows(df, dup_row)
        } else {
          df <- dplyr::bind_rows(
            df[1:df_row, ],
            dup_row,
            df[(df_row + 1):nrow(df), ]
          )
        }
        df$.row_num <- seq_len(nrow(df)) + 1
      }
    },
    "move" = {
      target_df_row <- op$target_row - 1
      if (df_row >= 1 && df_row <= nrow(df) && 
          target_df_row >= 1 && target_df_row <= nrow(df)) {
        row_to_move <- df[df_row, ]
        df <- df[-df_row, ]
        if (target_df_row > df_row) {
          target_df_row <- target_df_row - 1
        }
        if (target_df_row <= 1) {
          df <- dplyr::bind_rows(row_to_move, df)
        } else if (target_df_row >= nrow(df)) {
          df <- dplyr::bind_rows(df, row_to_move)
        } else {
          df <- dplyr::bind_rows(
            df[1:(target_df_row - 1), ],
            row_to_move,
            df[target_df_row:nrow(df), ]
          )
        }
        df$.row_num <- seq_len(nrow(df)) + 1
      }
    }
  )
  
  xlsform_data$sheets[[sheet_key]] <- df
  xlsform_data$row_counts[[sheet_key]] <- nrow(df)
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

#' Get all changes formatted for display in cleaning log
#' @param tracker Change tracker
#' @return Tibble with formatted change history
get_change_history <- function(tracker) {
  if (nrow(tracker$changes) == 0) {
    return(tibble::tibble(
      id = integer(),
      sheet = character(),
      row = integer(),
      column = character(),
      change_summary = character(),
      comment = character(),
      timestamp = as.POSIXct(character())
    ))
  }
  
  tracker$changes |>
    dplyr::mutate(
      change_summary = sprintf('"%s" → "%s"', 
                               substr(old_value, 1, 20), 
                               substr(new_value, 1, 20))
    ) |>
    dplyr::select(id, sheet, row, column, change_summary, comment, timestamp) |>
    dplyr::arrange(dplyr::desc(timestamp))
}

#' Export changes to a cleaning log format
#' @param tracker Change tracker
#' @return Data frame suitable for export
export_cleaning_log <- function(tracker) {
  if (nrow(tracker$changes) == 0 && nrow(tracker$row_operations) == 0) {
    return(data.frame(
      Type = character(),
      Sheet = character(),
      Row = integer(),
      Column = character(),
      OldValue = character(),
      NewValue = character(),
      Comment = character(),
      Timestamp = character()
    ))
  }
  
  # Cell changes
  cell_log <- if (nrow(tracker$changes) > 0) {
    tracker$changes |>
      dplyr::mutate(
        Type = "Cell Edit"
      ) |>
      dplyr::select(
        Type,
        Sheet = sheet,
        Row = row,
        Column = column,
        OldValue = old_value,
        NewValue = new_value,
        Comment = comment,
        Timestamp = timestamp
      )
  } else {
    data.frame()
  }
  
  # Row operations
  row_log <- if (nrow(tracker$row_operations) > 0) {
    tracker$row_operations |>
      dplyr::mutate(
        Type = paste0("Row ", stringr::str_to_title(operation)),
        Column = NA_character_,
        OldValue = NA_character_,
        NewValue = ifelse(operation == "move", 
                          paste0("Moved to row ", target_row), 
                          NA_character_),
        Comment = ""
      ) |>
      dplyr::select(
        Type,
        Sheet = sheet,
        Row = row,
        Column,
        OldValue,
        NewValue,
        Comment,
        Timestamp = timestamp
      )
  } else {
    data.frame()
  }
  
  dplyr::bind_rows(cell_log, row_log) |>
    dplyr::arrange(Timestamp) |>
    dplyr::mutate(Timestamp = format(Timestamp, "%Y-%m-%d %H:%M:%S"))
}

# =============================================================================
# Snapshot / Version Control Functions
# =============================================================================

#' Create a snapshot of current changes
#' Snapshots allow saving and restoring change states
#' @param tracker Change tracker
#' @param description Description of this snapshot
#' @param xlsform_data Optional XLSForm data to compute hash
#' @return Updated tracker with new snapshot
create_snapshot <- function(tracker, description = "", xlsform_data = NULL) {
  snapshot_id <- tracker$next_snapshot_id
  
  # Compute data hash if xlsform_data provided
  data_hash <- NA_character_
  if (!is.null(xlsform_data) && requireNamespace("digest", quietly = TRUE)) {
    data_hash <- tryCatch({
      digest::digest(xlsform_data$sheets, algo = "md5")
    }, error = function(e) NA_character_)
  }
  
  snapshot <- list(
    id = snapshot_id,
    timestamp = Sys.time(),
    description = description,
    changes = tracker$changes,
    row_operations = tracker$row_operations,
    change_count = nrow(tracker$changes),
    row_op_count = nrow(tracker$row_operations),
    data_hash = data_hash
  )
  
  tracker$snapshots <- c(tracker$snapshots, list(snapshot))
  tracker$next_snapshot_id <- snapshot_id + 1L
  
  tracker
}

#' List all snapshots
#' @param tracker Change tracker
#' @return Data frame with snapshot info
list_snapshots <- function(tracker) {
  if (length(tracker$snapshots) == 0) {
    return(data.frame(
      id = integer(),
      timestamp = as.POSIXct(character()),
      description = character(),
      change_count = integer(),
      row_op_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    id = sapply(tracker$snapshots, function(s) s$id),
    timestamp = as.POSIXct(sapply(tracker$snapshots, function(s) s$timestamp), origin = "1970-01-01"),
    description = sapply(tracker$snapshots, function(s) s$description),
    change_count = sapply(tracker$snapshots, function(s) s$change_count),
    row_op_count = sapply(tracker$snapshots, function(s) s$row_op_count),
    stringsAsFactors = FALSE
  )
}

#' Get a specific snapshot
#' @param tracker Change tracker
#' @param snapshot_id ID of snapshot to retrieve
#' @return Snapshot list or NULL if not found
get_snapshot <- function(tracker, snapshot_id) {
  for (snapshot in tracker$snapshots) {
    if (snapshot$id == snapshot_id) {
      return(snapshot)
    }
  }
  NULL
}

#' Restore tracker to a snapshot state
#' This replaces current changes with the snapshot's changes
#' @param tracker Change tracker
#' @param snapshot_id ID of snapshot to restore
#' @param keep_snapshots If TRUE, keep snapshot history; if FALSE, clear it
#' @return Updated tracker at snapshot state, or original if snapshot not found
restore_snapshot <- function(tracker, snapshot_id, keep_snapshots = TRUE) {
  snapshot <- get_snapshot(tracker, snapshot_id)
  
  if (is.null(snapshot)) {
    warning(paste("Snapshot not found:", snapshot_id))
    return(tracker)
  }
  
  # Restore changes from snapshot
  tracker$changes <- snapshot$changes
  tracker$row_operations <- snapshot$row_operations
  
  # Update next IDs
  tracker$next_id <- if (nrow(tracker$changes) > 0) {
    max(tracker$changes$id) + 1L
  } else {
    1L
  }
  
  tracker$next_row_op_id <- if (nrow(tracker$row_operations) > 0) {
    max(tracker$row_operations$id) + 1L
  } else {
    1L
  }
  
  # Optionally clear snapshots after the restored one
  if (!keep_snapshots) {
    tracker$snapshots <- tracker$snapshots[sapply(tracker$snapshots, function(s) s$id <= snapshot_id)]
  }
  
  tracker
}

#' Delete a snapshot
#' @param tracker Change tracker
#' @param snapshot_id ID of snapshot to delete
#' @return Updated tracker
delete_snapshot <- function(tracker, snapshot_id) {
  tracker$snapshots <- Filter(
    function(s) s$id != snapshot_id,
    tracker$snapshots
  )
  tracker
}

#' Clear all snapshots
#' @param tracker Change tracker
#' @return Updated tracker with empty snapshots
clear_snapshots <- function(tracker) {
  tracker$snapshots <- list()
  tracker
}

#' Get the most recent snapshot
#' @param tracker Change tracker
#' @return Most recent snapshot or NULL if none exist
get_latest_snapshot <- function(tracker) {
  if (length(tracker$snapshots) == 0) {
    return(NULL)
  }
  tracker$snapshots[[length(tracker$snapshots)]]
}

#' Check if there are unsaved changes since last snapshot
#' @param tracker Change tracker
#' @return TRUE if there are changes since last snapshot
has_unsaved_changes <- function(tracker) {
  latest <- get_latest_snapshot(tracker)
  
  if (is.null(latest)) {
    # No snapshots, check if there are any changes
    return(nrow(tracker$changes) > 0 || nrow(tracker$row_operations) > 0)
  }
  
  # Compare current state to latest snapshot
  current_change_count <- nrow(tracker$changes)
  current_row_op_count <- nrow(tracker$row_operations)
  
  current_change_count != latest$change_count || 
    current_row_op_count != latest$row_op_count
}

#' Get diff between two snapshots
#' @param tracker Change tracker
#' @param from_id ID of earlier snapshot (or 0 for initial state)
#' @param to_id ID of later snapshot (or NULL for current state)
#' @return List with added_changes, removed_changes, added_row_ops, removed_row_ops
diff_snapshots <- function(tracker, from_id, to_id = NULL) {
  # Get from state
  if (from_id == 0) {
    from_changes <- tibble::tibble(
      id = integer(), sheet = character(), row = integer(),
      column = character(), old_value = character(), new_value = character(),
      comment = character(), timestamp = as.POSIXct(character())
    )
    from_row_ops <- tibble::tibble(
      id = integer(), sheet = character(), operation = character(),
      row = integer(), target_row = integer(), row_data = list(),
      timestamp = as.POSIXct(character())
    )
  } else {
    from_snapshot <- get_snapshot(tracker, from_id)
    if (is.null(from_snapshot)) {
      stop(paste("Snapshot not found:", from_id))
    }
    from_changes <- from_snapshot$changes
    from_row_ops <- from_snapshot$row_operations
  }
  
  # Get to state
  if (is.null(to_id)) {
    to_changes <- tracker$changes
    to_row_ops <- tracker$row_operations
  } else {
    to_snapshot <- get_snapshot(tracker, to_id)
    if (is.null(to_snapshot)) {
      stop(paste("Snapshot not found:", to_id))
    }
    to_changes <- to_snapshot$changes
    to_row_ops <- to_snapshot$row_operations
  }
  
  # Calculate diffs
  from_change_ids <- from_changes$id
  to_change_ids <- to_changes$id
  
  added_change_ids <- setdiff(to_change_ids, from_change_ids)
  removed_change_ids <- setdiff(from_change_ids, to_change_ids)
  
  from_row_op_ids <- from_row_ops$id
  to_row_op_ids <- to_row_ops$id
  
  added_row_op_ids <- setdiff(to_row_op_ids, from_row_op_ids)
  removed_row_op_ids <- setdiff(from_row_op_ids, to_row_op_ids)
  
  list(
    added_changes = to_changes[to_changes$id %in% added_change_ids, ],
    removed_changes = from_changes[from_changes$id %in% removed_change_ids, ],
    added_row_ops = to_row_ops[to_row_ops$id %in% added_row_op_ids, ],
    removed_row_ops = from_row_ops[from_row_ops$id %in% removed_row_op_ids, ]
  )
}

#' Auto-save snapshot if threshold reached
#' Creates automatic snapshots after a certain number of changes
#' @param tracker Change tracker
#' @param threshold Number of changes before auto-snapshot
#' @return Updated tracker (possibly with new snapshot)
auto_snapshot <- function(tracker, threshold = 10) {
  latest <- get_latest_snapshot(tracker)
  
  changes_since_snapshot <- if (is.null(latest)) {
    nrow(tracker$changes) + nrow(tracker$row_operations)
  } else {
    (nrow(tracker$changes) - latest$change_count) + 
      (nrow(tracker$row_operations) - latest$row_op_count)
  }
  
  if (changes_since_snapshot >= threshold) {
    tracker <- create_snapshot(
      tracker,
      description = sprintf("Auto-save after %d changes", changes_since_snapshot)
    )
  }
  
  tracker
}
