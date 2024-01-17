#' Read forms with checkbox fields
#'
#' @param redcap_uri
#' @param token
#' @param records
#' @param events
#' @param fields
#' @param forms
#' @param batch_size integer, not in use for now, currently only use one_shot
#' @param blank_for_gray_form_status boolean, use `NA` for gray form status
#' @param blank_for_untouched_checkbox boolean, use `NA` for untouched checkbox
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
redcap_read_checkbox <- function(redcap_uri,
                                 token,
                                 records = NULL,
                                 events = NULL,
                                 fields = NULL,
                                 forms = NULL,
                                 batch_size = 100L,
                                 blank_for_gray_form_status = TRUE,
                                 blank_for_untouched_checkbox = TRUE,
                                 ...) {
  #   __________________________________________________________________________
  #   load data                                                             ####

  #   ..........................................................................
  #   metadata                                                              ####

  meta <- REDCapR:::redcap_metadata_internal(
    redcap_uri,
    token
  )

  ds_metadata <- meta$d_variable

  # only include selected fields/forms
  if (!is.null(fields) & !is.null(forms)) {
    ds_metadata <- ds_metadata |>
      dplyr::filter(
        field_name_base %in% fields | form_name %in% forms
      )
  } else if (!is.null(fields)) {
    ds_metadata <- ds_metadata |>
      dplyr::filter(
        field_name_base %in% fields
      )
  } else if (!is.null(forms)) {
    ds_metadata <- ds_metadata |>
      dplyr::filter(
        form_name %in% forms
      )
  }


  #   ..........................................................................
  #   col types                                                             ####

  col_types <- REDCapR::redcap_metadata_coltypes(
    redcap_uri,
    token,
    print_col_types_to_console = FALSE
  )

  # only include selected fields/forms
  col_types[[1]] <- col_types[[1]][ds_metadata$field_name]


  #   ..........................................................................
  #   data                                                                  ####

  # TODO: add support for batch_size
    ds_eav <- REDCapR:::redcap_read_eav_oneshot(
      redcap_uri,
      token,
      records = records,
      events  = events,
      fields  = fields,
      forms   = forms
    )$data


  #   __________________________________________________________________________
  #   transform data                                                        ####

  ##  ..........................................................................
  ##  possible values                                                       ####

  .repeating    <- "redcap_repeat_instrument" %in% names(ds_eav)
  .longitudinal <- meta$longitudinal

  .fields_plumbing <- "record"

  if (.longitudinal) {
    .fields_plumbing <- c(
      .fields_plumbing,
      "redcap_event_name"
    )

    # get longitudinal instrument metadata
    instruments_events <- REDCapR::redcap_event_instruments(
      redcap_uri,
      token
    )$data |>
      dplyr::select(
        -arm_num
      ) |>
      dplyr::rename(
        redcap_event_name  = unique_event_name,
        form_name          = form
      )

    # only include selected forms/events
    instruments_events <- instruments_events |>
      dplyr::filter(
        form_name %in% ds_metadata$form_name
      )
    if (!is.null(events)) {
      instruments_events <- instruments_events |>
        dplyr::filter(
          redcap_event_name %in% events
        )
    }
  }

  if (.repeating) {
    .fields_plumbing <- c(
      .fields_plumbing,
      "redcap_repeat_instrument",
      "redcap_repeat_instance"
    )

    # get repeating instrument metadata
    instruments_repeating <- redcap_repeating(
      redcap_uri,
      token
    )$data |>
      dplyr::select(
        -custom_form_label
      )

    # only include selected forms/events
    instruments_repeating <- instruments_repeating |>
      dplyr::filter(
        form_name %in% ds_metadata$form_name
      )
    if (!is.null(events)) {
      instruments_repeating <- instruments_repeating |>
        dplyr::filter(
          event_name %in% events
        )
    }
  }

  # create dataframe of possible values
  if (.longitudinal & .repeating) {
    .fields_to_cross <- ds_metadata |>
      dplyr::filter(
        !plumbing
      ) |>
      dplyr::select(
        field_name,
        form_name
      ) |>
      dplyr::left_join(
        instruments_events,
        by       = "form_name",
        multiple = "all",
        relationship = "many-to-many"
      )

    .fields_to_cross_repeating <- .fields_to_cross |>
      dplyr::right_join(
        instruments_repeating,
        by = c(
          "form_name",
          "redcap_event_name" = "event_name"
        )
      )

    .fields_to_cross_not_repeating <- .fields_to_cross |>
      dplyr::anti_join(
        instruments_repeating,
        by = c(
          "form_name",
          "redcap_event_name" = "event_name"
        )
      ) |>
      dplyr::select(
        -form_name
      )

    ds_eav_expand <- ds_eav |>
      tidyr::expand(
        tidyr::nesting(
          record,
          redcap_event_name,
          redcap_repeat_instrument,
          redcap_repeat_instance
        )
      )

    ds_eav_possible <- dplyr::bind_rows(
      # not repeating
      ds_eav_expand |>
        dplyr::filter(
          is.na(redcap_repeat_instrument)
        ) |>
        dplyr::left_join(
          .fields_to_cross_not_repeating,
          by = c(
            "redcap_event_name"
          ),
          multiple = "all",
          relationship = "many-to-many"
        ) |>
        # eliminate empty events (i.e., rows without field names).
        # this happens if an event only contains repeating forms as eav exports
        # always include a record ID field for each event
        dplyr::filter(
          !is.na(field_name)
        ),
      # repeating
      ds_eav_expand |>
        dplyr::filter(
          !is.na(redcap_repeat_instrument)
        ) |>
        dplyr::left_join(
          .fields_to_cross_repeating,
          by = c(
            "redcap_event_name",
            "redcap_repeat_instrument" = "form_name"
          ),
          multiple = "all",
          relationship = "many-to-many"
        )
    )
  } else if (.repeating) {
    .fields_to_cross <- ds_metadata |>
      dplyr::filter(
        !plumbing
      ) |>
      dplyr::select(
        field_name,
        form_name
      )

    .fields_to_cross_repeating <- .fields_to_cross |>
      dplyr::filter(
        form_name %in% instruments_repeating$form_name
      )

    .fields_to_cross_not_repeating <- .fields_to_cross |>
      dplyr::filter(
        !form_name %in% instruments_repeating$form_name
      ) |>
      dplyr::select(
        -form_name
      )

    ds_eav_expand <- ds_eav |>
      tidyr::expand(
        tidyr::nesting(
          record,
          redcap_repeat_instrument,
          redcap_repeat_instance
        )
      )

    ds_eav_possible <- dplyr::bind_rows(
      # not repeating
      ds_eav_expand |>
        dplyr::filter(
          is.na(redcap_repeat_instrument)
        ) |>
        tidyr::crossing(
          .fields_to_cross_not_repeating
        ),
      # repeating
      ds_eav_expand |>
        dplyr::filter(
          !is.na(redcap_repeat_instrument)
        ) |>
        dplyr::left_join(
          .fields_to_cross_repeating,
          by = c(
            "redcap_repeat_instrument" = "form_name"
          ),
          multiple = "all",
          relationship = "many-to-many"
        )
    )
  } else if (.longitudinal) {
    .fields_to_cross <- ds_metadata |>
      dplyr::filter(
        !plumbing
      ) |>
      dplyr::select(
        field_name,
        form_name
      ) |>
      dplyr::left_join(
        instruments_events,
        by       = "form_name",
        multiple = "all",
        relationship = "many-to-many"
      ) |>
      dplyr::select(
        -form_name
      )

    ds_eav_possible <- ds_eav |>
      tidyr::expand(
        tidyr::nesting(
          record,
          redcap_event_name
        )
      ) |>
      dplyr::left_join(
        .fields_to_cross,
        by = c(
          "redcap_event_name"
        ),
        multiple = "all",
        relationship = "many-to-many"
      )
  } else {
    .fields_to_cross <- ds_metadata |>
      dplyr::filter(
        !plumbing
      ) |>
      dplyr::select(
        field_name,
      )

    ds_eav_possible <- ds_eav |>
      tidyr::expand(
        tidyr::nesting(
          record
        ),
        tidyr::crossing(
          .fields_to_cross
        )
      )
  }


  ##  ..........................................................................
  ##  transform data                                                        ####

  ds_eav_2 <- ds_eav |>
    # include field type column
    dplyr::rename(field_name_base = field_name) |>
    dplyr::left_join(
      ds_metadata |>
        dplyr::distinct(.data$field_name_base, .data$field_type),
      by = "field_name_base"
    ) |>

    # convert values from eav format to standard format
    dplyr::mutate(
      checkbox   = !is.na(.data$field_type) & (.data$field_type == "checkbox"),
      field_name = dplyr::if_else(
        .data$checkbox,
        paste0(.data$field_name_base , "___", .data$value),
        .data$field_name_base
      ),
      value      = dplyr::if_else(
        .data$checkbox,
        "1",
        .data$value
      )
    ) |>

    # get rid of helper fields
    dplyr::right_join(ds_eav_possible, by = c(.fields_plumbing, "field_name")) |>
    dplyr::select(-"field_type", -"field_name_base", -"checkbox") |>

    # join new helper fields
    dplyr::left_join(
      ds_metadata |>
        dplyr::select("field_name", "field_name_base", "field_type"),
      by = "field_name"
    )

  # coalesce values for NAs depending on user's choice how to handle untouched
  # checkbox fields
  .complete_value_for_untouched_forms <- dplyr::if_else(
    blank_for_gray_form_status,
    NA_character_,
    as.character(REDCapR::constant("form_incomplete"))
  )

  if (blank_for_untouched_checkbox) {
    ds_eav_2_coalesce <- ds_eav_2 |>
      dplyr::mutate(
        touched = dplyr::if_else(
          !is.na(value),
          1,
          0
        )
      ) |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              .fields_plumbing,
              "field_name_base"
            )
          )
        )
      ) |>
      dplyr::mutate(
        any_touched = sum(touched) > 0
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        value = dplyr::case_when(
          .data$field_type == "checkbox" & .data$any_touched ~
            dplyr::coalesce(value, "0"),
          .data$field_type == "complete" ~
            dplyr::coalesce(value, .complete_value_for_untouched_forms),
          TRUE ~ value
        ),
      ) |>
      dplyr::select(
        -c(
          "field_type",
          "field_name_base",
          "touched",
          "any_touched"
        )
      )
  } else {
    ds_eav_2_coalesce <- ds_eav_2 |>
      dplyr::mutate(
        value = dplyr::case_when(
          .data$field_type == "checkbox" ~
            dplyr::coalesce(value, "0"),
          .data$field_type == "complete" ~
            dplyr::coalesce(value, .complete_value_for_untouched_forms),
          TRUE ~ value
        )
      ) |>
      dplyr::select(
        -c(
          "field_type",
          "field_name_base"
        )
      )
  }


  ##  ..........................................................................
  ##  reshape data                                                          ####

  # if ID field is in ds_metadata, add all plumbing fields to returned fields
  # if not, do not include any plumbing fields (while not particularly useful,
  # it resembles standard behavior which allows to download data without the
  # subject ID)
  # TODO: REDCapR is about to change this behavior and always include the ID
  #       and plumbing vars: https://ouhscbbmc.github.io/REDCapR/news/index.html#upcoming-changes-in-v120
  #       We should do that as well
  .fields_to_return <- ds_metadata |>
    dplyr::filter(
      !plumbing
    ) |>
    dplyr::pull(
      field_name
    )

  .record_id_name <- meta$d_variable$field_name[1]

  if(.record_id_name %in% ds_metadata$field_name) {
    .fields_to_return <- c(
      .fields_plumbing,
      .fields_to_return
    )

    .fields_to_return[1] <- .record_id_name
  }

  # pivot data into standard format
  ds <- ds_eav_2_coalesce |>
    tidyr::pivot_wider(
      id_cols     = !!.fields_plumbing,
      names_from  = "field_name",
      values_from = "value"
    ) |>
    dplyr::rename(
      {{ .record_id_name }} := record
    ) |>
    dplyr::select(
      dplyr::all_of(.fields_to_return)
    )

  ds
}


#' @title
#' Export repeating forms and events
#'
#' @description
#' This function calls the 'repeatingFormsEvents' function of the
#' REDCap API.
#'
#' @param redcap_uri The
#' [uri](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier)/url
#' of the REDCap server
#' typically formatted as "https://server.org/apps/redcap/api/".
#' Required.
#' @param token The user-specific string that serves as the password for a
#' project.  Required.
#' @param verbose A boolean value indicating if `message`s should be printed
#' to the R console during the operation.  The verbose output might contain
#' sensitive information (*e.g.* PHI), so turn this off if the output might
#' be visible somewhere public. Optional.
#' @param config_options A list of options passed to [httr::POST()].
#' See details at [httr::httr_options()]. Optional.
#' @param handle_httr The value passed to the `handle` parameter of
#' [httr::POST()].
#' This is useful for only unconventional authentication approaches.  It
#' should be `NULL` for most institutions.  Optional.
#'
#' @return
#' Currently, a list is returned with the following elements,
#' * `data`: A [tibble::tibble()] where each row represents one repeating form
#' and event combination
#' * `success`: A boolean value indicating if the operation was apparently
#' successful.
#' * `status_code`: The
#' [http status code](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes)
#' of the operation.
#' * `outcome_message`: A human readable string indicating the operation's
#' outcome.
#' * `elapsed_seconds`: The duration of the function.
#' * `raw_text`: If an operation is NOT successful, the text returned by
#' REDCap.  If an operation is successful, the `raw_text` is returned as an
#' empty string to save RAM.
#'
#' @author
#' Will Beasley
#'
#' @references
#' The official documentation can be found on the 'API Help Page'
#' and 'API Examples' pages on the REDCap wiki (*i.e.*,
#' https://community.projectredcap.org/articles/456/api-documentation.html and
#' https://community.projectredcap.org/articles/462/api-examples.html).
#' If you do not have an account for the wiki, please ask your campus REDCap
#' administrator to send you the static material.
#'
#' @examples
#' \dontrun{
#' uri         <- "https://bbmc.ouhsc.edu/redcap/api/"
#' token       <- "9A81268476645C4E5F03428B8AC3AA7B"
#' ds_variable <- REDCapR::redcap_variables(redcap_uri=uri, token=token)$data
#' }

#' @export
redcap_repeating <- function(
    redcap_uri,
    token,
    verbose           = TRUE,
    config_options    = NULL,
    handle_httr       = NULL
) {

  checkmate::assert_character(redcap_uri, any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_character(token     , any.missing=FALSE, len=1, pattern="^.{1,}$")

  token   <- REDCapR:::sanitize_token(token)
  verbose <- REDCapR:::verbose_prepare(verbose)

  post_body <- list(
    token     = token,
    content   = "repeatingFormsEvents",
    format    = "csv"
  )

  # This is the important call that communicates with the REDCap server.
  kernel <-
    REDCapR:::kernel_api(
      redcap_uri      = redcap_uri,
      post_body       = post_body,
      config_options  = config_options,
      handle_httr     = handle_httr
    )

  if (!kernel$success) {
    if (is.null(kernel$raw_text)) {
      # nocov start
      rlang::abort(
        message = "REDCapR::redcap_variables() encountered an error communicating with the server."
      )
      # nocov end
    } else {
      # nocov start
      rlang::abort(
        message = kernel$raw_text
      )
      # nocov end
    }
  } else {
    try(
      {
        # Convert the raw text to a dataset.
        ds <-
          readr::read_csv(
            file            = I(kernel$raw_text),
            show_col_types  = FALSE
          )
      },
      silent = TRUE
      # Don't print the warning in the try block.  Print it below, where
      #    it's under the control of the caller.
    )

    if (exists("ds") && inherits(ds, "data.frame")) {
      outcome_message <- sprintf(
        "%s variable metadata records were read from REDCap in %0.1f seconds.  The http status code was %i.",
        format(nrow(ds), big.mark = ",", scientific = FALSE, trim = TRUE),
        kernel$elapsed_seconds,
        kernel$status_code
      )

      kernel$raw_text   <- ""
      # If an operation is successful, the `raw_text` is no longer returned
      #   to save RAM.  The content is not really necessary with httr's status
      #   message exposed.
    } else {
      # nocov start
      kernel$success  <- FALSE # Override the 'success' http status code.
      ds              <- tibble::tibble() # Return an empty data.frame

      outcome_message <- sprintf(
        "The REDCap variable retrieval failed.  The http status code was %i.  The 'raw_text' returned was '%s'.",
        kernel$status_code,
        kernel$raw_text
      )
      # nocov end
    }
  }
  # } else {
  #   ds              <- tibble::tibble() # Return an empty data.frame
  #   outcome_message <-
  #     if (any(grepl(kernel$regex_empty, kernel$raw_text))) {
  #       "The REDCapR read/export operation was not successful.  The returned dataset (of variables) was empty." # nocov
  #     } else {
  #       sprintf(
  #         "The REDCapR variable retrieval was not successful.  The error message was:\n%s",
  #         kernel$raw_text
  #       )
  #     }
  # }

  if (verbose)
    message(outcome_message)

  list(
    data                = ds,
    success             = kernel$success,
    status_code         = kernel$status_code,
    outcome_message     = outcome_message,
    elapsed_seconds     = kernel$elapsed_seconds,
    raw_text            = kernel$raw_text
  )
}
