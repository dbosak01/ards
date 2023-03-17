

# Globals -----------------------------------------------------------------



ardsenv <- new.env()

ardsenv$ards <- NULL
ardsenv$template <- NULL



# Functions ---------------------------------------------------------------



#' @title Initialize the Analysis Results Dataset
#' @description A function to initialize the Analysis Results Dataset (ARDS).
#' This function will
#' first create a data template in the desired structure, and then
#' populates common values across the dataset.  These common values will be
#' repeated on each row of the analysis data frame for subsequent inserts.
#' Use the "reset" parameter to clear out any existing data and start a new
#' analysis dataset.
#' @param studyid The study for which the analysis was performed. Optional string value.
#' @param tableid A table identifier to use for the results. Optional string
#' value.
#' @param adsns A vector of source dataset names.
#' @param population A description of the analysis population.
#' @param time A description of the time frame used in the analysis.
#' @param where A description of the criteria used to subset the data for analysis.
#' @param reset If true, clear out existing ARDS dataset and replace with
#' empty template.  Otherwise, just assign parameters to template.  The default
#' value is TRUE.
#' @return The initialized analysis dataset.
#' @export
init_ards <- function(studyid = NULL,
                      tableid = NULL, adsns = NULL,
                      population = NULL, time = NULL, where = NULL, reset = TRUE) {


  if (reset) {
    if (!is.null(ardsenv$ards)) {
      ardsenv$ards <- NULL
    }
  }

  # Create template record
  ardsenv$template <- data.frame(studyid = studyid, resultid = 0,
                                 tableid = tableid,
                                 adsns = paste(adsns, sep ="", collapse = "|"),
                                 population = population, time = time,
                                 where = where, byvar1 = NA, byvar2 = NA,
                                 byvar3 = NA, byvar4 = NA, byvar5 = NA,
                                 byvar6 = NA, byvar7 = NA, byvar8 = NA,
                                 byvar9 = NA, byval1 = NA, byval2 = NA,
                                 byval3 = NA, byval4 = NA, byval5 = NA,
                                 byval6 = NA, byval7 = NA, byval8 = NA,
                                 byval9 = NA, trtvar = NA, trtval = NA,
                                 paramcd = NA, anal_var = NA, anal_val = NA,
                                 statname = NA, statval = NA, statdesc = NA)

  if (reset) {
    # Assign empty template to ards
    ardsenv$ards <- ardsenv$template[0, ]
  }

  return(ardsenv$template)

}



#' @title Adds data to an Analysis Results Dataset
#' @description The \code{add_ards} function dumps data from an input dataset
#' to the ARDS dataset.  The function is designed to be pipe-friendly, and will
#' return the input dataset unaltered.  The parameters on the function
#' define how to extract the desired data from the input dataset.
#' The "statvars" parameter defines which column names contain desired
#' analysis data.  The values in these columns will be used to populate the
#' "statval" variable in the output dataset.  Other parameters are used to
#' define identifying information for the statistics value, and are optional.
#' @param data The input dataset to create analysis results for.
#' @param statvars  A vector of column names that identify the desired results.
#' Statvar columns must be numeric.  This parameter is required.
#' @param statdesc A vector of column names that identify a description value
#' for each statvar.
#' @param byvars A vector of column names to use for by variables.
#' @param trtvar A column name to use for the treatment variable.
#' @param paramcd A description of the analysis parameter.
#' @param anal_var A column name for the analysis variable.
#' @param anal_val The analysis variable value.  Can be identified by a column
#' name or a vector of values.
#' @return The input data frame, unaltered.
#' @export
add_ards <- function(data, statvars, statdesc = NULL,
                     byvars = NULL, trtvar = NULL, paramcd = NULL,
                     anal_var = NULL, anal_val = NULL) {

  #browser()

  nms <- names(data)

  for (i in seq_len(length(statvars))) {

    # Start with template and turn into list
    ret <- unclass(ardsenv$template)

    # Populate statistics value
    ret$statval <- data[[statvars[[i]]]]

    # Populate statistic name from variable name
    ret$statname <- statvars[[i]]

    # Determine starting point of ResultID
    if (nrow(ardsenv$ards) == 0) {
      strt <- 1
    } else {
      strt <- max(ardsenv$ards$resultid) + 1
    }

    # Populate ResultID
    ret$resultid <- seq(strt, strt + nrow(data) - 1)

    # Populate Stat Description
    if (!is.null(statdesc)) {
      if (all(statdesc %in% nms)) {
        if (length(statdesc) == 1) {

          ret$statdesc <- data[[statdesc]]
        } else if (length(statdesc) >= i) {
          ret$statdesc <- data[[statdesc[[i]]]]
        }
      } else {
        if (length(statdesc) == 1) {
          ret$statdesc <- statdesc
        } else if (length(statdesc) >= i) {
          ret$statdesc <- statdesc[[i]]
        }
      }
    }

    # Populate By variables and values
    if (!is.null(byvars)) {
      for (j in seq_along(length(byvars))) {
        ret[[paste0("byvar", j)]] <- byvars[[j]]

        if (!is.null(data[[byvars[[j]]]]))
          ret[[paste0("byval", j)]] <- data[[byvars[[j]]]]

      }
    }

    # Populate Analysis Variable and Value
    if (!is.null(anal_var)) {
      ret[["anal_var"]] <- anal_var
      if (is.null(anal_val)) {
        if (all(anal_var %in% nms)) {

          ret[["anal_val"]] <- data[[anal_var]]
        }
      } else if (all(anal_val %in% nms)) {
        ret[["anal_val"]] <- data[[anal_val]]
      } else {

        ret[["anal_val"]] <- anal_var
      }
    }


    # Populate Treatment Variable
    if (!is.null(trtvar)) {
      if (trtvar %in% nms) {

        ret[["trtvar"]] <- trtvar
        ret[["trtval"]] <- data[[trtvar]]

      } else {

        ret[["trtval"]] <- trtvar
      }
    }



    # Convert to data frame and recycle template values.
    ret <- data.frame(ret)

    # Append to ards
    ardsenv$ards <- rbind(ardsenv$ards, ret)

  }

  return(data)

}

#' @title Returns the Current Analysis Results Dataset
#' @description The \code{get_ards} function returns the current state
#' of the analysis dataset.  This data frame may be saved to disk, saved in
#' a database, or examined from code.  The function takes no parameters.
#' @return A data frame of the current analysis results.
#' @export
get_ards <- function() {

 return(ardsenv$ards)

}
