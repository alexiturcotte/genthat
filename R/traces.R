#
# Functions to make up the info to go in a trace
#
get_attrs <- function(v) {
  # before we start, make sure we're not dealing with an error
  if (!is.null(attr(v, "typeR::did_it_work")))
    "error"
  else {
    # first, get the attributes
    the_attrs <- attributes(v)

    # if NA, no attributes
    if (is.null(the_attrs) || is.na(the_attrs) || length(the_attrs) == 0) {
      retv <- NA
    }
    else {
      # get names of attributes which are not genthats
      # is genthat_extracted_closure in ret
      if ("genthat_extracted_closure" %in% names(the_attrs)) {
        # remove it if it is
        the_attrs["genthat_extracted_closure"] <- NULL
      }
      if (length(the_attrs) == 0)
        retv <- NA
      else
        retv <- lapply(the_attrs, get_type)
        # names(the_attrs) -> retv
    }
    retv[sort(names(retv))]
  }
}

#
get_type <- function(v) {

  if (!is.null(attr(v, "typeR::did_it_work")))
    "error"
  else {

    r_t <- tryCatch(
      typeof(v),
      error = function(e) NA )

    if ( r_t == "logical" && is.na(v)) {
      r_t <- "raw_NA"

    # handle lists
    } else if (r_t == "list") {
      if (is.data.frame(v)) {
        # can't look inside
        r_t <- "data.frame"
      } else {
        types_in_list <- unique(sapply(v[!is.na(v)], typeof))

        if (length(types_in_list) == 1) {
          # only one
          r_t <- paste("list<", types_in_list, ">", sep="")
        } else {
          # more than one
          r_t <- "list<any>"
        }
      }
    } # handle vectors
    else if (r_t %in% c("double", "integer", "logical", "complex", "character", "raw")) {
      # could be a vector
      if (length(v) == 1) {
        # scalar
        r_t <- paste("scalar", r_t, sep="/")
      } else {
        # vector
        r_t <- paste("vector", r_t, sep="/")
      }
    }
    r_t
  }
}

#
get_class <- function(v) {
  if (!is.null(attr(v, "typeR::did_it_work")))
    "error"
  else {
    class(v)
  }
}

#
# typeR modification: change to record type information only in the trace
# alexi / Feb 2019
#
create_trace <- function(fun, pkg=NULL, args=list(), globals=list(), retv, seed, error, failure, skipped=0) {
    stopifnot(is.character(fun) && length(fun) == 1)
    stopifnot(is.null(pkg) || (is.character(pkg) && length(pkg) == 1))
    stopifnot(missing(retv) || missing(error) || missing(failure))

    # old trace result from genthat, which built up a minimum environment to
    # rerun the test
    # trace <- list(fun=fun, pkg=pkg, args=as.list(args), globals=as.list(globals))
    # globals <- as.list(globals)
    # special_eval <- function(x) {
    #   tryCatch(
    #     eval(x, globals),
    #     error = function(e) {
    #       r <- list()
    #       attr(r, "typeR::did_it_work") <- FALSE
    #       r
    #   })
    # }
    # args <- lapply(as.list(args), special_eval)

    # build up...
    # ... arg_types:
    arg_types <- lapply(args, get_type)

    # ... arg_attrs:
    arg_attrs <- lapply(args, get_attrs)

    #
    # # ... arg_classes:
    arg_classes <- lapply(args, class)

    # compute the file that ran
    file_ran <- getOption("genthat.current_file")
    file_ran <- unlist(strsplit(file_ran, split="/"))
    file_ran <- paste(file_ran[length(file_ran)-1], file_ran[length(file_ran)], sep="/")
    # get_last_slash <- regexpr("/[^/]*$", file_ran)
    # get_last_slash[1] has location of last slash, +1 skips slash
    # file_ran <- substr(file_ran, get_last_slash[1]+1, nchar(file_ran))

    trace <- list( fun=fun,
                   pkg=pkg,
                   arg_types=arg_types,
                   arg_attrs=arg_attrs,
                   arg_classes=arg_classes,
                   file_ran=file_ran,
                   times_seen=1)

    if (!missing(retv)) {
        # trace$retv <- retv
        # retv <- special_eval(retv)
        trace$arg_types <- c(trace$arg_types, list(retv=get_type(retv)))
        trace$arg_attrs <- c(trace$arg_attrs, list(retv=get_attrs(retv)))
        trace$arg_classes <- c(trace$arg_classes, list(retv=get_class(retv)))
        class(trace) <- "genthat_trace"
    } else if (!missing(error)) {
        trace$error <- error
        class(trace) <- "genthat_trace_error"
    } else if (!missing(failure)) {
        trace$failure <- failure
        class(trace) <- "genthat_trace_failure"
    } else if (skipped > 0) {
        trace$skipped <- skipped
        class(trace) <- "genthat_trace_skipped"
    } else {
        class(trace) <- "genthat_trace_entry"
    }

    trace
}

create_trace_old <- function(fun, pkg=NULL, args=list(), globals=list(), retv, seed, error, failure, skipped=0) {
    stopifnot(is.character(fun) && length(fun) == 1)
    stopifnot(is.null(pkg) || (is.character(pkg) && length(pkg) == 1))
    stopifnot(missing(retv) || missing(error) || missing(failure))

    trace <- list(fun=fun, pkg=pkg, args=as.list(args), globals=as.list(globals))

    if (!missing(seed)) {
        trace$seed <- seed
    }

    if (!missing(retv)) {
        trace$retv <- retv
        class(trace) <- "genthat_trace"
    } else if (!missing(error)) {
        trace$error <- error
        class(trace) <- "genthat_trace_error"
    } else if (!missing(failure)) {
        trace$failure <- failure
        class(trace) <- "genthat_trace_failure"
    } else if (skipped > 0) {
        trace$skipped <- skipped
        class(trace) <- "genthat_trace_skipped"
    } else {
        class(trace) <- "genthat_trace_entry"
    }

    trace
}

#' @export
format.genthat_trace <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}

#' @export
format.genthat_trace_entry <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}

#' @export
format.genthat_trace_error <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}

#' @export
format.genthat_trace_failure <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}
