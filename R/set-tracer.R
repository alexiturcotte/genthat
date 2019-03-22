#' @export
#'
create_set_tracer <- function(session_file=NULL) {
    stopifnot(is.null(session_file) || is_chr_scalar(session_file))

    known_traces <-
        if (!is.null(session_file) && file.exists(session_file)) {
            if (is_debug_enabled()) {
                log_debug("Loading existing trace hashes from ", session_file)
            }

            readRDS(session_file)
        } else {
            new.env(parent=emptyenv(), hash=TRUE)
        }

    structure(
        list(
            traces=new.env(parent=emptyenv(), hash=TRUE),
            known_traces=known_traces,
            session_file=session_file,
            hash_to_loc_map=new.env(parent=emptyenv(), hash=TRUE) # new
        ),
        class="set_tracer"
    )
}

#' @export
#'
#
store_trace.set_tracer <- function(tracer, trace) {

  # trace <- list( fun=fun,
  #                pkg=pkg,
  #                arg_types=arg_types,
  #                arg_attrs=arg_attrs,
  #                arg_classes=arg_classes,
  #                file_ran=file_ran)

  # This will be called any time something might be stored.
  # Is this a good place to count the number of function invocations?

    arg_attrs = unname(lapply(trace$arg_attrs, function(x) {
      paste(c("{", paste(x[!is.na(x)], collapse=","), "}"), collapse="")
    }))

    new_and_hash <- is_interesting(pkg_name=trace$pkg, fun_name=trace$fun, arg_len=length(trace$arg_types),
                    arg_names=as.list(names(trace$arg_types)), arg_types=(unname(trace$arg_types)),
                    arg_attrs=arg_attrs, arg_classes=(unname(trace$arg_classes)))

    new_and_hash <- unlist(strsplit(new_and_hash, split="_"))
    is_new <- new_and_hash[1]
    hashed_sig <- new_and_hash[2]

    if (is_new == "1")
      is_new <- TRUE
    else # is_new == "0"
      is_new <- FALSE

    if (is_new) {
        where <- toString(length(tracer$traces) + 1)
        assign(where, TRUE, envir=tracer$known_traces)
        trace$hash <- hashed_sig
        tracer$traces[[where]] <- trace

        # process_traces(
        #     traces=list(trace), # stoopid hack
        #     output_dir=getOption("genthat.output_dir"),
        #     action="export"
        # )
        save_trace_file(trace, getOption("genthat.output_dir"), "trace")

        # make sure to save hash in case we see it later
        # tracer$hash_to_loc_map[[hashed_sig]] <- where
        # TODO: Also write the trace out????
        # log_debug("Counts file: ", getOption("genthat.counts_file"))
        # log_debug("getwd(): ", getwd())
        count_file <- readRDS(getOption("genthat.counts_file"))
        count_file[[trace$pkg]][paste(trace$fun, hashed_sig, sep="-")] <- 1
        saveRDS(count_file, getOption("genthat.counts_file"))
    } else {
    #     # update trace entry with new count
        count_file <- readRDS(getOption("genthat.counts_file"))
        subindex <- paste(trace$fun, hashed_sig, sep="-")
        count_file[[trace$pkg]][subindex] <- count_file[[trace$pkg]][subindex] + 1
        saveRDS(count_file, getOption("genthat.counts_file"))

        # tracer$traces[[tracer$hash_to_loc_map[[hashed_sig]]]]$times_seen <-
        # tracer$traces[[tracer$hash_to_loc_map[[hashed_sig]]]]$times_seen + 1
    }

    invisible(trace)
}

# #' @export
# #'
# #
# store_trace.set_tracer <- function(tracer, trace) {
#
#   # trace <- list( fun=fun,
#   #                pkg=pkg,
#   #                arg_types=arg_types,
#   #                arg_attrs=arg_attrs,
#   #                arg_classes=arg_classes,
#   #                file_ran=file_ran)
#
#   # This will be called any time something might be stored.
#   # Is this a good place to count the number of function invocations?
#
#     arg_attrs = unname(lapply(trace$arg_attrs, function(x) {
#       paste(c("{", paste(x[!is.na(x)], collapse=","), "}"), collapse="")
#     }))
#
#     new_and_hash <- is_interesting(pkg_name=trace$pkg, fun_name=trace$fun, arg_len=length(trace$arg_types),
#                     arg_names=as.list(names(trace$arg_types)), arg_types=(unname(trace$arg_types)),
#                     arg_attrs=arg_attrs, arg_classes=(unname(trace$arg_classes)))
#
#     new_and_hash <- unlist(strsplit(new_and_hash, split="_"))
#     is_new <- new_and_hash[1]
#     hashed_sig <- new_and_hash[2]
#
#     if (is_new == "1")
#       is_new <- TRUE
#     else # is_new == "0"
#       is_new <- FALSE
#
#     if (is_new) {
#         where <- toString(length(tracer$traces) + 1)
#         assign(where, TRUE, envir=tracer$known_traces)
#         tracer$traces[[where]] <- trace
#
#         # make sure to save hash in case we see it later
#         tracer$hash_to_loc_map[[hashed_sig]] <- where
#     } else {
#         # update trace entry with new count
#         tracer$traces[[tracer$hash_to_loc_map[[hashed_sig]]]]$times_seen <- tracer$traces[[tracer$hash_to_loc_map[[hashed_sig]]]]$times_seen + 1
#     }
#
#     invisible(trace)
# }

#' @export
#'
reset_traces.set_tracer <- function(tracer) {
    rm(list=ls(envir=tracer$known_traces, sort=FALSE, all.names=TRUE), envir=tracer$known_traces)
    rm(list=ls(envir=tracer$traces, sort=FALSE, all.names=TRUE), envir=tracer$traces)
}

#' @export
#'
copy_traces.set_tracer <- function(tracer) {
    if (!is.null(tracer$session_file)) {
        saveRDS(tracer$known_traces, tracer$session_file)
    }

    traces <- as.list(tracer$traces)
    names(traces) <- NULL
    traces
}
