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
            session_file=session_file
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

    arg_attrs = unname(lapply(trace$arg_attrs, function(x) {
      paste(c("{", paste(x[!is.na(x)], collapse=","), "}"), collapse="")
    }))

    if (is_interesting(pkg_name=trace$pkg, fun_name=trace$fun, arg_len=length(trace$arg_types),
                       arg_names=as.list(names(trace$arg_types)), arg_types=(unname(trace$arg_types)),
                       arg_attrs=arg_attrs, arg_classes=(unname(trace$arg_classes)))) {
        where <- toString(length(tracer$traces) + 1)
        assign(where, TRUE, envir=tracer$known_traces)
        tracer$traces[[where]] <- trace
    }

    invisible(trace)
}

# #' @export
# #'
# #
# store_trace.set_tracer <- function(tracer, trace) {
#     # we need to compute the digest without the seed
#     trace_without_seed <- trace
#     trace_without_seed$seed <- NULL
#
#     ser <- serialize(trace_without_seed, connection=NULL, ascii=FALSE)
#
#     if (length(ser) > getOption("genthat.max_trace_size", .Machine$integer.max)) {
#         trace <- create_trace(trace$fun, trace$pkg, skipped=length(ser))
#         ser <- serialize(trace, connection=NULL, ascii=FALSE)
#     }
#
#     key <- digest::digest(ser, algo="sha1", serialize=FALSE)
#
#     if (is.null(tracer$known_traces[[key]])) {
#         tracer$known_traces[[key]] <- TRUE
#         tracer$traces[[key]] <- trace
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
