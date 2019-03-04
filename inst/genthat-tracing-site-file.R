options(error = function() {
    traceback(3)
    quit(status = 1, save = "no")
})

# A: GENTHAT_CURRENT_FILE ... this is the one we want

options(genthat.debug=as.logical(Sys.getenv("GENTHAT_DEBUG", "FALSE")))
options(genthat.keep_failed_traces=as.logical(Sys.getenv("GENTHAT_KEEP_FAILED_TRACES", "FALSE")))
options(genthat.keep_all_traces=as.logical(Sys.getenv("GENTHAT_KEEP_ALL_TRACES", "FALSE")))
options(genthat.max_trace_size=as.integer(Sys.getenv("GENTHAT_MAX_TRACE_SIZE")))
options(genthat.current_file=Sys.getenv("GENTHAT_CURRENT_FILE"))

genthat::set_decorator(genthat::create_decorator(Sys.getenv("GENTHAT_DECORATOR")))

if (Sys.getenv("GENTHAT_TRACER") == "set") {
    local({
        session_file <- Sys.getenv("GENTHAT_SESSION_FILE")
        if (nchar(session_file) == 0) {
            session_file <- NULL
        }

        genthat::set_tracer(
            genthat::create_tracer(
                "set",
                session_file=session_file
            )
        )
    })
} else {
    genthat::set_tracer(genthat::create_tracer(Sys.getenv("GENTHAT_TRACER")))
}

library(methods)

for (pkg in strsplit(Sys.getenv("GENTHAT_PKGS"), ",", fixed=TRUE)[[1]]) {
    genthat::decorate_environment(pkg)
}

reg.finalizer(
    e=loadNamespace("genthat"),
    onexit=TRUE,
    f=function(x) {
        genthat::disable_tracing()

        ret <- genthat::process_traces(
            traces=genthat::copy_traces(genthat::get_tracer()),
            output_dir=Sys.getenv("GENTHAT_OUTPUT_DIR"),
            action=Sys.getenv("GENTHAT_ACTION")
        )

        stats_file <- Sys.getenv("GENTHAT_STATS_FILE")

        # this is important since some tracing might spawn extra instances
        # of R in which case we want to keep all the traces together
        stats_file_exists <- file.exists(stats_file)

        write.table(
            ret,
            file=stats_file,
            row.names=FALSE,
            col.names=!stats_file_exists,
            append=stats_file_exists,
            qmethod="double",
            sep=","
        )
    }
)
