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
# options(genthat.counts_file=Sys.getenv("GENTHAT_COUNTS_FILE"))
options(genthat.output_dir=Sys.getenv("GENTHAT_OUTPUT_DIR"))

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

# Decorate Base Env Functions
# First, this lets us reassign
unlockBinding(as.symbol("+"), baseenv())
unlockBinding(as.symbol("["), baseenv())
# This makes + into a closure, so we can deal with it
assign("+", function(e1, e2) .Primitive("+")(e1, e2), envir=baseenv())
assign("[", function(e1, e2, ...) {
  if (rlang::is_missing(e2)) { # e2 missing if you do e.g. df[, 2]
    .Primitive("[")(e1, rlang::missing_arg(), ...)
  } else {
    .Primitive("[")(e1, e2, ...), envir=baseenv()
  }
}, envir=baseenv())
# Actually decorate the base env functions
genthat::decorate_function(`+`, env=baseenv())
genthat::decorate_function(`[`, env=baseenv())

print("[:")
print(`[`)

reg.finalizer(
    e=loadNamespace("genthat"),
    onexit=TRUE,
    f=function(x) {
        genthat::disable_tracing()

        stats_file <- Sys.getenv("GENTHAT_STATS_FILE")

        # this is important since some tracing might spawn extra instances
        # of R in which case we want to keep all the traces together
        stats_file_exists <- file.exists(stats_file)
    }
)
