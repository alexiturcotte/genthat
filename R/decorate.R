#' @export
#'
create_decorator <- function(method="on.exit") {
    fun <- if (is.character(method)) {
        method <- match.arg(arg=method, choices=c("onentry", "onexit", "on.exit", "onboth", "trycatch", "count-entry", "count-exit", "noop"), several.ok=FALSE)

        switch(method,
            onentry=decorate_with_onentry,
            onexit=decorate_with_onexit,
            on.exit=decorate_with_on.exit,
            onboth=decorate_with_onboth,
            trycatch=decorate_with_trycatch,
            `count-entry`=decorate_with_count_entry,
            `count-exit`=decorate_with_count_exit,
            noop=decorate_with_noop
        )
    } else if (is.function(method)) {
        method
    } else {
        stop("Unknown type of method: ", typeof(method))
    }

    structure(list(
        method=fun,
        decorations=new.env(parent=emptyenv(), hash=TRUE)
    ), class="decorator")
}

#' @title Decorates functions in an environment
#'
#' @param envir an environment that shall be decorated or a character scalar
#'     that will be resolved using `as.environment("package:....")`. In the
#'     latter case, the package will also be loaded in the case it has not been
#'     loaded yet.
#'
#' @description Decorates all symbols form the given environment for which
#'     `is.function` is `TRUE`.
#' @export
#'
decorate_environment <- function(env,
                                 decorator=get_decorator(),
                                 record_fun=substitute(genthat:::record_trace),
                                 exclude=character()) {
    stopifnot(is_decorator(decorator))

    if (is.character(env)) {
        stopifnot(length(env) == 1)

        log_debug("Loading namespace: ", env)
        library(env, character.only=TRUE)

        env <- getNamespace(env)
    }

    stopifnot(is.environment(env))

    names <- ls(env, all.names=TRUE)
    vals <- lapply(names, get, env=env)
    names(vals) <- names

    # need to skip at most these, as they interfere with the analysis
    # (we get infinite recursion, probably through onexit hooks)
    funs_to_skip <- list("::", ":::", "<-", "get", "globalEnv", "on.exit", "typeof", "class", "attributes")
    exclude <- c(exclude, funs_to_skip)

    funs <- filter(vals, is.function)
    funs <- funs[!(names(funs) %in% exclude)]

    res <- lapply(names(funs), function(name) {
        tryCatch({
            fun <- funs[[name]]
            decorate_function(fun, name=name, record_fun=record_fun, decorator=decorator, env=env)
            fun
        }, error=function(e) {
            e$message
        })
    })

    names(res) <- names(funs)

    invisible(res)
}

#' @title Decorates given functions
#'
#' @param fun the function that shall be decorated
#'
#' @description Given function will be decorated in their defining environment.
#' @export
#'
decorate_function <- function(fun, name=substitute(fun),
                              record_fun=substitute(genthat:::record_trace),
                              decorator=get_decorator(), env=parent.frame()) {
    stopifnot(is_decorator(decorator))
    stopifnot(!missing(fun) || is_chr_scalar(name))

    if (missing(fun)) {
        fun <- NULL
    }

    # TODO: check args

    if (is_tracing_enabled()) {
        disable_tracing()
        on.exit(enable_tracing())
    }

    resolved_fun <- resolve_function(name, fun, env)
    fun <- resolved_fun$fun
    fqn <- resolved_fun$fqn
    name <- resolved_fun$name
    package <- resolved_fun$package

    # TODO: test
    if (!is.function(fun)) {
        stop(fqn, ": is not a function")
    }

    # maybe let primitive functions go through?
    # if (is.primitive(fun)) {
    #     stop(fqn, ": is a primitive function")
    # }

    if (is_s3_generic(fun)) {
        stop(fqn, ": is a S3 generic function")
    }

    if (is_decorated(fun, fqn, decorator=decorator, env=env)) {
        reset_function(fun, fqn, decorator=decorator, env=env)
    }

    log_debug("Decorating function: ", name)

    orig_fun <- create_duplicate(fun)
    new_fun <- decorator$method(fun, name, package, record_fun)

    reassign_function(fun, new_fun)

    assign(fqn, list(fun=fun, orig=orig_fun), envir=decorator$decorations)

    invisible(NULL)
}

#' @title Resets decorated function back to its original
#'
#' @description Reverts decorated function back to their state they were before calling `decorate_function`.
#' @export
#'
reset_function <- function(fun, name=substitute(fun), decorator=get_decorator(), env=parent.frame()) {
    stopifnot(is_decorator(decorator))
    stopifnot(!missing(fun) || is_chr_scalar(name))

    if (missing(fun)) {
        fun <- NULL
    }

    resolved_fun <- resolve_function(name, fun, env)

    fun <- resolved_fun$fun
    fqn <- resolved_fun$fqn

    if (!is_decorated(fun, fqn, decorator=decorator, env=env)) {
        warning("Function ", fqn, " is not decorated")
        return(NULL)
    }

    log_debug("Resetting decorated function: ", fqn)

    rec <- get(fqn, envir=decorator$decorations)

    reassign_function(fun, rec$orig)

    rm(list=fqn, envir=decorator$decorations)

    invisible(NULL)
}

#' @export
#'
is_decorated <- function(fun, name=substitute(fun), decorator=get_decorator(), env=parent.frame()) {
    stopifnot(is_decorator(decorator))
    stopifnot(!missing(fun) || is_chr_scalar(name))

    if (missing(fun)) {
        fun <- NULL
    }

    resolved_fun <- resolve_function(name, fun, env)
    exists(resolved_fun$fqn, envir=decorator$decorations)
}

#' @export
#'
set_decorator <- function(decorator) {
    stopifnot(is_decorator(decorator))

    options(genthat.decorator=decorator)

    invisible(decorator)
}

#' @export
#'
get_decorator <- function() {
    decorator <- getOption("genthat.decorator")

    if (is.null(decorator)) {
        decorator <- create_decorator()
        set_decorator(decorator)
    }

    decorator
}

is_decorator <- function(x) {
    inherits(x, "decorator")
}

is_s3_generic <- function(fun) {
    stopifnot(is.function(fun))

    globals <- codetools::findGlobals(fun, merge = FALSE)$functions
    any(globals == "UseMethod")
}
