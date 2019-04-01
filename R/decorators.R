check_decorate_args <- function(fun, name, pkg, record_fun) {
    stopifnot(is.function(fun))
    stopifnot(is.character(name) && length(name) == 1)
    stopifnot(is.null(pkg) || (is.character(pkg) && length(pkg) == 1))
}

decorate_with_onboth <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    # The `retv <- BODY` is OK because in the case of multiple expressions, it
    # will be surrounded with `{}`.

    # The reason why we use .Internal(...) is that in the case base library is
    # decorated the `genthat::is_tracing_enabled` will invoke `::` and
    # consecutively other functions making it an infinite loop
    create_function(
        params=formals(fun),
        body=substitute({
            if (.Internal(getOption("genthat.tracing"))) {
                .Internal(options(genthat.tracing=FALSE))
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], env=parent.frame())
                .Internal(options(genthat.tracing=TRUE))
            }

           `__retv` <- BODY

            if (.Internal(getOption("genthat.tracing"))) {
                .Internal(options(genthat.tracing=FALSE))
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], retv=`__retv`, env=parent.frame())
                .Internal(options(genthat.tracing=TRUE))
            }

            `__retv`
         }, list(NAME=name, PKG=pkg, RECORD_FUN=record_fun, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_onentry <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    create_function(
        params=formals(fun),
        body=substitute({
            if (.Internal(getOption("genthat.tracing"))) {
                .Internal(options(genthat.tracing=FALSE))
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], env=parent.frame())
                .Internal(options(genthat.tracing=TRUE))
            }

            BODY
        }, list(NAME=name, PKG=pkg, RECORD_FUN=record_fun, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_onexit <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    # The `retv <- BODY` is OK because in the case of multiple expressions, it will be surrounded with `{}`.
    create_function(
        params=formals(fun),
        body=substitute({
            `__retv` <- BODY

            if (.Internal(getOption("genthat.tracing"))) {
                .Internal(options(genthat.tracing=FALSE))

                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], retv=`__retv`, env=parent.frame())

                .Internal(options(genthat.tracing=TRUE))
            }

            `__retv`
        }, list(NAME=name, PKG=pkg, RECORD_FUN=record_fun, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_count_entry <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    create_function(
        params=formals(fun),
        body=substitute({
            genthat:::store_trace(genthat:::get_tracer(), genthat:::create_trace(NAME, PKG))
            BODY
        }, list(NAME=name, PKG=pkg, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_count_exit <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    # which_file <- GENTHAT_CURRENT_FILE

    create_function(
        params=formals(fun),
        body=substitute({
            `__retv` <- BODY
            genthat:::store_trace(genthat:::get_tracer(), genthat:::create_trace(NAME, PKG)) # , current_file=which_file))
            `__retv`
        }, list(NAME=name, PKG=pkg, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_trycatch <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    create_function(
        params=formals(fun),
        body=substitute({
            if (.Internal(getOption("genthat.tracing"))) {
                .Internal(options(genthat.tracing=FALSE))

                frame <- new.env(parent=parent.frame())
                assign(NAME, attr(sys.function(), "__genthat_original_fun"), envir=frame)
                call <- sys.call()
                call[[1]] <- as.name(NAME)

                tryCatch({
                    .Internal(options(genthat.tracing=TRUE))
                    retv <- eval(call, envir=frame)

                    .Internal(options(genthat.tracing=FALSE))
                    RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], retv=retv, env=parent.frame())
                    .Internal(options(genthat.tracing=TRUE))

                    return(retv)
                },  error=function(e) {
                    .Internal(options(genthat.tracing=FALSE))

                    depth <- getOption("genthat.tryCatchDepth")
                    env <- parent.frame(depth + 2)
                    match_call <- match.call(
                        definition=sys.function(-depth - 1),
                        call=sys.call(-depth - 1),
                        envir=env
                    )

                    RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match_call)[-1], error=e, env=env)
                    .Internal(options(genthat.tracing=TRUE))

                    stop(e)
                })
            }

            frame <- new.env(parent=parent.frame())
            assign(NAME, attr(sys.function(), "__genthat_original_fun"), envir=frame)

            call <- sys.call()
            call[[1]] <- as.name(NAME)

            eval(call, envir=frame)
        }, list(
            NAME=name,
            PKG=pkg,
            RECORD_FUN=record_fun
        )),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_noop <- function(fun, name, pkg, record_fun) {
    fun
}

decorate_with_on.exit <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    create_function(
        params=formals(fun),
        body=substitute({
            `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())
            TMP
            on.exit({
                if (.Internal(getOption("genthat.tracing"))) {
                    .Internal(options(genthat.tracing=FALSE))
                    default <- genthat:::`__genthat_default_retv`
                    retv <- returnValue(default=default)

                    if (!identical(retv, default) && !genthat:::is_exception_returnValue(retv)) {
                        RECORD_FUN(
                            name=NAME,
                            pkg=PKG,
                            args=as.list(match.call())[-1],
                            retv=retv,
                            seed=`__genthat_captured_seed`,
                            env=parent.frame()
                        )
                    }

                    .Internal(options(genthat.tracing=TRUE))
                }
            })

            BODY
        }, list(
            NAME=name,
            PKG=pkg,
            RECORD_FUN=record_fun,
            BODY=body(fun),
            TMP={
                if (endsWith(name, "<-")) {
                    substitute(`__genthat_tmp` <- ARG_NAME, list(ARG_NAME=as.name(names(formals(fun))[1])))
                } else {
                    ""
                }
            })
        ),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}
