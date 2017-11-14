library(jsonlite)


## JSON-ified versions of save() and load()
##
## Save a couple of objects to JSON
## test = list()
## test$num = 1
## test$str = "just a test"
## test2 = c("one", "two", "three")
## j <- json_save(test, test2)
##
## Load them back into the current environment, but names prefixed with "n_"
## json_load(j, prefix = "n_")


json_save <- function(..., list = character(), envir = parent.frame()) {
    ## code from src/library/base/R/load.R
    ## Gather names. If called as json_save(test, test2):
    ##   substitute(list(...)) -> character()(test, test2)
    ##   as.character(substitute(list(...))) -> [1] "character()" "test" "test2"
    names <- as.character(substitute(list(...)))[-1L]
    if (missing(list) && !length(names))
	   warning("nothing specified to be save()d")
    list <- c(list, names)
    ## Precheck for existence of objects
	ok <- vapply(list, exists, NA, envir = envir)
    if (!all(ok)) {
        n <- sum(!ok)
        stop(sprintf(ngettext(n,
                "object %s not found",
                "objects %s not found"
                ),
            paste(sQuote(list[!ok]), collapse = ", ")
            ), domain = NA)
    }
    ## Build list of names and values
    objects <- list()
    for (x in list) {
        objects[[x]] <- get(x, envir = envir)
    }
    ## Return JSON string
    jsonlite::toJSON(objects, auto_unbox = TRUE)
}


json_load <- function(json, prefix = "", envir = parent.frame()) {
    objects <- jsonlite::fromJSON(json)
    lapply(names(objects), function(x) { envir[[paste0(prefix, x)]] <- objects[[x]] })
}
