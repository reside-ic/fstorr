##' Create an \code{fstorr} object - a place to store data by content
##' based on a global key.
##' @title Create an fstorr object
##'
##' @param root Root to store files in - a path to a directory.  If it
##'   exists it must be a directory and you should treat the contents
##'   as opaque.
##'
##' @param get_resource A function that gets a resource.  It must take
##'   as its argument a key, and it must return a raw vector
##'   (character vectors are not supported on return because they are
##'   complicated for thinking about line-endings and hashes).
##'
##' @param hash_content A function that hashes a resource.  If not
##'   given then \code{openssl::sha256} will be used.  If providing
##'   your own it must take a filename as an argument and return a
##'   character string corresponding to a hash.  It is \emph{very
##'   important} that the same mapping function here is used for all
##'   creations of an \code{fstorr} object!
##'
##' @return A \code{fstorr} object
##' @author Rich Fitzjohn
##' @export
##' @examples
##' path <- tempfile()
##' obj <- fstorr::fstorr(path, charToRaw)
##' obj$list()
##' obj$get("a")
##' obj$list()
fstorr <- function(root, get_resource, hash_content = NULL) {
  r6_fstorr$new(root, get_resource, hash_content %||% hash_content_default)
}


##' @importFrom R6 R6Class
r6_fstorr <- R6::R6Class(
  "fstorr",

  private = list(
    root = NULL,
    get_resource = NULL,
    hash_content = NULL,
    db = NULL,

    path_data = function(hash) {
      if (any(is.na(hash))) {
        ret <- rep(NA_character_, length(hash))
        i <- !is.na(hash)
        ret[i] <- private$path_data(hash[i])
        ret
      } else {
        file.path(private$root, "data", hash)
      }
    },

    insert1 = function(key) {
      data <- private$get_resource(key)

      stopifnot(is.raw(data))
      hash <- private$hash_content(data)
      path <- private$path_data(hash)

      writeBin(data, path)
      Sys.chmod(path, "0444")

      private$db$put(key, hash)

      invisible(path)
    }
  ),

  public = list(
    initialize = function(root, get_resource, hash_content = NULL) {
      dir.create(root, FALSE, TRUE)
      dir.create(file.path(root, "data"), FALSE, TRUE)
      private$root <- root
      private$get_resource <- get_resource
      private$hash_content <- hash_content
      private$db <- thor::mdb_env(file.path(root, "db"))
    },

    insert = function(key) {
      path <- self$path(key)
      i <- is.na(path)
      if (any(i)) {
        path[i] <- vapply(key[i], private$insert1, character(1))
      }
      invisible(path)
    },

    hash = function(key) {
      ret <- private$db$mget(key, FALSE)
      ret[!nzchar(ret)] <- NA_character_
      ret
    },

    path = function(key) {
      private$path_data(self$hash(key))
    },

    get = function(key, insert = TRUE) {
      ret <- self$path(key)
      msg <- is.na(ret)
      if (any(msg)) {
        if (!insert) {
          stop(sprintf("Did not find %s: %s",
                       ngettext(sum(msg), "key", "keys"),
                       paste(squote(key[msg]), collapse = ", ")),
               call. = FALSE)
        }
        ret[msg] <- self$insert(key[msg])
      }
      ret
    },

    list = function() {
      sort(private$db$list(as_raw = FALSE))
    },

    exists = function(key) {
      private$db$exists(key)
    }
  ))


hash_content_default <- function(content) {
  as.character(openssl::sha256(content))
}
