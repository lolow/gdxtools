#' gdxtools
#'
#' @name gdxtools
#' @docType package
#' @description
#' Read and write GDX files (GAMS database exchange) using the GAMS-maintained
#' \code{gamstransfer} package as the underlying backend. Symbols are returned
#' as plain data.frames with domain columns plus a \code{value} column,
#' preserving the API of earlier (gdxrrw-backed) versions.
NULL

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()
  packageStartupMessage(paste("gdxtools", utils::packageVersion("gdxtools"),
                              "(gamstransfer backend)"))
}

#' Extract a list of items from many GDX
#'
#' @param items vector or list of items to extract
#' @param files list of files; if \code{NULL}, \code{gdxs} must be supplied
#' @param gdxs list of \code{gdx} objects; if \code{NULL}, \code{files} must be supplied
#' @param ... passed to \code{extract.gdx}
#' @author Laurent Drouet
#' @examples
#'  \dontrun{
#'    myfiles  <- c("test1.gdx", "test2.gdx")
#'    allparam <- batch_extract("myparam", files = myfiles)
#'  }
#' @export
batch_extract <- function(items, files = NULL, gdxs = NULL, ...) {
  if (is.null(gdxs)) gdxs <- lapply(files, gdx)
  out <- list()
  for (item in items) {
    rows <- lapply(gdxs, extract, item, addgdx = TRUE, ...)
    out[[item]] <- do.call("rbind", rows)
  }
  out
}

# Pull an attribute named "gams" off a data.frame (used to carry GAMS symbol
# descriptions). Returns "" when missing.
.text_attr <- function(x) {
  a <- attributes(x)$gams
  if (is.null(a)) "" else as.character(a)
}

.coerce_name <- function(provided, fallback) {
  if (is.null(provided) || is.na(provided) || provided == "") fallback else provided
}

# Merge level/lower/upper data.frames for the same variable into a single
# records frame indexed by the union of all index combinations.
.merge_var_records <- function(vl, vlo, vup) {
  inputs <- list()
  if (!is.null(vl))  inputs[["level"]] <- vl
  if (!is.null(vlo)) inputs[["lower"]] <- vlo
  if (!is.null(vup)) inputs[["upper"]] <- vup
  if (length(inputs) == 0) return(NULL)

  norm <- function(df, valname) {
    df <- as.data.frame(df)
    if (ncol(df) == 0) return(NULL)
    val_col <- if ("value" %in% names(df)) "value" else names(df)[ncol(df)]
    idx_cols <- setdiff(names(df), val_col)
    out <- df[, c(idx_cols, val_col), drop = FALSE]
    names(out)[ncol(out)] <- valname
    for (j in idx_cols) out[[j]] <- as.character(out[[j]])
    out[[valname]] <- as.numeric(out[[valname]])
    out
  }
  normed <- Map(norm, inputs, names(inputs))
  normed <- normed[!vapply(normed, is.null, logical(1))]
  if (length(normed) == 0) return(NULL)
  if (length(normed) == 1) return(normed[[1]])

  idx_cols <- setdiff(names(normed[[1]]), names(inputs)[1])
  Reduce(function(a, b) merge(a, b, by = idx_cols, all = TRUE), normed)
}

# Resolve domain spec: returns "*" when the column name is not registered as a
# set in the container, otherwise returns the set name (gamstransfer resolves
# string names to Set objects).
.resolve_domain <- function(m, idx_cols, registered_sets) {
  vapply(idx_cols, function(cn) {
    if (cn == "*" || !cn %in% registered_sets) "*" else cn
  }, character(1))
}

# A column name is treated as a set reference only when it is a valid GAMS
# identifier — letters / digits / underscore, starting with a letter. Names
# like "X." (which R produces from `data.frame(`*`=...)`) fall back to the
# universe domain.
.is_valid_gams_name <- function(s) {
  is.character(s) & nzchar(s) & grepl("^[a-zA-Z][a-zA-Z0-9_]*$", s)
}

# Common writer used by both write.gdx and write2.gdx.
.write_container <- function(file, params, vars_l, vars_lo, vars_up, sets, compress) {
  m <- gamstransfer::Container$new()

  # Step 1 — gather the universe of values for each named index column,
  # mirroring how the legacy implementation auto-built sets from union of
  # all parameter / variable / set index columns. Skip column names that
  # aren't valid GAMS identifiers; those become "*" domains downstream.
  collect <- list()
  add_to <- function(df, val_col_name) {
    df <- as.data.frame(df)
    if (ncol(df) == 0) return(invisible())
    cols <- names(df)
    idx_cols <- setdiff(cols, val_col_name)
    for (cn in idx_cols) {
      if (!.is_valid_gams_name(cn)) next
      collect[[cn]] <<- unique(c(collect[[cn]], as.character(df[[cn]])))
    }
  }
  for (s in sets)    add_to(s, character(0))
  for (p in params)  add_to(p, "value")
  for (v in c(vars_l, vars_lo, vars_up)) add_to(v, "value")

  # Step 2 — register universe-domain sets for index column names that are not
  # already provided as explicit sets in the call.
  explicit_set_names <- setdiff(names(sets), "")
  auto_set_names <- setdiff(names(collect), explicit_set_names)
  for (sn in auto_set_names) {
    rec <- data.frame(uni = collect[[sn]], stringsAsFactors = FALSE)
    m$addSet(sn, "*", records = rec)
  }

  registered_sets <- function() m$listSets()

  # Step 3 — explicit sets.
  for (i in seq_along(sets)) {
    name <- .coerce_name(names(sets)[i], paste0("set", i))
    s <- as.data.frame(sets[[i]])
    dim <- ncol(s)
    cols <- names(s)
    for (j in seq_len(dim)) s[[j]] <- as.character(s[[j]])
    if (name %in% cols) {
      domains <- as.list(rep("*", dim))
    } else {
      domains <- as.list(.resolve_domain(m, cols, registered_sets()))
    }
    if (m$hasSymbols(name)) m$removeSymbols(name)
    m$addSet(name, domains, records = s, description = .text_attr(sets[[i]]))
  }

  # Step 4 — parameters.
  for (i in seq_along(params)) {
    name <- .coerce_name(names(params)[i], paste0("param", i))
    p <- as.data.frame(params[[i]])
    dim <- ncol(p) - 1L
    if (dim < 0) dim <- 0L
    if (dim == 0) {
      val <- if (nrow(p) > 0) as.numeric(p[[1]][1]) else 0
      m$addParameter(name, records = val, description = .text_attr(params[[i]]))
    } else {
      val_col <- if ("value" %in% names(p)) "value" else names(p)[ncol(p)]
      idx_cols <- setdiff(names(p), val_col)
      for (j in idx_cols) p[[j]] <- as.character(p[[j]])
      p[[val_col]] <- as.numeric(p[[val_col]])
      names(p)[names(p) == val_col] <- "value"
      p <- p[p$value != 0, , drop = FALSE]
      domains <- as.list(.resolve_domain(m, idx_cols, registered_sets()))
      m$addParameter(name, domains, records = p,
                     description = .text_attr(params[[i]]))
    }
  }

  # Step 5 — variables (level/lower/upper merged by index).
  vnames <- unique(c(names(vars_l), names(vars_lo), names(vars_up)))
  vnames <- vnames[!is.na(vnames) & vnames != ""]
  for (vn in vnames) {
    rec <- .merge_var_records(vars_l[[vn]], vars_lo[[vn]], vars_up[[vn]])
    if (is.null(rec) || nrow(rec) == 0) next
    description_ref <- vars_l[[vn]]
    if (is.null(description_ref)) description_ref <- vars_lo[[vn]]
    if (is.null(description_ref)) description_ref <- vars_up[[vn]]
    desc <- .text_attr(description_ref)
    val_cols_present <- intersect(c("level", "lower", "upper"), names(rec))
    idx_cols <- setdiff(names(rec), val_cols_present)
    dim <- length(idx_cols)
    if (dim == 0) {
      m$addVariable(vn, "free",
                    records = as.data.frame(rec[1, val_cols_present, drop = FALSE]),
                    description = desc)
    } else {
      domains <- as.list(.resolve_domain(m, idx_cols, registered_sets()))
      m$addVariable(vn, "free", domains, records = rec, description = desc)
    }
  }

  if (isTRUE(compress)) m$write(file, compress = TRUE) else m$write(file)
  invisible(0)
}

#' Write a list of parameters / sets / variables to a GDX
#'
#' Builds a \code{gamstransfer::Container} from the supplied data and writes it
#' to \code{file}. Variables can be given as separate level / lower / upper
#' data.frames (keyed by the same variable name across the three lists);
#' missing entries fall back to the gamstransfer defaults (level 0,
#' lower -Inf, upper +Inf for free variables).
#'
#' @export
#' @param file the output gdx filename
#' @param params named list of parameter data.frames
#' @param vars_l named list of variable level data.frames
#' @param vars_lo named list of variable lower-bound data.frames
#' @param vars_up named list of variable upper-bound data.frames
#' @param sets named list of set data.frames
#' @param removeLST kept for backward compatibility; ignored.
#' @param usetempdir kept for backward compatibility; ignored.
#' @param digits kept for backward compatibility; ignored (gamstransfer
#'   preserves full numeric precision).
#' @param compress when \code{TRUE}, write a compressed gdx.
#' @author Laurent Drouet
#' @examples
#'  \dontrun{
#'    param1 <- data.frame(x = c('1','2'), value = 1:2)
#'    param2 <- data.frame(a = c('london','paris'), value = c(50, 0.2))
#'    write.gdx("test.gdx", list(param1 = param1, param2 = param2))
#'  }
write.gdx <- function(file, params = list(),
                      vars_l = list(), vars_lo = list(), vars_up = list(),
                      sets = list(),
                      removeLST = TRUE, usetempdir = TRUE,
                      digits = 16, compress = FALSE) {
  .write_container(file, params, vars_l, vars_lo, vars_up, sets, compress)
}

#' Write parameters and sets to a gdx (alias of write.gdx for backward compatibility)
#'
#' Historically this was a faster path that bypassed the GAMS process used by
#' the legacy \code{write.gdx}. With the gamstransfer backend both functions
#' use the same fast path; this entry point is kept for code that calls it
#' explicitly.
#'
#' @export
#' @param file the output gdx filename
#' @param params named list of parameter data.frames
#' @param sets named list of set data.frames
#' @author Laurent Drouet
write2.gdx <- function(file, params = list(), sets = list()) {
  .write_container(file, params, list(), list(), list(), sets, compress = FALSE)
}
