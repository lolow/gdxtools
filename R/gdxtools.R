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
    v_idx    <- if ("value" %in% names(df)) match("value", names(df)) else ncol(df)
    idx_idxs <- setdiff(seq_len(ncol(df)), v_idx)
    idx_cols <- names(df)[idx_idxs]
    for (j in idx_idxs) df[[j]] <- as.character(df[[j]])
    df[[v_idx]] <- as.numeric(df[[v_idx]])
    out <- df[, c(idx_idxs, v_idx), drop = FALSE]
    names(out) <- c(make.unique(idx_cols), valname)
    out
  }
  normed <- Map(norm, inputs, names(inputs))
  normed <- normed[!vapply(normed, is.null, logical(1))]
  if (length(normed) == 0) return(NULL)
  if (length(normed) == 1) return(normed[[1]])

  idx_cols <- setdiff(names(normed[[1]]), names(inputs)[1])
  Reduce(function(a, b) merge(a, b, by = idx_cols, all = TRUE), normed)
}

# A column name is treated as a domain label only when it is a valid GAMS
# identifier — letters / digits / underscore, starting with a letter. Names
# like "X." (which R produces from `data.frame(`*`=...)`) fall back to the
# universe domain.
.is_valid_gams_name <- function(s) {
  is.character(s) & nzchar(s) & grepl("^[a-zA-Z][a-zA-Z0-9_]*$", s)
}

# For each index column name, return either a registered Set (when the user
# supplied an explicit set with that name) or the column name as a relaxed
# string domain. Invalid GAMS names collapse to "*". This matches the legacy
# behavior where parameters declared `parameter b(r)` even when the set `r`
# was not separately unloaded into the GDX.
.domain_for <- function(m, idx_cols, explicit_set_names) {
  lapply(idx_cols, function(cn) {
    if (!.is_valid_gams_name(cn)) return("*")
    if (cn %in% explicit_set_names) return(m[cn])
    cn
  })
}

# Guard against NAs in index columns — gamstransfer's underlying C++ aborts
# with an out-of-bounds vector access when any index value is NA, which
# crashes the R session (SIGABRT) rather than throwing a recoverable error.
# Pre-validate so the user gets an informative message instead.
.check_index_na <- function(records, idx_idxs, sym_name, sym_kind) {
  for (j in idx_idxs) {
    if (anyNA(records[[j]])) {
      stop(sprintf(
        "%s '%s': index column '%s' contains NA values; gamstransfer cannot encode NA UELs",
        sym_kind, sym_name, names(records)[j]
      ), call. = FALSE)
    }
  }
}

# Common writer used by both write.gdx and write2.gdx.
.write_container <- function(file, params, vars_l, vars_lo, vars_up, sets, compress) {
  m <- gamstransfer::Container$new()

  explicit_set_names <- setdiff(names(sets), "")

  # Sets — always written with universe domain regardless of column names
  # (matching the legacy `set name (*,...)` declaration). Set column names
  # never leak as separate symbols in the output GDX.
  for (i in seq_along(sets)) {
    name <- .coerce_name(names(sets)[i], paste0("set", i))
    s <- as.data.frame(sets[[i]])
    dim <- ncol(s)
    for (j in seq_len(dim)) s[[j]] <- as.character(s[[j]])
    .check_index_na(s, seq_len(dim), name, "set")
    if (nrow(s) > 0L) {
      dup <- duplicated(s, fromLast = TRUE)
      if (any(dup)) {
        warning(sprintf(
          "set '%s': %d duplicate row(s) collapsed",
          name, sum(dup)), call. = FALSE)
        s <- s[!dup, , drop = FALSE]
      }
    }
    if (m$hasSymbols(name)) m$removeSymbols(name)
    m$addSet(name, rep("*", dim), records = s,
             description = .text_attr(sets[[i]]))
  }

  # Parameters — index column names become relaxed domain references unless
  # they correspond to an explicit set the caller provided.
  for (i in seq_along(params)) {
    name <- .coerce_name(names(params)[i], paste0("param", i))
    p <- as.data.frame(params[[i]])
    dim <- ncol(p) - 1L
    if (dim < 0) dim <- 0L
    if (dim == 0) {
      val <- if (nrow(p) > 0) as.numeric(p[[1]][1]) else 0
      m$addParameter(name, records = val,
                     description = .text_attr(params[[i]]))
    } else {
      # Position-based — column names may be duplicated (e.g. "*", "*", "value"
      # from a data.table) and setdiff() would collapse those.
      v_idx    <- if ("value" %in% names(p)) match("value", names(p)) else ncol(p)
      idx_idxs <- setdiff(seq_len(ncol(p)), v_idx)
      idx_cols <- names(p)[idx_idxs]
      for (j in idx_idxs) p[[j]] <- as.character(p[[j]])
      p[[v_idx]] <- as.numeric(p[[v_idx]])
      records <- p[, c(idx_idxs, v_idx), drop = FALSE]
      names(records) <- c(make.unique(idx_cols), "value")
      # Drop zero rows (matches legacy `subset(p, value != 0)`) but keep
      # NA / NaN values — those map to GAMS NA / undef in the GDX. Use
      # `is.na | x != 0` rather than `x != 0` directly so NA logicals don't
      # leak into indexing and produce all-NA rows.
      keep <- is.na(records$value) | records$value != 0
      records <- records[keep, , drop = FALSE]
      .check_index_na(records, seq_along(idx_cols), name, "parameter")
      # Resolve duplicate keys with last-wins semantics, matching the legacy
      # GAMS-process behavior (gamstransfer otherwise rejects duplicates).
      if (nrow(records) > 0L) {
        idx_only <- records[, seq_along(idx_cols), drop = FALSE]
        dup <- duplicated(idx_only, fromLast = TRUE)
        if (any(dup)) {
          warning(sprintf(
            "parameter '%s': %d duplicate key row(s) collapsed (last value kept)",
            name, sum(dup)), call. = FALSE)
          records <- records[!dup, , drop = FALSE]
        }
      }
      domains <- .domain_for(m, idx_cols, explicit_set_names)
      m$addParameter(name, domains, records = records,
                     description = .text_attr(params[[i]]))
    }
  }

  # Variables — same domain logic as parameters, with level/lower/upper merged
  # on the union of provided indices. Empty input still writes a declaration
  # of the right dimension (matching legacy behavior).
  vnames <- unique(c(names(vars_l), names(vars_lo), names(vars_up)))
  vnames <- vnames[!is.na(vnames) & vnames != ""]
  for (vn in vnames) {
    rec <- .merge_var_records(vars_l[[vn]], vars_lo[[vn]], vars_up[[vn]])
    if (is.null(rec)) next
    description_ref <- vars_l[[vn]]
    if (is.null(description_ref)) description_ref <- vars_lo[[vn]]
    if (is.null(description_ref)) description_ref <- vars_up[[vn]]
    desc <- .text_attr(description_ref)
    val_cols_present <- intersect(c("level", "lower", "upper"), names(rec))
    # Index columns sit after the user's index data and may legitimately share
    # names (e.g. "*","*"); use position to separate them from level/lower/upper.
    val_idxs <- which(names(rec) %in% val_cols_present)
    idx_idxs <- setdiff(seq_len(ncol(rec)), val_idxs)
    idx_cols <- names(rec)[idx_idxs]
    dim <- length(idx_idxs)
    if (dim == 0) {
      if (nrow(rec) == 0) next  # scalar with nothing to write
      m$addVariable(vn, "free",
                    records = as.data.frame(rec[1, val_cols_present, drop = FALSE]),
                    description = desc)
    } else {
      .check_index_na(rec, idx_idxs, vn, "variable")
      if (nrow(rec) > 0L) {
        idx_only <- rec[, idx_idxs, drop = FALSE]
        dup <- duplicated(idx_only, fromLast = TRUE)
        if (any(dup)) {
          warning(sprintf(
            "variable '%s': %d duplicate key row(s) collapsed (last value kept)",
            vn, sum(dup)), call. = FALSE)
          rec <- rec[!dup, , drop = FALSE]
        }
      }
      domains <- .domain_for(m, idx_cols, explicit_set_names)
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
