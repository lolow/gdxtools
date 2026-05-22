#' gdxtools: manipulate 'GDX' files
#'
#' Read and write 'GDX' files ('GAMS' data exchange) using the
#' 'GAMS'-maintained \code{gamstransfer} package as the underlying backend.
#' Symbols are returned as plain data frames with domain columns plus a
#' \code{value} column, preserving the API of earlier (gdxrrw-backed) versions.
#'
#' @name gdxtools
#' @keywords internal
"_PACKAGE"

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
#' @return A named \code{list} with one \code{data.frame} per requested item,
#'   each the row-bind of that item extracted from every file with a
#'   \code{gdx} column identifying the source file.
#' @author Laurent Drouet
#' @examples
#'  f1 <- tempfile(fileext = ".gdx")
#'  f2 <- tempfile(fileext = ".gdx")
#'  write.gdx(f1, list(myparam = data.frame(i = c("a", "b"), value = 1:2)))
#'  write.gdx(f2, list(myparam = data.frame(i = c("a", "b"), value = 3:4)))
#'  allparam <- batch_extract("myparam", files = c(f1, f2))
#' @export
batch_extract <- function(items, files = NULL, gdxs = NULL, ...) {
  if (is.null(gdxs)) gdxs <- lapply(files, gdx)
  # Pre-load all requested items in one read per file so we pay the
  # gamstransfer C++ call cost once per gdx, not once per (item, gdx).
  for (g in gdxs) load_records(g, items)
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

# Flag double-precision columns with non-integer content used as indices.
# UELs are strings; a real index column is typically character, factor, or
# integer-valued (e.g. year). A double column with fractional values almost
# always means the caller forgot to drop a stray numeric column from the
# source data (e.g. a `low` / `high` column left over from the raw parquet),
# which would silently encode the floats as UEL strings — a class of bug
# that is very hard to spot in the resulting GDX. Integer-valued doubles
# (year stored as `1850.0`) are common enough to be silent.
.check_index_double <- function(records, idx_idxs, sym_name, sym_kind) {
  for (j in idx_idxs) {
    col <- records[[j]]
    if (!is.double(col)) next
    finite <- col[is.finite(col)]
    if (!length(finite)) next
    if (any(finite != floor(finite))) {
      warning(sprintf(
        "%s '%s': index column '%s' has non-integer numeric values; this usually means a stray non-value column slipped in and will be encoded as UEL strings. Drop it before calling write.gdx, or coerce to character if intentional.",
        sym_kind, sym_name, names(records)[j]
      ), call. = FALSE)
    }
  }
}

# Apply the user's NA policy to a records data.frame. "drop" matches the
# legacy v0.7 behavior (NA / NaN value rows silently disappear, equivalent
# to GAMS reading 0 for those keys); "keep" preserves them so they round-
# trip as GAMS NA / undef; "error" stops with an informative message so the
# caller can fix the upstream data.
.apply_na_policy <- function(records, na, sym_name, sym_kind) {
  na_rows <- is.na(records$value)
  if (!any(na_rows)) return(records)
  switch(na,
    drop  = records[!na_rows, , drop = FALSE],
    keep  = records,
    error = stop(sprintf(
      "%s '%s': %d NA/NaN value row(s) present; pass na = \"drop\" or \"keep\" to write.gdx",
      sym_kind, sym_name, sum(na_rows)
    ), call. = FALSE)
  )
}

# Apply the user's duplicate-key policy. "first" matches the legacy v0.7
# behavior (write2.gdx → wgdx kept the first row when a key repeated);
# "last" matches the legacy `write.gdx` GAMS-process path (each assignment
# overwrote the previous one); "error" stops so the caller can dedupe
# upstream. The warning fires for `first` and `last` so dropped rows are
# always audible.
.apply_dup_policy <- function(records, idx_idxs, dup, sym_name, sym_kind) {
  if (nrow(records) == 0L) return(records)
  idx_only <- records[, idx_idxs, drop = FALSE]
  drop_rows <- switch(dup,
    first = duplicated(idx_only, fromLast = FALSE),
    last  = duplicated(idx_only, fromLast = TRUE),
    error = {
      d <- duplicated(idx_only) | duplicated(idx_only, fromLast = TRUE)
      if (any(d)) {
        stop(sprintf(
          "%s '%s': %d duplicate key row(s) present; pass dup = \"first\" or \"last\" to write.gdx",
          sym_kind, sym_name, sum(d)
        ), call. = FALSE)
      }
      logical(nrow(records))
    }
  )
  if (any(drop_rows)) {
    warning(sprintf(
      "%s '%s': %d duplicate key row(s) collapsed (%s value kept)",
      sym_kind, sym_name, sum(drop_rows), dup), call. = FALSE)
    records <- records[!drop_rows, , drop = FALSE]
  }
  records
}

# Common writer used by both write.gdx and write2.gdx.
.write_container <- function(file, params, vars_l, vars_lo, vars_up, sets,
                             compress, na, dup) {
  na  <- match.arg(na,  c("drop", "keep", "error"))
  dup <- match.arg(dup, c("first", "last", "error"))
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
      dup_rows <- duplicated(s, fromLast = TRUE)
      if (any(dup_rows)) {
        warning(sprintf(
          "set '%s': %d duplicate row(s) collapsed",
          name, sum(dup_rows)), call. = FALSE)
        s <- s[!dup_rows, , drop = FALSE]
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
      .check_index_double(p, idx_idxs, name, "parameter")
      for (j in idx_idxs) p[[j]] <- as.character(p[[j]])
      p[[v_idx]] <- as.numeric(p[[v_idx]])
      records <- p[, c(idx_idxs, v_idx), drop = FALSE]
      names(records) <- c(make.unique(idx_cols), "value")
      # Apply NA policy first (so the user's choice governs whether NAs
      # survive), then drop zero rows. Use `is.na | x != 0` rather than
      # `x != 0` directly so NA logicals don't leak into indexing and
      # produce all-NA rows when na == "keep".
      records <- .apply_na_policy(records, na, name, "parameter")
      keep <- is.na(records$value) | records$value != 0
      records <- records[keep, , drop = FALSE]
      .check_index_na(records, seq_along(idx_cols), name, "parameter")
      records <- .apply_dup_policy(records, seq_along(idx_cols), dup,
                                   name, "parameter")
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
    # Run the stray-column check on the user-supplied data.frames before
    # .merge_var_records coerces every index column to character (which
    # would mask the double-precision flag).
    for (src in list(vars_l[[vn]], vars_lo[[vn]], vars_up[[vn]])) {
      if (is.null(src)) next
      src <- as.data.frame(src)
      if (ncol(src) == 0) next
      v_idx_src <- if ("value" %in% names(src)) match("value", names(src)) else ncol(src)
      .check_index_double(src, setdiff(seq_len(ncol(src)), v_idx_src),
                          vn, "variable")
    }
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
      # Drop default-valued records to match legacy v0.7 (`subset(v, value!=0)`
      # for level, `subset(v, !is.infinite(value))` for lower / upper). Keep
      # a row only when at least one provided field differs from its free-var
      # default (level 0, lower -Inf, upper +Inf). Downstream tools (WITCH's
      # witchtools) rely on the absence of zero-level records to detect "no
      # value at this t/n" and back-fill from the first non-default record.
      if (nrow(rec) > 0L && length(val_cols_present) > 0L) {
        keep <- logical(nrow(rec))
        for (vc in val_cols_present) {
          col <- rec[[vc]]
          default <- switch(vc,
            level = 0,
            lower = -Inf,
            upper = +Inf
          )
          keep <- keep | (!is.na(col) & col != default)
        }
        rec <- rec[keep, , drop = FALSE]
      }
      rec <- .apply_dup_policy(rec, idx_idxs, dup, vn, "variable")
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
#' @param na how to handle NA / NaN values in parameter \code{value} columns:
#'   \code{"drop"} (default, legacy v0.7 behavior: NA rows are silently
#'   discarded and GAMS reads 0 for those keys), \code{"keep"} (preserve as
#'   GAMS NA / undef), or \code{"error"} (stop with an informative message).
#' @param dup how to collapse duplicate index keys: \code{"first"} (default,
#'   matches legacy \code{write2.gdx}/\code{wgdx} behavior), \code{"last"}
#'   (matches the legacy \code{write.gdx} GAMS-process path, where each
#'   assignment overwrote the previous one), or \code{"error"} (stop so the
#'   caller can dedupe upstream). When rows are dropped a warning reports
#'   the count.
#' @return Invisibly returns \code{0}; called for the side effect of writing
#'   the gdx file at \code{file}.
#' @author Laurent Drouet
#' @examples
#'  param1 <- data.frame(x = c('1', '2'), value = 1:2)
#'  param2 <- data.frame(a = c('london', 'paris'), value = c(50, 0.2))
#'  write.gdx(tempfile(fileext = ".gdx"),
#'            list(param1 = param1, param2 = param2))
write.gdx <- function(file, params = list(),
                      vars_l = list(), vars_lo = list(), vars_up = list(),
                      sets = list(),
                      removeLST = TRUE, usetempdir = TRUE,
                      digits = 16, compress = FALSE,
                      na = c("drop", "keep", "error"),
                      dup = c("first", "last", "error")) {
  .write_container(file, params, vars_l, vars_lo, vars_up, sets,
                   compress, na, dup)
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
#' @param na how to handle NA / NaN values; see \code{\link{write.gdx}}.
#' @param dup how to collapse duplicate index keys; see \code{\link{write.gdx}}.
#' @return Invisibly returns \code{0}; called for the side effect of writing
#'   the gdx file at \code{file}.
#' @author Laurent Drouet
write2.gdx <- function(file, params = list(), sets = list(),
                       na = c("drop", "keep", "error"),
                       dup = c("first", "last", "error")) {
  .write_container(file, params, list(), list(), list(), sets,
                   compress = FALSE, na = na, dup = dup)
}
