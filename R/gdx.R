#' GDX file
#'
#' Constructs a \code{gdx} object backed by a \code{gamstransfer::Container}.
#' By default the file is opened in \strong{lazy mode}: only symbol metadata
#' (names, dimensions, domains, descriptions) is read up front. Each symbol's
#' records are loaded on first access via \code{[.gdx} / \code{extract()} and
#' then cached on the container for subsequent calls. Pass \code{lazy = FALSE}
#' to read everything eagerly (legacy 1.0.0 behavior).
#'
#' @param filename filename of the gdx file
#' @param lazy if \code{TRUE} (default), defer reading symbol records until
#'   the symbol is actually accessed. If \code{FALSE}, load all records at
#'   open time.
#' @param ... extra fields stored on the resulting object
#' @return An object of class \code{gdx}: an S3 list holding the live
#'   \code{gamstransfer::Container}(s) and per-type symbol metadata
#'   (\code{variables}, \code{parameters}, \code{sets}, \code{equations}
#'   data.frames with \code{name}, \code{text} and \code{dim} columns).
#' @author Laurent Drouet
#' @examples
#'  f <- tempfile(fileext = ".gdx")
#'  write.gdx(f, list(demand = data.frame(city = c("paris", "lyon"),
#'                                         value = c(50, 20))))
#'  mygdx <- gdx(f)                  # lazy by default
#'  mygdx["demand"]                  # triggers a targeted read
#'  eager <- gdx(f, lazy = FALSE)
#' @export
gdx <- function(filename, lazy = TRUE, ...) {
  if (!file.exists(filename)) {
    warning(paste(filename, "does not exist!"))
    return(structure(list(filename = filename, .lazy = isTRUE(lazy), ...),
                     class = "gdx"))
  }

  m <- gamstransfer::Container$new()
  .gdx_silent_read(m, filename, records = !isTRUE(lazy))

  # Records are fetched into a *separate* container in lazy mode. Reading
  # symbols into the metadata container is O(symCount) per call because
  # gamstransfer revalidates the existing symbol table; reading into an
  # empty container is O(1). On a 4496-symbol WITCH gdx that's ~10 ms vs.
  # ~800 ms per extract. In eager mode the metadata container already holds
  # every record, so we alias the two.
  rec_ct <- if (isTRUE(lazy)) gamstransfer::Container$new() else m

  # Single-pass describe: pull all symbol objects once and classify them.
  # The legacy per-type approach (5 list* + dim/desc vapply per name) traversed
  # the symbol table multiple times — costly on gdx files with thousands of
  # equations.
  all_syms  <- m$getSymbols()
  if (length(all_syms) == 0L) {
    nms  <- character(0); dims <- integer(0)
    text <- character(0); cls  <- character(0)
  } else {
    nms  <- vapply(all_syms, function(s) s$name, character(1))
    dims <- vapply(all_syms, function(s) as.integer(s$dimension), integer(1))
    text <- vapply(all_syms, function(s) {
      d <- s$description; if (is.null(d)) "" else as.character(d)
    }, character(1))
    cls  <- vapply(all_syms, function(s) class(s)[1], character(1))
  }
  make_df <- function(idx) data.frame(
    name = nms[idx], text = text[idx], dim = dims[idx],
    stringsAsFactors = FALSE)
  variables_df  <- make_df(cls == "Variable")
  parameters_df <- make_df(cls == "Parameter")
  sets_df       <- make_df(cls == "Set")
  equations_df  <- make_df(cls == "Equation")
  set_names <- sets_df$name
  par_names <- parameters_df$name
  var_names <- variables_df$name
  eq_names  <- equations_df$name
  ali_names <- m$listAliases()
  if (is.null(ali_names)) ali_names <- character(0)

  loaded <- new.env(parent = emptyenv())
  if (!isTRUE(lazy)) {
    for (n in c(set_names, par_names, var_names, eq_names)) {
      assign(n, TRUE, envir = loaded, inherits = FALSE)
    }
  }

  structure(list(
    filename           = filename,
    sets               = sets_df,
    parameters         = parameters_df,
    variables          = variables_df,
    equations          = equations_df,
    aliases            = if (length(ali_names))
                           data.frame(name = ali_names, stringsAsFactors = FALSE)
                         else
                           data.frame(name = character()),
    symCount           = length(all_syms) + length(ali_names),
    .container         = m,
    .records_container = rec_ct,
    .lazy              = isTRUE(lazy),
    .loaded            = loaded,
    ...
  ), class = "gdx")
}

# gamstransfer warns once per call when a GDX contains GAMS acronyms
# (which it converts to NA). The warning is informational and there is
# no remediation for it on the R side, so we filter it out while letting
# any other warning through. Used by every code path that calls
# Container$read().
.gdx_silent_read <- function(container, filename, ...) {
  withCallingHandlers(
    container$read(filename, ...),
    warning = function(w) {
      if (grepl("acronym", conditionMessage(w), ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Ensure that records for `symbols` are loaded into the dedicated records
# container. In eager mode this is a no-op (records already live in the
# metadata container, which is aliased as the records container).
.gdx_ensure_records <- function(x, symbols) {
  if (!isTRUE(x$.lazy)) return(invisible(x))
  m_meta <- x$.container
  m_rec  <- x$.records_container
  if (is.null(m_meta) || is.null(m_rec) || length(symbols) == 0L)
    return(invisible(x))
  loaded <- x$.loaded
  todo <- symbols[!vapply(symbols, function(s)
    exists(s, envir = loaded, inherits = FALSE), logical(1))]
  todo <- todo[vapply(todo, function(s) m_meta$hasSymbols(s), logical(1))]
  if (length(todo) == 0L) return(invisible(x))
  # gamstransfer rejects re-adding existing symbols; usually rec_ct is empty
  # for these, but a defensive remove costs nothing.
  already <- todo[vapply(todo, function(s) m_rec$hasSymbols(s), logical(1))]
  if (length(already)) m_rec$removeSymbols(already)
  .gdx_silent_read(m_rec, x$filename, symbols = todo)
  for (s in todo) assign(s, TRUE, envir = loaded, inherits = FALSE)
  invisible(x)
}

#' Eagerly load records for one or more symbols
#'
#' Lazy-opened gdx objects defer reading records until first access. When you
#' know up front which symbols you will use (or you want to amortize the I/O
#' cost), call \code{load_records()} to read them in a single batched
#' \code{gamstransfer::Container$read()} call.
#'
#' @param x a \code{gdx} object
#' @param symbols character vector of symbol names; \code{NULL} (default)
#'   means every symbol in the file.
#' @return \code{x} invisibly. Records are stored on the underlying container.
#' @author Laurent Drouet
#' @examples
#'  f <- tempfile(fileext = ".gdx")
#'  write.gdx(f, list(a = data.frame(i = c("x", "y"), value = 1:2),
#'                    b = data.frame(i = c("x", "y"), value = 3:4)))
#'  g <- gdx(f)
#'  load_records(g, c("a", "b"))
#'  g["a"]  # already cached, no I/O
#' @export
load_records <- function(x, symbols = NULL) {
  if (!inherits(x, "gdx")) stop("`x` must be a gdx object")
  m_meta <- x$.container
  if (is.null(m_meta)) {
    warning("gdx not loaded")
    return(invisible(x))
  }
  if (is.null(symbols)) symbols <- m_meta$listSymbols()
  .gdx_ensure_records(x, symbols)
}

#' @export
print.gdx <- function(x, ...) {
  n <- if (is.null(x$symCount)) 0L else x$symCount
  mode <- if (isTRUE(x$.lazy)) " lazy" else ""
  cat("<gdx: ", x$filename, ", ", n, " symbol",
      if (n != 1L) "s" else "", mode, ">\n", sep = "")
}

#' Extract data from a gdx
#'
#' @param x the gdx object
#' @param ... arguments passed to \code{extract.gdx}
#' @return A \code{data.frame}; see \code{\link{extract.gdx}} for the column
#'   layout and attributes.
#' @export
extract <- function(x, ...) UseMethod("extract", x)

#' @export
`[.gdx` <- function(x, ...) extract.gdx(x, ...)

# Return character vector of domain set names; "*" for the universe domain.
.gdx_domain <- function(sym) {
  d <- sym$domain
  if (length(d) == 0) return(character(0))
  vapply(d, function(x) if (is.character(x)) x else x$name, character(1))
}

# Normalize gamstransfer's record column names to the user-friendly form.
# Two cases:
#   - universe domain ("*") → gamstransfer returns `uni` / `uni_<i>`; rename
#     to V1, V2, ... for backward compatibility with the gdxrrw-era naming
#     (this is what data.frame() produces from anonymous columns).
#   - relaxed string domain (e.g. "witch13") → gamstransfer suffixes the
#     position when the same name appears at multiple positions or to
#     guarantee uniqueness (`witch13_3` at index 3). Callers expect the
#     bare set name as the column header (witchtools' region aggregation
#     looks up `df$witch13`, not `df$witch13_3`).
.gdx_rename_domains <- function(df, dom_chr) {
  if (length(dom_chr) == 0 || ncol(df) == 0) return(df)
  cols <- names(df)
  for (i in seq_along(dom_chr)) {
    if (i > length(cols)) next
    if (dom_chr[i] == "*") {
      cols[i] <- paste0("V", i)
    } else {
      cols[i] <- dom_chr[i]
    }
  }
  # If two positions share the same set name (e.g. both index columns are
  # `n`), make.unique restores the disambiguation.
  cols[seq_along(dom_chr)] <- make.unique(cols[seq_along(dom_chr)])
  names(df) <- cols
  df
}

#' Extract parameter or variable data from the gdx file
#'
#' @export
#' @author Laurent Drouet
#' @param x the gdx object
#' @param item the name of the item to extract
#' @param field the field of the variable to be extracted. Can be 'l', 'm',
#'   'lo', 'up' (level, marginal, lower, upper). Defaults to level.
#' @param addgdx if \code{TRUE}, append a \code{gdx} column with the filename.
#' @param ... ignored; for backward compatibility.
#' @return A \code{data.frame} with one character column per domain index plus
#'   a numeric \code{value} column (parameters, variables and equations); sets
#'   have no \code{value} column. A \code{gams} attribute carries the symbol's
#'   description text, and \code{addgdx = TRUE} adds a \code{gdx} column with
#'   the source filename.
#' @examples
#'  f <- tempfile(fileext = ".gdx")
#'  write.gdx(f, list(travel_cost = data.frame(city = c("paris", "lyon"),
#'                                              value = c(12, 7))))
#'  mygdx <- gdx(f)
#'  travel_cost <- mygdx["travel_cost"]
#'  travel_cost <- extract(mygdx, "travel_cost")
extract.gdx <- function(x, item, field = "l", addgdx = FALSE, ...) {
  m_meta <- x$.container
  if (is.null(m_meta)) {
    warning("gdx not loaded")
    return(NULL)
  }
  if (!m_meta$hasSymbols(item)) {
    warning("item not found")
    return(NULL)
  }
  .gdx_ensure_records(x, item)
  m <- if (is.null(x$.records_container)) m_meta else x$.records_container
  sym <- m[item]
  rec <- sym$records
  if (is.null(rec)) rec <- data.frame()

  is_var <- inherits(sym, "Variable")
  is_eq  <- inherits(sym, "Equation")
  is_par <- inherits(sym, "Parameter")
  is_set <- inherits(sym, c("Set", ".BaseAlias", "Alias", "UniverseAlias"))

  field_map <- c(l = "level", m = "marginal", lo = "lower", up = "upper")
  dim <- as.integer(sym$dimension)

  if (is_var || is_eq) {
    fcol <- field_map[field]
    if (is.na(fcol)) stop("invalid field '", field,
                          "' (must be 'l', 'm', 'lo' or 'up')")
    if (nrow(rec) == 0) {
      df <- data.frame(matrix(character(), ncol = dim, nrow = 0),
                       stringsAsFactors = FALSE)
      df$value <- numeric(0)
    } else {
      dom_cols <- names(rec)[seq_len(dim)]
      df <- rec[, c(dom_cols, fcol), drop = FALSE]
      names(df)[length(names(df))] <- "value"
    }
  } else if (is_par) {
    if (dim == 0) {
      val <- if (nrow(rec) > 0) as.numeric(rec[[1]][1]) else NA_real_
      df <- data.frame(value = val)
    } else if (nrow(rec) == 0) {
      df <- data.frame(matrix(character(), ncol = dim, nrow = 0),
                       stringsAsFactors = FALSE)
      df$value <- numeric(0)
    } else {
      df <- rec
    }
  } else if (is_set) {
    if (nrow(rec) == 0) {
      df <- data.frame(matrix(character(), ncol = dim, nrow = 0),
                       stringsAsFactors = FALSE)
    } else {
      dom_cols <- names(rec)[seq_len(dim)]
      df <- rec[, dom_cols, drop = FALSE]
    }
  } else {
    stop("Unknown symbol type: ", paste(cls, collapse = "/"))
  }

  for (j in seq_along(df)) if (is.factor(df[[j]])) df[[j]] <- as.character(df[[j]])

  df <- .gdx_rename_domains(df, .gdx_domain(sym))

  if (addgdx) {
    df$gdx <- if (nrow(df) == 0) character(0) else x$filename
  }

  text <- if (is.null(sym$description)) "" else as.character(sym$description)
  attributes(df) <- c(attributes(df), gams = text)
  df
}

#' Return the list of all items
#'
#' @param x the gdx object
#' @param ... ignored
#' @return A named \code{list} with four character vectors of symbol names:
#'   \code{variables}, \code{parameters}, \code{sets} and \code{equations}.
#' @author Laurent Drouet
#' @export
all_items <- function(x, ...) UseMethod("all_items", x)

#' @export
all_items.gdx <- function(x, ...) {
  # Prefer the cached name vectors stored on the gdx object (computed once at
  # open time) over re-querying the container; the container call iterates the
  # full symbol table on every invocation.
  if (!is.null(x$variables) && !is.null(x$parameters) &&
      !is.null(x$sets) && !is.null(x$equations)) {
    return(list(
      variables  = x$variables$name,
      parameters = x$parameters$name,
      sets       = x$sets$name,
      equations  = x$equations$name
    ))
  }
  m <- x$.container
  if (is.null(m)) {
    return(list(variables = character(0), parameters = character(0),
                sets = character(0), equations = character(0)))
  }
  list(
    variables  = m$listVariables(),
    parameters = m$listParameters(),
    sets       = m$listSets(),
    equations  = m$listEquations()
  )
}
