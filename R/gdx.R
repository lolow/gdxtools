#' GDX file
#'
#' Constructs a \code{gdx} object backed by a \code{gamstransfer::Container}.
#' The container loads the GDX once and is reused on subsequent
#' \code{extract}/\code{[} calls.
#'
#' @param filename filename of the gdx file
#' @param ... extra fields stored on the resulting object
#' @author Laurent Drouet
#' @examples
#'  \dontrun{
#'    mygdx <- gdx('results.gdx')
#'  }
#' @export
gdx <- function(filename, ...) {
  if (!file.exists(filename)) {
    warning(paste(filename, "does not exist!"))
    return(structure(list(filename = filename, ...), class = "gdx"))
  }
  # gamstransfer warns once per call when a GDX contains GAMS acronyms
  # (which it converts to NA). The warning is informational and there is
  # no remediation for it on the R side, so we filter it out here while
  # letting any other warning through.
  m <- withCallingHandlers(
    gamstransfer::Container$new(filename),
    warning = function(w) {
      if (grepl("acronym", conditionMessage(w), ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  describe <- function(names_vec) {
    if (length(names_vec) == 0) {
      return(data.frame(name = character(), text = character(),
                        dim = integer(), stringsAsFactors = FALSE))
    }
    text <- vapply(names_vec, function(n) {
      d <- m[n]$description
      if (is.null(d)) "" else as.character(d)
    }, character(1))
    dims <- vapply(names_vec, function(n) as.integer(m[n]$dimension), integer(1))
    data.frame(name = names_vec, text = text, dim = dims,
               stringsAsFactors = FALSE)
  }

  set_names <- m$listSets()
  par_names <- m$listParameters()
  var_names <- m$listVariables()
  eq_names  <- m$listEquations()
  ali_names <- m$listAliases()
  if (is.null(ali_names)) ali_names <- character(0)

  structure(list(
    filename   = filename,
    sets       = describe(set_names),
    parameters = describe(par_names),
    variables  = describe(var_names),
    equations  = describe(eq_names),
    aliases    = if (length(ali_names))
                   data.frame(name = ali_names, stringsAsFactors = FALSE)
                 else
                   data.frame(name = character()),
    symCount   = length(m$listSymbols()),
    .container = m,
    ...
  ), class = "gdx")
}

#' @export
print.gdx <- function(x, ...) {
  n <- if (is.null(x$symCount)) 0L else x$symCount
  cat("<gdx: ", x$filename, ", ", n, " symbol",
      if (n != 1L) "s" else "", ">\n", sep = "")
}

#' Extract data from a gdx
#'
#' @param x the gdx object
#' @param ... arguments passed to \code{extract.gdx}
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
#' @examples
#'  \dontrun{
#'    mygdx <- gdx('results.gdx')
#'    travel_cost <- mygdx["travel_cost"]
#'    travel_cost <- extract(mygdx, "travel_cost")
#'  }
extract.gdx <- function(x, item, field = "l", addgdx = FALSE, ...) {
  m <- x$.container
  if (is.null(m)) {
    warning("gdx not loaded")
    return(NULL)
  }
  if (!m$hasSymbols(item)) {
    warning("item not found")
    return(NULL)
  }
  sym <- m[item]
  rec <- sym$records
  if (is.null(rec)) rec <- data.frame()

  cls <- class(sym)
  is_var <- "Variable" %in% cls
  is_eq  <- "Equation" %in% cls
  is_par <- "Parameter" %in% cls
  is_set <- ("Set" %in% cls) || (".BaseAlias" %in% cls) ||
            ("Alias" %in% cls) || ("UniverseAlias" %in% cls)

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
#' @author Laurent Drouet
#' @export
all_items <- function(x, ...) UseMethod("all_items", x)

#' @export
all_items.gdx <- function(x, ...) {
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
