#' Read a meta-analysis data file
#'
#' Reads CSV, TSV, XLSX, or XLS files without changing the source file.
#'
#' @param path File path.
#' @param sheet Excel sheet name or index.
#' @param encoding Encoding used for delimited files. `"auto"` tries common
#'   UTF-8 and Chinese Windows encodings.
#' @param decimal Decimal mark for delimited files, normally `"."` or `","`.
#' @param ... Additional arguments passed to the reader.
#' @return A data frame.
read_meta_data <- function(path, sheet = 1, encoding = "auto",
                           decimal = ".", ...) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be one non-empty file path.", call. = FALSE)
  }
  if (!file.exists(path)) stop("Data file does not exist: ", path, call. = FALSE)
  if (!is.character(decimal) || length(decimal) != 1 ||
      !decimal %in% c(".", ",")) {
    stop("'decimal' must be '.' or ','.", call. = FALSE)
  }
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Additional reader arguments in '...' must be named.", call. = FALSE)
  }
  call_reader <- function(fun, defaults) {
    do.call(fun, utils::modifyList(defaults, dots))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading Excel files requires the optional package 'readxl'.", call. = FALSE)
    }
    out <- as.data.frame(call_reader(
      readxl::read_excel,
      list(path = path, sheet = sheet, .name_repair = "minimal")
    ), check.names = FALSE)
    out <- .repair_imported_names(out)
    .validate_column_names(out)
    return(out)
  }
  if (ext == "csv") {
    encodings <- if (identical(encoding, "auto")) {
      c("UTF-8-BOM", "UTF-8", "GB18030", "GBK")
    } else encoding
    separators <- c(",", ";", "\t")
    errors <- character()
    for (enc in encodings) {
      for (separator in separators) {
        attempt <- tryCatch(
          suppressWarnings(call_reader(utils::read.table, list(
            file = path, header = TRUE, sep = separator, quote = "\"",
            comment.char = "", dec = decimal, fileEncoding = enc,
            check.names = FALSE, stringsAsFactors = FALSE,
            colClasses = "character"
          ))),
          error = function(e) e
        )
        if (!inherits(attempt, "error") && ncol(attempt) > 1 &&
            !anyNA(names(attempt)) && all(nzchar(trimws(names(attempt))))) {
          attempt <- .repair_imported_names(attempt)
          .validate_column_names(attempt)
          attr(attempt, "meta3_encoding") <- enc
          attr(attempt, "meta3_separator") <- separator
          return(attempt)
        }
        sep_label <- if (separator == "\t") "tab" else separator
        errors <- c(errors, paste0(enc, "/", sep_label, ": ",
          if (inherits(attempt, "error")) conditionMessage(attempt) else "invalid header/one column"))
      }
    }
    stop("Could not read the CSV with the requested/common encodings. Attempts: ",
         paste(errors, collapse = " | "), call. = FALSE)
  }
  if (ext %in% c("tsv", "txt")) {
    encodings <- if (identical(encoding, "auto")) {
      c("UTF-8-BOM", "UTF-8", "GB18030", "GBK")
    } else encoding
    for (enc in encodings) {
      out <- tryCatch(
        suppressWarnings(call_reader(utils::read.delim, list(
          file = path, fileEncoding = enc, check.names = FALSE,
          stringsAsFactors = FALSE, dec = decimal, colClasses = "character"
        ))),
        error = function(e) e
      )
      if (!inherits(out, "error") && ncol(out) > 1) {
        out <- .repair_imported_names(out)
        .validate_column_names(out)
        attr(out, "meta3_encoding") <- enc
        attr(out, "meta3_separator") <- "\t"
        return(out)
      }
    }
    stop("Could not read the TSV/TXT with the requested/common encodings.", call. = FALSE)
  }
  stop("Unsupported file type: ", ext, call. = FALSE)
}
