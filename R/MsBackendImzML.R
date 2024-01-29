#' @include hidden_aliases.R
NULL

#' @title Imaging-MS data backend for imzML files
#'
#' @name MsBackendImzML
#'
#' @author Roger Gine, Pere Rafols
#'
#' @noRd
#'
#' @exportClass MsBackendImzML
NULL

#' @importFrom S4Vectors DataFrame
#' 
#' @importClassesFrom Spectra MsBackend
#' 
#' @import Spectra
#' @import ProtGenerics
#' 
#' @useDynLib MsBackendImzML
#' 
#' @importFrom Rcpp evalCpp
#' 
#' 
setClass("MsBackendImzML",
         contains = "MsBackend",
         slots = c(spectraData = "DataFrame",
                   peaksVariables = "character"),
         prototype = prototype(spectraData = DataFrame(),
                               peaksVariables = c("mz", "intensity"),
                               readonly = TRUE,
                               version = "0.1"))

setValidity("MsBackendImzML", function(object) {
  msg <- .valid_spectra_data_required_columns(object@spectraData)
  if (length(msg))
    return(msg)
  msg <- c(
    .valid_column_datatype(object@spectraData, .SPECTRA_DATA_COLUMNS)
  )
  if (is.null(msg)) TRUE
  else msg
})

#' @rdname hidden_aliases
setMethod("show", "MsBackendImzML", function(object) {
  spd <- spectraData(object, c("msLevel", "xPixel", "yPixel"))
  cat(class(object), "with", nrow(spd), "spectra\n")
  if (nrow(spd)) {
    txt <- capture.output(print(spd))
    cat(txt[-1], sep = "\n")
    sp_cols <- spectraVariables(object)
    cat(" ...", length(sp_cols) - 3, "more variables/columns.\n")
  }
})

#' @importMethodsFrom S4Vectors $ $<-
#'
#' @importClassesFrom IRanges NumericList
#'
#' @importFrom IRanges NumericList
#' @importFrom MsCoreUtils rbindFill
#'
#' @rdname MsBackend
setMethod("backendInitialize", signature = "MsBackendImzML",
          function(object, files, ...) {
            if (missing(files) || !length(files))
              stop("Parameter 'files' is mandatory for 'MsBackendImzML'")
            if (!is.character(files))
              stop("Parameter 'files' is expected to be a character vector",
                   " with the imzML file names from where data should be",
                   " imported")
            files <- normalizePath(files, mustWork = FALSE)
            msg <- .valid_imzml_files_exist(files)
            if (length(msg))
              stop(msg)
            
            data <- do.call(rbindFill, lapply(files, .parse_imzml))
            object@spectraData <- data
            
            if("imLength" %in% colnames(data)){
              object@peaksVariables <- c("mz", "intensity", "inv_ion_mobility")
            } else {
              object@peaksVariables <- c("mz", "intensity")
            }
            
            validObject(object)     
            object
          })

#' @rdname hidden_aliases
setMethod("backendMerge", "MsBackendImzML", function(object, ...) {
  object <- unname(c(object, ...))
  not_empty <- lengths(object) > 0
  if (any(not_empty))
    res <- .combine_backend_data_frame(object[not_empty])
  else res <- object[[1L]]
  validObject(res)
  res
})

## Data accessors

#' @rdname hidden_aliases
setMethod("acquisitionNum", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "acquisitionNum")
})

#' @rdname hidden_aliases
setMethod("peaksData", "MsBackendImzML",
          function(object, columns = peaksVariables(object)) {
            na <- columns[!columns %in% peaksVariables(object)]
            if (length(na))
              stop("Peaks variable \"", na, "\" not available.")
            lst <- lapply(columns, function(z) {
              if (z %in% c("mz", "intensity", "inv_ion_mobility"))
                do.call(z, list(object))
              else object@spectraData[, z]
            })
            names(lst) <- columns
            if (all(columns %in% c("mz", "intensity", "inv_ion_mobility")))
              fun <- cbind
            else fun <- cbind.data.frame
            do.call(mapply, c(list(FUN = fun, SIMPLIFY = FALSE,
                                   USE.NAMES = FALSE), lst))
            
          })

#' @rdname hidden_aliases
setMethod("centroided", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "centroided")
})

#' @rdname hidden_aliases
#'
#' @aliases centroided<-,MsBackendImzML-method
setReplaceMethod("centroided", "MsBackendImzML", function(object, value) {
  value_len <- length(value)
  value_type <- is.logical(value)
  if (value_type && (value_len == 1L || value_len == length(object)))
    object@spectraData$centroided <- value
  else
    stop("'value' has to be a 'logical' of length 1 or ", length(object))
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("collisionEnergy", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "collisionEnergy")
})

#' @rdname hidden_aliases
setReplaceMethod("collisionEnergy", "MsBackendImzML",
                 function(object, value) {
                   if (!is.numeric(value) || length(value) != length(object))
                     stop("'value' has to be a 'numeric' of length ",
                          length(object))
                   object@spectraData$collisionEnergy <- as.numeric(value)
                   validObject(object)
                   object
                 })

#' @rdname hidden_aliases
setMethod("dataOrigin", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "dataOrigin")
})

#' @rdname hidden_aliases
setReplaceMethod("dataOrigin", "MsBackendImzML", function(object, value) {
  if (!is.character(value) || length(value) != length(object))
    stop("'value' has to be a 'character' of length ", length(object))
  object@spectraData$dataOrigin <- as.character(value)
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("dataStorage", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "dataStorage")
})

#' @rdname hidden_aliases
setReplaceMethod("dataStorage", "MsBackendImzML", function(object, value) {
  if (!is.character(value) || length(value) != length(object))
    stop("'value' has to be a 'character' of length ", length(object))
  object@spectraData$dataStorage <- as.character(value)
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("intensity", "MsBackendImzML", function(object) {
  mapply(imzMLBinReadGeneric,
         ibdFname = .get_column(object@spectraData, "dataStorage"),
         N = .get_column(object@spectraData, "intLength"),
         offset = .get_column(object@spectraData, "intOffset"),
         dataTypeString = .get_column(object@spectraData, "dataTypeInt"),
         MoreArgs = list(NPixels = 1, read_mz = FALSE, continuous = FALSE),
         SIMPLIFY = FALSE)
})


#' @rdname hidden_aliases
setMethod("isEmpty", "MsBackendImzML", function(x) {
  lengths(intensity(x)) == 0
})

#' @rdname hidden_aliases
setMethod("isolationWindowLowerMz", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "isolationWindowLowerMz")
})

#' @rdname hidden_aliases
setReplaceMethod("isolationWindowLowerMz", "MsBackendImzML",
                 function(object, value) {
                   if (!is.numeric(value) || length(value) != length(object))
                     stop("'value' has to be a 'numeric' of length ",
                          length(object))
                   object@spectraData$isolationWindowLowerMz <-
                     as.numeric(value)
                   validObject(object)
                   object
                 })

#' @rdname hidden_aliases
setMethod("isolationWindowTargetMz", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "isolationWindowTargetMz")
})

#' @rdname hidden_aliases
setReplaceMethod("isolationWindowTargetMz", "MsBackendImzML",
                 function(object, value) {
                   if (!is.numeric(value) || length(value) != length(object))
                     stop("'value' has to be a 'numeric' of length ",
                          length(object))
                   object@spectraData$isolationWindowTargetMz <-
                     as.numeric(value)
                   validObject(object)
                   object
                 })

#' @rdname hidden_aliases
setMethod("isolationWindowUpperMz", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "isolationWindowUpperMz")
})

#' @rdname hidden_aliases
setReplaceMethod("isolationWindowUpperMz", "MsBackendImzML",
                 function(object, value) {
                   if (!is.numeric(value) || length(value) != length(object))
                     stop("'value' has to be a 'numeric' of length ",
                          length(object))
                   object@spectraData$isolationWindowUpperMz <-
                     as.numeric(value)
                   validObject(object)
                   object
                 })

#' @rdname hidden_aliases
setMethod("length", "MsBackendImzML", function(x) {
  nrow(x@spectraData)
})

#' @rdname hidden_aliases
setMethod("lengths", "MsBackendImzML", function(x, use.names = FALSE) {
  lengths(mz(x))
})

#' @rdname hidden_aliases
setMethod("msLevel", "MsBackendImzML", function(object, ...) {
  .get_column(object@spectraData, "msLevel")
})

#' @rdname hidden_aliases
setReplaceMethod("msLevel", "MsBackendImzML", function(object, value) {
  if (!is.integer(value) && is.numeric(value))
    value <- as.integer(value)
  if (!is.integer(value) || length(value) != length(object))
    stop("'value' has to be an 'integer' of length ", length(object))
  object@spectraData$msLevel <- value
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("mz", "MsBackendImzML", function(object) {
  mapply(imzMLBinReadGeneric,
         ibdFname = .get_column(object@spectraData, "dataStorage"),
         N = .get_column(object@spectraData, "mzLength"),
         offset = .get_column(object@spectraData, "mzOffset"),
         dataTypeString = .get_column(object@spectraData, "dataTypeMz"),
         MoreArgs = list(NPixels = 1, read_mz = FALSE, continuous = FALSE),
         SIMPLIFY = FALSE)
})

setMethod("inv_ion_mobility", "MsBackendImzML", function(object) {
  mapply(imzMLBinReadGeneric,
         ibdFname = .get_column(object@spectraData, "dataStorage"),
         N = .get_column(object@spectraData, "imLength"),
         offset = .get_column(object@spectraData, "imOffset"),
         dataTypeString = .get_column(object@spectraData, "dataTypeIm"),
         MoreArgs = list(NPixels = 1, read_mz = FALSE, continuous = FALSE),
         SIMPLIFY = FALSE)
})


#' @rdname hidden_aliases
setMethod("polarity", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "polarity")
})

#' @rdname hidden_aliases
setReplaceMethod("polarity", "MsBackendImzML", function(object, value) {
  value_len <- length(value)
  value_type <- is.numeric(value)
  if (value_type && (value_len == 1L || value_len == length(object)))
    object@spectraData$polarity <- as.integer(value)
  else
    stop("'value' has to be an 'integer' of length 1 or ", length(object))
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("precScanNum", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "precScanNum")
})

#' @rdname hidden_aliases
setMethod("precursorCharge", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "precursorCharge")
})

#' @rdname hidden_aliases
setMethod("precursorIntensity", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "precursorIntensity")
})

#' @rdname hidden_aliases
setMethod("precursorMz", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "precursorMz")
})


#' @rdname hidden_aliases
setMethod("peaksVariables", "MsBackendImzML", function(object) {
  union(c("mz", "intensity"), .peaks_variables(object))
})

#' @rdname hidden_aliases
setMethod("rtime", "MsBackendImzML", function(object) {
  stop("Retention time does not apply to imaging-MS data")
})


#' @rdname hidden_aliases
setMethod("scanIndex", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "scanIndex")
})

#' @rdname hidden_aliases
setMethod("selectSpectraVariables", "MsBackendImzML",
          function(object, spectraVariables = spectraVariables(object)) {
            if (!all(spectraVariables %in% spectraVariables(object)))
              stop("Spectra variables ",
                   paste(spectraVariables[!(spectraVariables %in%
                                              spectraVariables(object))],
                         collapse = ", "), " not available")
            keep <- spectraVariables[spectraVariables %in%
                                       colnames(object@spectraData)]
            if (length(keep))
              object@spectraData <- object@spectraData[, keep,
                                                       drop = FALSE]
            msg <- .valid_spectra_data_required_columns(object@spectraData)
            if (length(msg))
              stop(msg)
            object@peaksVariables <- intersect(object@peaksVariables,
                                               colnames(object@spectraData))
            validObject(object)
            object
          })

#' @rdname hidden_aliases
setMethod("smoothed", "MsBackendImzML", function(object) {
  .get_column(object@spectraData, "smoothed")
})

#' @rdname hidden_aliases
#'
#' @aliases smoothed<-,MsBackendImzML-method
setReplaceMethod("smoothed", "MsBackendImzML", function(object, value) {
  value_len <- length(value)
  value_type <- is.logical(value)
  if (value_type && (value_len == 1L || value_len == length(object)))
    object@spectraData$smoothed <- value
  else
    stop("'value' has to be a 'logical' of length 1 or ", length(object))
  validObject(object)
  object
})

#' @rdname hidden_aliases
#'
#' @importFrom methods as
#'
#' @importFrom S4Vectors SimpleList
#'
#' @importMethodsFrom S4Vectors lapply
setMethod("spectraData", "MsBackendImzML",
          function(object, columns = spectraVariables(object)) {
            df_columns <- intersect(columns,colnames(object@spectraData))
            res <- object@spectraData[, df_columns, drop = FALSE]
            other_columns <- setdiff(columns,colnames(object@spectraData))
            if (length(other_columns)) {
              other_res <- lapply(other_columns, .get_column,
                                  x = object@spectraData)
              names(other_res) <- other_columns
              is_mz_int <- names(other_res) %in% c("mz", "intensity")
              if (!all(is_mz_int))
                res <- cbind(res, as(other_res[!is_mz_int], "DataFrame"))
              if (any(names(other_res) == "mz"))
                res$mz <- if (length(other_res$mz)) other_res$mz
              else NumericList(compress = FALSE)
              if (any(names(other_res) == "intensity"))
                res$intensity <- if (length(other_res$intensity))
                  other_res$intensity
              else NumericList(compress = FALSE)
            }
            res[, columns, drop = FALSE]
          })

#' @rdname hidden_aliases
setReplaceMethod("spectraData", "MsBackendImzML", function(object, value) {
  if (inherits(value, "DataFrame")) {
    if (length(object) && nrow(value) != length(object))
      stop("'value' has to be a 'DataFrame' with ",
           length(object), " rows.")
    if (!is(value$mz, "NumericList"))
      value$mz <- NumericList(value$mz, compress = FALSE)
    if (!is(value$intensity, "NumericList"))
      value$intensity <- NumericList(value$intensity, compress = FALSE)
    if (is.null(value$dataStorage))
      value$dataStorage <- "<memory>"
  } else {
    if (length(value) == 1)
      value <- rep(value, length(object))
    if (length(value) != length(object))
      stop("length of 'value' has to be ", length(object))
  }
  object@spectraData <- value
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("spectraNames", "MsBackendImzML", function(object) {
  rownames(object@spectraData)
})

#' @rdname hidden_aliases
setReplaceMethod("spectraNames", "MsBackendImzML", function(object, value) {
  rownames(object@spectraData) <- value
  validObject(object)
  object
})

#' @rdname hidden_aliases
setMethod("spectraVariables", "MsBackendImzML", function(object) {
  unique(c(names(.SPECTRA_DATA_COLUMNS), colnames(object@spectraData)))
})

#' @rdname hidden_aliases
setMethod("tic", "MsBackendImzML", function(object, initial = TRUE) {
  if (initial) {
    if (any(colnames(object@spectraData) == "totIonCurrent"))
      .get_column(object@spectraData, "totIonCurrent")
    else rep(NA_real_, times = length(object))
  } else vapply1d(intensity(object), sum, na.rm = TRUE)
})

#' @rdname hidden_aliases
setMethod("$", "MsBackendImzML", function(x, name) {
  if (!any(spectraVariables(x) == name))
    stop("spectra variable '", name, "' not available")
  spectraData(x, name)[, 1]
})

#' @rdname hidden_aliases
setReplaceMethod("$", "MsBackendImzML", function(x, name, value) {
  if (is.list(value) && any(c("mz", "intensity") == name))
    value <- NumericList(value, compress = FALSE)
  x@spectraData[[name]] <- value
  validObject(x)
  x
})

#### ---------------------------------------------------------------------------
##
##                      FILTERING AND SUBSETTING
##
#### ---------------------------------------------------------------------------

#' @importMethodsFrom S4Vectors [
#'
#' @importFrom MsCoreUtils i2index
#'
#' @rdname hidden_aliases
setMethod("[", "MsBackendImzML", function(x, i, j, ..., drop = FALSE) {
  .subset_backend_data_frame(x, i)
})

#' @rdname hidden_aliases
setMethod("split", "MsBackendImzML", function(x, f, drop = FALSE, ...) {
  if (!is.factor(f))
    f <- as.factor(f)
  lapply(split(seq_along(x), f, ...), function(i) x[i, ])
})

#' @rdname hidden_aliases
setMethod("filterAcquisitionNum", "MsBackendImzML",
          function(object, n = integer(), dataStorage = character(),
                   dataOrigin = character()) {
            if (!length(n) || !length(object)) return(object)
            if (!is.integer(n)) stop("'n' has to be an integer representing the ",
                                     "acquisition number(s) for sub-setting")
            sel_file <- .sel_file(object, dataStorage, dataOrigin)
            sel_acq <- acquisitionNum(object) %in% n & sel_file
            object[sel_acq | !sel_file]
          })