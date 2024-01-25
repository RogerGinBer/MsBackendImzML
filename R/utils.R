.valid_imzml_files_exist <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) && !all(file.exists(x))){
    return(paste0("File(s) ", paste(x[!file.exists(x)], collapse = ", "),
                  " not found"))
  } else {
    if (!all(grepl("imzML$", x))){
      return(paste0("File(s) ", paste(x[!grepl("imzML$", x)], collapse = ", "),
                  " are not in imzML format.\nPlease provide only imzML files."))
    }
    if (!all(file.exists(gsub("imzML$", "ibd", x)))){
      return(paste0("File(s) ", paste(x[!file.exists(gsub("imzML$", "ibd", x))], collapse = ", "),
                    " do not have a corresponding ibd file.\nPlease provide it in the same folder path"))
    }
  }
  NULL
}
