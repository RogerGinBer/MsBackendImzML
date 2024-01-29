# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(Spectra)
library(MsBackendImzML)

imzml_path <- system.file("extdata/test_image.imzML", package = "MsBackendImzML")
if(!file.exists(imzml_path)) stop("imzML test file not found")
ibd_path <- system.file("extdata/test_image.ibd", package = "MsBackendImzML")
if(!file.exists(imzml_path)) stop("ibd test file not found")

be <- backendInitialize(MsBackendImzML(), files = imzml_path)

test_check("MsBackendImzML")

## Check generic MsBackend test suite
test_suite <- system.file("test_backends", "test_MsBackend",
                          package = "Spectra")
test_dir(test_suite, stop_on_failure = TRUE)
