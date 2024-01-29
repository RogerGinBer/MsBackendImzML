# MsBackendImzML

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Spectra backend supporting imzML/ibd imaging-MS data files using a self-contained C++
library derived from [rMSI2](https://github.com/prafols/rMSI2). 

Main features:

- This backend is processing-agnostic: imzML files can be read at any point of the pre-processing  (ie. raw/profile data, aligned, centroided...).
- In case of imaging-TIMS data, the backend extracts the ion mobility information (offsets and lengths) from the imzML. In the case of Bruker data, the use of [TIMSCONVERT](https://github.com/gtluu/timsconvert) to convert to imzML/ibd or [MsBackendTimsTof](https://github.com/rformassspectrometry/MsBackendTimsTof) to access the raw data is suggested.

# Contributions

Contributions are highly welcome and should follow the [contribution
guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).
Also, please check the coding style guidelines in the [RforMassSpectrometry
vignette](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html).
