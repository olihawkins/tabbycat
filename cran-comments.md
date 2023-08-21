# CRAN comments

This is a new version of an existing package. It updates the package from version 0.17.0 to 0.18.0.

## Changes in version 0.18.0

None of the changes in version 0.18.0 alter the functionality of the package. They simply remove deprecated behaviour. In particular:

* The file R/tabbycat.R has been replaced with the file R/tabbycat-package.R. 
* The @docType annotation has been removed from tabbycat-package.R.
* Brief package-level documentation has been created by documenting the _PACKAGE sentinel in tabbycat-package.R.
* Package functions no longer use the .data variable in tidy selections in order to comply with its deprecation in tidyselect.

## R CMD check results

There were 0 errors, 0 warnings, 0 notes in all test environments.

## Test environments

* Local R installation
    * MacOS 13.4.1, R 4.3.1

* GitHub actions
    * macOS Monterey 12.6.7, R version 4.3.1
    * Windows Server 2022 x64 (build 20348), R version 4.3.1
    * Ubuntu 22.04.3, LTS-R version 4.4.0
    * Ubuntu 22.04.3, LTS-R version 4.3.1
    * Ubuntu 22.04.3, LTS-R version 4.2.3
