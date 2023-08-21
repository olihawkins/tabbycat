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
    * Mac OS X 10.15.7 19H1519, R 4.1.2
    * Windows Server 2019 10.0.17763, R 4.1.2
    * Ubuntu 20.04.3 LTS, R-devel 2021-11-16 r81199
    * Ubuntu 20.04.3 LTS, R 4.1.2
    * Ubuntu 20.04.3 LTS, R 4.0.5
