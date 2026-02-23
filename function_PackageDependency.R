## === Verify and Install Required Packages  === ###
# A utility function to verify what packages require installation for a script to successfully run and installs the missing packages.
# A character vector with package names serves as the input and function proceeds to check for the name of the package within the system's R path package list
# Functionality is currently limited to Bioconductor and CRAN repositories.

PackageDependency = function (package_list) {
  package_list = c(package_list, "BiocManager")
  require_install = c() # output vector of packages needing to be installed
  #flow control to validated or identify needed packages for script functionality from input character vector
  for (p in package_list) {
    if (p %in% rownames(installed.packages())) { #package already installed
      print(paste("Package", p, "installed on system...")) 
    } else { # package missing
      require_install = c(require_install, p)
    }
  }
  print("Verification Check Complete.")
  if (length(require_install) < 1) { #empty vector means all packages are installed
    print("All required packages are installed on system.")
  } else { # otherwise, at least one package is needing installation
    print("PACKAGES REQUIRING INSTALLATION:")
    print(require_install)
    if ("BiocManager" %in% require_install) {
      install.packages("BiocManager")
      library("BiocManager")
    } else {
      library("BiocManager")
    }
    # flow control to install remaining packages needed through CRAN or Bioconductor
    for (p in require_install) {
      # uses tryCatch to handle warning/error when package is not located in CRAN and only found in Bioconductor
      tryCatch(
        expr = {
          install.packages(p)
        },
        warning = function(w) {
          BiocManager::install(p)
        },
        error = function(e) {
          BiocManager::install(p)
        },
        finally = {
          print("All required packages are installed on system.")
        }
      )
    }
  }
}