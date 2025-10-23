<!-- badges: start -->

[![ards version](https://www.r-pkg.org/badges/version/ards)](https://cran.r-project.org/package=ards)
[![ards lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://cran.r-project.org/package=ards)
[![ards downloads](https://cranlogs.r-pkg.org/badges/ards)](https://cran.r-project.org/package=ards)
[![ards total downloads](https://cranlogs.r-pkg.org/badges/grand-total/ards)](https://cran.r-project.org/package=ards)
[![R-CMD-check](https://github.com/dbosak01/ards/workflows/R-CMD-check/badge.svg)](https://github.com/dbosak01/ards/actions)
<!-- badges: end -->

# Introduction to **ards**
<img src="man/images/ards.png" align="left" height="138px" 
     style="margin-right:10px;height:138px" alt = "ards package logo"/>

An Analysis Results Dataset (ARDS) is commonly used in the pharma-biotech industry
to capture the results of an analysis in a tabular data structure.  The **ards**
package helps create the ARDS.  **ards** functions can be called from inside 
a report program or a data preparation program.  The functions use a 
"bucketing" approach, whereby data can be added to the ARDS in multiple 
calls and from multiple intermediate data sets. 


### Installation

The **ards** package can be installed from the console.  Simply run 
the following command: 

    install.packages("ards")
    
Or if you want the latest development version, you can install it directly
from github:

    devtools::install_github("https://github.com/dbosak01/ards")


Then put the following line at the top of your program or script:

    library(ards)

The **ards** package will give you access to a number of functions
to help create an Analysis Results Dataset (ARDS). 
For examples and usage information, visit the **ards** documentation
site [here](https://ards.r-sassy.org/articles/ards.html).

### Getting Help

If you need help, the first place 
to turn to is the [ards](https://ards.r-sassy.org/) web site. The web site
has full documentation on all **ards** functions.

If you want to look at the code for the **ards** package, visit the
github page [here](https://github.com/dbosak01/ards/).

If you encounter a bug or have a feature request, please submit an issue 
[here](https://github.com/dbosak01/ards/issues/).

### See Also

The **sassy** meta-package includes several packages that help make R
easier for everyone.  You can read more about the **sassy** package
[here](https://r-sassy.org/).
