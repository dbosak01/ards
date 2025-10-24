# ards 0.1.3

* Added `restore_ards()` function to convert back to wide format.
* Removed any factors during `add_ards()` to avoid problems stacking 
data from different analysis variables.
* Added "factors" attribute to ARDS dataset to capture which columns
were factors when added.  This information will be used by `restore_ards()`
to re-apply the factors on the transposed data.
* Added examples 1 and 2 for using **ards** package in parallel or serial.
* Added quite a few new test cases.

# ards 0.1.2

* Bug fix for byvars parameter in `add_ards()`.
* Documentation fixes.

# ards 0.1.1

* Initial version.
* Add `init_ards()`, `add_ards()`, and `get_ards()` functions.
* Wrote documentation and created pkgdown site.
* Added a `NEWS.md` file to track changes to the package.
