library(dplyr)
library(ards)

# Initialize the ARDS
# - These values will be repeated on all rows in the ARDS dataset
init_ards(studyid = "MTCARS",
          tableid = "01", adsns = "mtcars",
          population = "all cars",
          time = "1973")

# Perform analysis on MPG
# - Using cylinders as a by group
analdf <- mtcars |> 
  select(cyl, mpg) |> 
  group_by(cyl) |> 
  summarize(n = n(),
            mean = mean(mpg),
            std = sd(mpg),
            min = min(mpg),
            max = max(mpg))

# View analysis data
analdf
#     cyl     n  mean   std   min   max
#   <dbl> <int> <dbl> <dbl> <dbl> <dbl>
# 1     4    11  26.7  4.51  21.4  33.9
# 2     6     7  19.7  1.45  17.8  21.4
# 3     8    14  15.1  2.56  10.4  19.2

# Add analysis data to ARDS
# - These values will be unique for each row in the ARDS dataset
add_ards(analdf, 
         statvars = c("n", "mean", "std", "min", "max"),
         anal_var = "mpg", trtvar = "cyl")


# Get the ARDS
# - Remove by-variables to make the ARDS dataset easier to read
ards <- get_ards() |> select(-starts_with("by"))

# Uncomment to view ards
# View(ards)


# Restore to wide format
res <- restore_ards(ards)

# View results
res
# $mpg
#   cyl anal_var  n     mean      std  min  max
# 1   4      mpg 11 26.66364 4.509828 21.4 33.9
# 2   6      mpg  7 19.74286 1.453567 17.8 21.4
# 3   8      mpg 14 15.10000 2.560048 10.4 19.2

