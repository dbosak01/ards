% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ards.R
\name{get_ards}
\alias{get_ards}
\title{Returns the current Analysis Results Dataset}
\usage{
get_ards()
}
\value{
A data frame of the current analysis results.
}
\description{
The \code{get_ards} function returns the current state
of the Analysis Results Dataset (ARDS) as an R data frame. 
This data frame may be saved to disk, saved in
a database, or examined from code.  The function takes no parameters.
}
\examples{
library(ards)
library(dplyr)

# Initialize the ARDS
# - These values will be common through the dataset
init_ards(studyid = "IRIS",
          tableid = "01", adsns = "iris",
          population = "all flowers",
          time = "1973")

# Perform analysis on Petal.Length
# - Using Species as a by-group
analdf1 <- iris |> 
  select(Petal.Length, Species) |> 
  group_by(Species) |> 
  summarize(n = n(),
            mean = mean(Petal.Length),
            std = sd(Petal.Length),
            min = min(Petal.Length),
            max = max(Petal.Length)) |> 
  add_ards(statvars = c("n", "mean", "std", "min", "max"),
           statdesc = c("Count", "Mean", "STD", "Minimum", "Maximum"),
           anal_var = "Petal.Length", trtvar = "Species")
           
# Perform analysis on Petal.Width
# - Using Species as a by-group
analdf2 <- iris |> 
  select(Petal.Width, Species) |> 
  group_by(Species) |> 
  summarize(n = n(),
            mean = mean(Petal.Width),
            std = sd(Petal.Width),
            min = min(Petal.Width),
            max = max(Petal.Width)) |> 
  add_ards(statvars = c("n", "mean", "std", "min", "max"),
           statdesc = c("Count", "Mean", "STD", "Minimum", "Maximum"),
           anal_var = "Petal.Width", trtvar = "Species")

# Get the ARDS
ards <- get_ards() 

# Uncomment to view ards
# View(ards)
}
\seealso{
Other ards: 
\code{\link{add_ards}()},
\code{\link{init_ards}()}
}
\concept{ards}
