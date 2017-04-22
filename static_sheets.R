library("googlesheets")
library("dplyr")
library("ggplot2")
library("stringr")

sheet <- gs_title("Commonplace")
ws <- sheet %>% gs_read(ws = "Outdoor Climbs")

# Visualization 1: Rock Routes by Grade
isRoute <- function(x) {
  # Should be a 'type' of the route
  x_lower <- tolower(x)
  x_lower == 'lead' | x_lower == 'sport' | x_lower == 'trad' | x_lower == 'toprope'
}

# Filter only routes and remove NAs
routes <- ws %>% filter(isRoute(type))

# Convert date to date object
routes$date <- as.Date(routes$date, "%m/%d/%Y")

# YDS grades only for now
yds_routes <- routes %>% filter(grepl("5\\.[0-9].*", grade))

# Takes the YDS and converts into MountainProject bins
renameGrade <- function(raw_grades) {
  renameOneGrade <- function(raw_grade) {
    pattern <- "5\\.([0-9]{1,2})([abcd]?)([-+]?)"
    # Grade/letter
    m <- str_match(raw_grade, pattern)
    
    grade <- as.integer(m[2])
    letter <- m[3]
    sym <- m[4]
    if (! is.na(grade)) {
      if (grade <= 6) {
        return("<=5.6")
      } else if (grade < 10) {
        # Don't care about +/- if grade is less than 5.10..
        sym <- ""    
      } else if (! is.na(letter)) {
        if (letter == "a") {
          sym <- "-"
        } else if (letter == "b" || letter == "c") {
          sym <- ""
        } else if (letter == "d") {
          sym <- "+"
        }
      }
      
      return(paste("5.", grade, sym, sep=""))
    }
  }
  
  sapply(raw_grades, renameOneGrade, USE.NAMES = FALSE)
}

# Add the "Simple grade" as a column
yds_routes <- yds_routes %>% mutate(simple_grade = renameGrade(grade))

# Sort by descending # of times climbed. Should really sort by grade order
ggplot(yds_routes, aes(x=reorder(simple_grade, -`times climbed`, sum), y=`times climbed`)) + geom_bar(stat = "summary", fun.y=sum) + xlab("Grade") + ylab("Count") + ggtitle("Rock Routes by Grade")

# Viz 2: Boulder Routes by Grade

# Viz 3: Pitches by Month

# Viz 4: Pitches by Year