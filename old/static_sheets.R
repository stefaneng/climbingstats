library("googlesheets")
library("dplyr")
library("ggplot2")
library("stringr")

sheet <- gs_title("Climbing / Workout")
ws <- sheet %>% gs_read(ws = "Outdoor Climbs")

# Convert date to date object
ws$date <- as.Date(ws$date, "%m/%d/%Y")

# Visualization 1: Rock Routes by Grade
isRoute <- function(x) {
  # Should be a 'type' of the route
  x_lower <- tolower(x)
  x_lower == 'lead' | x_lower == 'sport' | x_lower == 'trad' | x_lower == 'toprope'
}

# Filter only routes and remove NAs
routes <- ws %>% filter(isRoute(type))

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
boulders <- ws %>% filter(tolower(type) == 'boulder')

# Remove +/- from other climbs
renameBoulderGrade <- function(raw_grades) {
  renameOneBoulderGrade <- function(raw_grade) {
    # Find the VB grades graded as routes (5.1 - 5.9+)
    pattern <- "5\\.([0-9]{1,2})[+-]?"
    m <- str_match(raw_grade, pattern)
    grade <- m[2]
    # Grade is on YDS scale
    if (! is.na(grade)) {
      return("VB")
    } 
    else if (raw_grade == "V-easy") {
      return("VB")
    }
    else {
      # Matches hueco grades
      vPattern <- "V([0-9]{1,2})[+-]?[0-9]?"
      m <- str_match(raw_grade, vPattern)
      
      grade <- m[2]
      if (! is.na(grade)) {
        # Rounds grades such that V1-2 returns V1, V2+ return V2, V3- returns V3.
        return(paste("V", grade, sep=""))
      }
      
      return(raw_grade)
    }
  }
  
  sapply(raw_grades, renameOneBoulderGrade, USE.NAMES = FALSE)
}

boulders <- boulders %>% mutate(simple_grade = renameBoulderGrade(grade))
boulders$simple_grade <- factor(boulders$simple_grade, levels <- c("VB", "V0", "V1", "V2", "V3", "V4", "V5"))
total_boulders <- sum(boulders$`times climbed`)
# TODO: Separate by repeat/first time
ggplot(boulders, aes(x = simple_grade, y=`times climbed`)) + geom_bar(stat = "summary", fun.y=sum) + xlab("Grade") + ylab("Times Climbed") + ggtitle(paste("Boulders by Grade (", total_boulders, " total)", sep=""))

# Viz 3: Pitches by Month
ws <- ws %>% mutate(month_year = format(date, "%m/%y"), year = format(date, "%Y"))
# Reorder by date but plot by month/date so it sorts correctly
# Add fill = type to get a breakdown of style: boulder/trad/sport
# Possible to skip a labels: http://stackoverflow.com/questions/5407367/remove-a-few-text-marks-from-tick-marks-in-ggplot-bar-plot
ggplot(ws, aes(x=reorder(month_year, date), y=`times climbed`)) + geom_bar(stat = "summary", fun.y=sum) + xlab("Month") + ylab("Pitches") + ggtitle("Pitches per Month")

# Viz 4: Pitches by Year
ggplot(ws, aes(x=reorder(year, date), y=`times climbed`)) + geom_bar(stat = "summary", fun.y=sum) + xlab("Year") + ylab("Pitches") + ggtitle("Pitches per Year")
