#' ---
#' title: "Sleep Study"
#' date: "`r Sys.Date()`"
#' author: "Statistical Case Studies"
#' output: pdf_document
#' ---
#' 
#' 
#' # Sleep Study Graphical Exploration Challenge
#'
#' - See `SleepStudy.html` for a description of the research problem.
#' - See `SleepData/` for 6 days of data, each day including two data
#'   files plus a README file.  There is a zip file of the data, too.
#'

days <- dir("SleepData")

#
# Read in one day of data for starters...
#

thisday <- days[2]
files <- dir(file.path("SleepData", thisday),
             full.names = TRUE, pattern = "^[^R]")

x <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
names(x) <- "angle"

y <- read.csv(files[2], as.is = TRUE)
names(y)[2] <- "angle"

# make index variable to x
x$index <- 1:nrow(x)

#'
#' ## Challenge 1
#' 
#' Create a list of length 6 (there are 6 days of data).
#' Each element of the list should be a list of two data frames.
#' Use suitable names for both the main list and the internal lists.
#' This should (might?) save you time down the road...
#'

# get the directory files
days <- dir("SleepData")

# make a list containing all the data
sleep_data <- vector("list", length(days))

year <- 2011 # the data is from 2011 only

for (i in 1:length(days)) {
  day <- days[i] # day like Nov23
  date <- as.Date(paste(day, year), format = "%b%d %Y") # convert to YYYY-MM-DD

  # get the file paths for that day
  files <- dir(file.path("SleepData", day),
              full.names = TRUE, pattern = "^[^R]")
  
  # assign the embletta data
  x <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
  names(x) <- "angle"

  # make secs column from 10PM to 7AM at 10Hz
  x$secs <- seq(from = 0, to = (nrow(x)-1)*0.1, by = 0.1)
  
  y <- read.csv(files[2], as.is = TRUE)
  names(y)[2] <- "angle"

  y_times <- strptime(paste(date, y$Time_of_day), format = "%Y-%m-%d %H:%M:%S")

  # handle the times by adding one day if not between 22:00:00 and 23:59:59
  after_midnight <- !(format(y_times, "%H:%M:%S") >= "22:00:00" & 
                     format(y_times, "%H:%M:%S") <= "23:59:59")
  y_times[after_midnight] <- y_times[after_midnight] + 86400  # +1 day
  
  # get start time in day and time format
  y_start_time <- strptime(paste(date,"22:00:00"), format = "%Y-%m-%d %H:%M:%S")
  
  # take difference in seconds from start_time to each time
  y_seconds  <- as.numeric(difftime(y_times, y_start_time, units = "secs"))

  # assign secs (in the same units as x) to y
  y$secs <- y_seconds

  # name each element with the day and assign the data frames
  names(sleep_data)[i] <- day

  # cut off  data after 9 hours (32400 seconds)
  x <- x[x$secs <= 32400, ]
  y <- y[y$secs <= 32400, ]

  # get rid of angles greater than 180 degrees or less than -180 degrees
  x <- x[x$angle >= -180 & x$angle <= 180, ]
  y <- y[y$angle >= -180 & y$angle <= 180, ]

  if (i == 5) {
    # Shift secs of embletta by 2.5 hours for Nov23
    #   Embletta data only had 6.5 hours of data, and shifting by +2.5 aligned
    #   with SomnoPose data
    x$secs <- x$secs + 3600 * 2.5
    # reverse the angle of SomnoPose
    y$angle <- - y$angle
  } else if (i == 6) {
    # Shift secs of embletta by +1.5 hours for Nov24
    #   Embletta data only had 7.5 hours of data, and shifting by +1.5 aligned
    #   with SomnoPose data
    x$secs <- x$secs + 3600 * 1.5
    y$angle <- - y$angle
  }

  # Shift the SomnoPose angle with the mean difference to compare amplitudes
  # diff <- mean(x$angle[which(x$secs %in% y$secs)] 
  #            - y$angle[which(y$secs %in% x$secs)])
  # y$angle <- y$angle + diff

  sleep_data[[i]] <- list(Embletta = x, SomnoPose = y)
}

str(sleep_data)

#'
#' ## Challenge 2
#' 
#' Try to do a basic graphical exploration of the data
#' from any one of the days.  For this to make any sense at all, you'll
#' need to read (carefully) `SleepStudy.html` and perhaps look at the
#' contents of the README files (in the data directories).
#'
#' \clearpage
#'
#' ## ULTIMATE GOAL
#' 
#' Ideally, produce a 6-page-plus PDF file (maybe more if it includes the
#' processed report) that includes one graphic per day
#' that could be used to help the researchers visualize the study results
#' and decide how to proceed with future research.  Be as professional as
#' possible with the graphics and the short report, where
#' you should discuss and try to answer the questions posed by the
#' researcher in `SleepStudy.html`.  Yes, this could be a very short report
#' (though plots and explorations consume quite a bit of space)!
#'
#' Guidelines:
#'
#' - You may use any graphical tools or packages that you want, ___BUT___
#'   if you use non-base R or non-`ggplot` graphics or some package to assist with the
#'   processing, your code should be particularly beautiful and well-documented
#'   to the extent that we might use it as a learning example to be shared
#'   with the whole class!  It should also answer the question, "Why do you think
#'   this is better than using base R or `ggplot`?"  
#'   The answer is hopefully more interesting than, "I don't know how to do this in base R."  
#'   So, you don't have to receive permission -- the answer is yes **as long
#'   as** you agree to the guideline above.
#'
#' - Eventually, your group should submit your script containing all your work;
#'   team members should be clearly listed at the top.  Only the group
#'   leader should submit this R script and an accompanying PDF file (of either
#'   6 plots or a processed report, whatever you want).  You should each come
#'   to the "extra sessions" with your group script
#'   and you should be comfortable with everything in the script in case
#'   I want to ask questions.  Saying, "Oh, the group leader did that and I'm
#'   not sure!" isn't ideal, needless to say.
#'   
#' - The PDF should not identify your group or your group members, so
#'   student assessments of submissions will be anonymous.
#'
#'
#' # How?
#' 
#' Starting Tuesday, September 2: You should form groups of 3 if possible. There may be one or two groups of 2. 
#'
#' This will continue on Thursday, September 4 (maybe along with some other
#' things during class time), with a final assignment submission (group) due
#' before class on Tuesday, September 9.
#'


# plotting function for each day
plot_day <- function(day_num) {
  x <- sleep_data[[day_num]]$Embletta
  y <- sleep_data[[day_num]]$SomnoPose
  day <- as.Date(paste(names(sleep_data)[day_num], year), format = "%b%d %Y")
  day <- format(day, "%B %d") # make day like November 17

  # Embletta
  plot(x$secs, x$angle, type = "l", col = "blue",
       xlab = "Local Time", ylab = "Orientation (deg)",
       xaxt = "n", yaxt = "n", main = paste("Following night of", day),
       xlim = range(c(0, 32400)), ylim = range(c(-180, 180))) # Embletta
  lines(y$secs, y$angle, col = "red")  #  SomnoPose

  # dotted line at 0, light gray
  abline(h = c(-180, -90, 0, 90, 180), lty = 2, col = "lightgray")

  # Custom ticks for x axis showing time in HH:MM
  ticks <- seq(0, 9 * 3600, by = 3600)  
  labels <- format(as.POSIXct("22:00:00", format = "%H:%M:%S" ) + ticks, "%H:%M")
  axis(1, at = ticks, labels = labels)

  yticks <- seq(-180, 180, by = 45)
  axis(2, at = yticks, labels = yticks, las = 1)

  legend("bottom", c("Embletta", "SomnoPose"), col = c("blue", "red"), 
         lty = 1, inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n")
}

# save all plots as pdf in a subdirectory
for (day_num in 1:6) {
  day <- names(sleep_data)[day_num]
  pdf(file = file.path("plots", paste0(day, ".pdf")), 
    width = 8, height = 6)
  plot_day(day_num)
  dev.off()
}
