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

#'
#' ## Challenge 1
#' 
#' Create a list of length 6 (there are 6 days of data).
#' Each element of the list should be a list of two data frames.
#' Use suitable names for both the main list and the internal lists.
#' This should (might?) save you time down the road...
# ---------- helpers ----------
read_embletta <- function(path_txt, fs = 10) {
  # Read lines, drop header, coerce to numeric degrees [0,360)
  ln <- readLines(path_txt, warn = FALSE)
  if (length(ln) == 0) stop("Empty Embletta file: ", path_txt)
  # files have a first line like "Position_EG Rate:10Hz"
  nums <- suppressWarnings(as.numeric(ln[-1]))
  nums <- nums[!is.na(nums)]
  eg_wrapped     <- ifelse(nums > 180, nums - 360, nums)   # 0..360 -> -180..180
  eg_deg_signed  <- -eg_wrapped 
  n <- seq_along(nums)
  data.frame(
    sample  = n,
    t_sec   = (n - 1) / fs,      # relative time; note that there is 10 recordings per sec (10Hz)
    eg_deg  = nums,              # Euler/Gravity-ish angle (0..360)                   
    eg_wrapped    = eg_wrapped,
    eg_deg_signed = eg_deg_signed,  # flip sign
    stringsAsFactors = FALSE
  )
}


read_somnopose <- function(path_csv, tz_local = "America/New_York") {
  df <- read.csv(path_csv, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  # pull inclination/orientation (be tolerant to slight header changes)
  inc <- as.numeric(df$inclination)
  ori <- as.numeric(df$orientation)
  ts_raw <- df$timestamp

  # --- timestamp parsing with epoch detection ---
  ts_utc <- NA
  vals <- as.numeric(ts_raw)
  #  - Apple (since 2001) values in 2011 are ~3.4e8
  use_apple <- TRUE
  origin <- if (use_apple) "2001-01-01" else "1970-01-01"
  ts_utc <- as.POSIXct(vals, origin = origin, tz = "UTC")
  
  data.frame(
    timestamp_utc   = ts_utc,                         # POSIXct UTC
    timestamp_local = as.POSIXct(format(ts_utc, tz = tz_local),
                                 tz = tz_local), 
    inclination_deg = inc,
    orientation_deg = ori,
    row_index       = seq_len(nrow(df)),
    stringsAsFactors = FALSE
  )
}


read_one_day <- function(day_dir) {
  files <- dir(day_dir, full.names = TRUE, recursive = FALSE)
  files <- files[!grepl("^README", basename(files), ignore.case = TRUE)]
  # Identify files by extension/name
  txt <- files[grepl("\\.txt$", files, ignore.case = TRUE) |
                 grepl("Position_EG", files, ignore.case = TRUE)]
  csv <- files[grepl("\\.csv$", files, ignore.case = TRUE) |
                 grepl("SomnoPose", files, ignore.case = TRUE)]

  if (length(txt) != 1 || length(csv) != 1) {
    stop("Expected exactly one Embletta .txt and one SomnoPose .csv in: ", day_dir,
         "\nFound: ", paste(basename(files), collapse=", "))
  }

  list(
    embletta  = read_embletta(txt[[1]], fs = 10),
    somnopose = read_somnopose(csv[[1]])
  )
}

# ---------- build the main list ----------
day_dirs <- dir("SleepData", full.names = TRUE)
day_dirs <- day_dirs[file.info(day_dirs)$isdir]
day_dirs <- sort(day_dirs)
sleep <- lapply(day_dirs, read_one_day)
names(sleep) <- basename(day_dirs)

stopifnot(length(sleep) == 6)

#'
#' ## Challenge 2
#' 
#' Try to do a basic graphical exploration of the data
#' from any one of the days.  For this to make any sense at all, you'll
#' need to read (carefully) `SleepStudy.html` and perhaps look at the
#' contents of the README files (in the data directories).
which_night <- 1
FLIP_EMBLETTA_NIGHTS <- c(5, 6)

# PLOT 1: Somnopose during the night ---------------------------------------
sp <- sleep[[which_night]]$somnopose  # <- selected night;
# get local timestamps
tl <- sp$timestamp_local

# choose the first date present, from 22:00 to +9 hours
date0 <- as.Date(min(tl, na.rm = TRUE), tz = "America/New_York")
# shift to previous date if recording started after midnight (like on day 3)
min_tl <- min(tl, na.rm = TRUE)
first_hour <- as.integer(format(min_tl, "%H"))  # local hour
date0 <- date0 - (first_hour < 12)
# Plot from 10pm for 9h (if data exists)
start <- as.POSIXct(paste(date0, "22:00:00"), tz = "America/New_York")
end   <- start + 9*3600  # 9 hours

# orientation
ori <- as.numeric(sp$orientation_deg)
ok  <- tl >= start & tl <= end
# plot
op <- par(mar=c(4.8,5.2,3.6,1.6), cex.lab=1.2, cex.axis=1.1)
plot(tl[ok], ori[ok], type="l", lwd=2.5, col="#1f77b4",
     xlab="Local time", ylab="Orientation (deg, signed)",
     xaxt="n", ylim=c(-180,180),
     main=paste("SomnoPose –", format(date0)))
axis.POSIXct(1, at=seq(start, end, by="2 hours"), format="%H:%M")
abline(h=seq(-180,180,30), col="gray90", lwd=1)
par(op)

# PLOT 2: Embletta ----------------------------------------------------------
# pick the same day index used above
em <- sleep[[which_night]]$embletta  

## --- pull/compute time (seconds since start) and signed angle [-180,180] ---
# time column
t_sec <- em$t_sec

# angle column (prefer already-signed if present)
ang <- em$eg_deg#_signed


# clean: finite and within [-180,180]
ok <- is.finite(t_sec) & is.finite(ang) & abs(ang) <= 300
t_sec <- t_sec[ok]; ang <- ang[ok]

## --- base plot (raw ~10 Hz) ---
op <- par(mar=c(4.8,5.2,3.6,1.6), cex.lab=1.2, cex.axis=1.1)
plot(t_sec, ang, type="l", lwd=1.5, col="#d62728",
     xlab="Seconds since start (Embletta, ~10 Hz)",
     ylab="Orientation (deg, signed)", ylim=c(-180,180),
     main="Embletta – seconds since start")
abline(h=seq(-180,180,30), col="gray90", lwd=1)

## --- add a 2-second median trace for readability ---
emb2 <- aggregate(list(eg = ang), by = list(sec2 = floor(t_sec/2)*2),
                  FUN = function(z) median(z, na.rm = TRUE))
lines(emb2$sec2, emb2$eg, col="#1f77b4", lwd=2.5)  # thicker, smoothed line
legend("topleft", bty="n", col=c("#d62728","#1f77b4"), lwd=c(1.5,2.5),
       legend=c("raw (~10 Hz)","2s median"))
par(op)


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
# PLOT 3: ----------------------------------------------------------------
## OVERLAY: SomnoPose (time-of-day) + Embletta (2s median, lag-aligned)
which_night <- 6 # TO BE ADJUSTED

# ------------------------------------------------------
# --- 1) SomnoPose in the night window -> 2s medians on a "seconds-since-night-start" axis
sp  <- sleep[[which_night]]$somnopose
tl  <- sp$timestamp_local
min_tl <- min(tl, na.rm = TRUE)
date0  <- as.Date(min_tl, tz = "America/New_York")
first_hour <- as.integer(format(min_tl, "%H"))
date0  <- date0 - (first_hour < 12)  # anchor to previous date if after midnight
start  <- as.POSIXct(paste(date0, "22:00:00"), tz = "America/New_York")
end    <- start + 10*3600

ori_sp <- as.numeric(sp$orientation_deg)
ori_sp <- ifelse(ori_sp > 180, ori_sp - 360, ori_sp)  # wrap to [-180,180]
in_win <- is.finite(tl) & is.finite(ori_sp) & tl >= start & tl <= end & abs(ori_sp) <= 180

sp_win <- data.frame(
  sec = floor(as.numeric(tl[in_win] - start, units = "secs")/2)*2,
  ori = ori_sp[in_win]
)
sp2 <- aggregate(list(ori = sp_win$ori), by = list(sec = sp_win$sec),
                 FUN = function(z) median(z, na.rm = TRUE))
names(sp2)[1] <- "sec"

# ------------------------------------------------------
# --- 2) Embletta -> 2s medians on its own seconds-since-device-start axis
em    <- sleep[[which_night]]$embletta
t_em  <- if ("t_sec" %in% names(em)) em$t_sec else (seq_len(nrow(em)) - 1)/10
ang_em <- em$eg_wrapped           # use the signed column from the loader

# force the flip on nights 5 & 6
if (which_night %in% FLIP_EMBLETTA_NIGHTS) ang_em <- -ang_em
ok_em <- is.finite(t_em) & is.finite(ang_em) & abs(ang_em) <= 180
emb2  <- aggregate(list(eg = ang_em[ok_em]),
                   by = list(sec = floor(t_em[ok_em]/2)*2),
                   FUN = function(z) median(z, na.rm = TRUE))
names(emb2)[1] <- "sec"

# ------------------------------------------------------
# --- 3) Align Embletta to SomnoPose by maximizing cross-correlation (on a common 2s grid)
lag_seconds <- 0
if (nrow(sp2) >= 30 && nrow(emb2) >= 30) {
  s <- max(min(sp2$sec, na.rm = TRUE), min(emb2$sec, na.rm = TRUE))
  e <- min(max(sp2$sec, na.rm = TRUE), max(emb2$sec, na.rm = TRUE))
  if (is.finite(s) && is.finite(e) && e - s >= 120) {
    step_sec <- 2
    grid <- seq(s, e, by = step_sec)  # 2s grid over the INTERSECTION
    # No extrapolation at the ends
    sg <- approx(sp2$sec,  sp2$ori, xout = grid, method = "linear", rule = 1)$y
    eg <- approx(emb2$sec, emb2$eg,  xout = grid, method = "linear", rule = 1)$y
    ok <- is.finite(sg) & is.finite(eg)
    if (sum(ok) > 30) {
      x <- scale(sg[ok])[,1]
      y <- scale(eg[ok])[,1]
      m <- length(x)
      # search up to ±8h, but also limited by data length
      MAX_LAG_SECS <- 8 * 3600
      lag.max <- min(MAX_LAG_SECS / step_sec, m - 5)  # in samples
      cc <- ccf(x, y, lag.max = lag.max, plot = FALSE, na.action = na.omit)
      # ---- MINIMUM OVERLAP FILTER: require >= 2 hours of overlapping pairs ----
      min_overlap_sec     <- 2 * 3600
      min_overlap_samples <- min_overlap_sec / step_sec
      # For ccf computed on x & y (length m), overlap at lag k is (m - |k|)
      valid <- (m - abs(cc$lag)) >= min_overlap_samples
      if (any(valid)) {
        acf_use <- cc$acf
        acf_use[!valid] <- -Inf  # discard lags with too little overlap
        best_lag_samples <- cc$lag[ which.max(acf_use) ]
        lag_seconds <- as.integer(best_lag_samples * step_sec)
        # Direction: ccf(x,y) correlates x[t+k] with y[t].
        # If k>0, x leads y. To align y (Embletta) to x (SomnoPose), move y FORWARD by k:
        emb2$sec_aligned <- emb2$sec + lag_seconds
        # (Optional) debug print:
        used_pairs <- m - abs(best_lag_samples)
        cat(sprintf("Lag=%ds (k=%d), overlap=%.1f h, peak=%.3f\n",
                    lag_seconds, best_lag_samples,
                    used_pairs * step_sec / 3600, max(acf_use, na.rm = TRUE)))
      } else {
        # No lag meets the 2h requirement; skip shifting
        emb2$sec_aligned <- emb2$sec
        cat("No candidate lag had >= 2h overlap; leaving lag at 0s.\n")
      }
    }
  }
}
# If CCF didn't run, pass-through alignment
if (!"sec_aligned" %in% names(emb2)) emb2$sec_aligned <- emb2$sec
# map to local time axis
sp2$time  <- start + sp2$sec
emb2$time <- start + emb2$sec_aligned

# ------------------------------------------------------
# --- 4) Plot overlay -------------------------------------------------------
end_date_str <- format(end, tz = "America/New_York", format = "%Y-%m-%d")
graphics.off(); par(mfrow = c(1,1))
col_sp <- "#1f77b4"; col_em <- "#d62728"
op <- par(mar = c(4.8, 5.2, 6.0, 1.6), cex.lab = 1.2, cex.axis = 1.1)
xr <- range(c(sp2$time, emb2$time), na.rm = TRUE)
plot(sp2$time, sp2$ori, type = "l", lwd = 2.6, col = col_sp,
     xlim = xr, ylim = c(-180, 180),
     xlab = "Local time", ylab = "Orientation / Roll (deg)",
     xaxt = "n",
     main = sprintf("SomnoPose vs Embletta (2s medians) — lag = %s s — ends %s",
                    lag_seconds, end_date_str))
axis.POSIXct(1, at = seq(start, end, by = "2 hours"), format = "%H:%M")
abline(h = seq(-180, 180, by = 30), v = seq(start, end, by = "2 hours"), col = "gray90", lwd = 1)
lines(emb2$time, emb2$eg, lwd = 2.6, col = col_em)
op_xpd <- par(xpd = NA)  # allow drawing in the margin
usr  <- par("usr")                        
xmid <- mean(usr[1:2])              
ytop <- usr[4] + 0.02 * diff(usr[3:4])
legend(x = xmid, y = ytop,
       xjust = 0.5, yjust = 0,      
       horiz = TRUE,
       bty = "o", bg = "white", box.lwd = 1, box.col = "gray40",
       col = c(col_sp, col_em),
       lwd = 2, seg.len = 1.4, cex = 0.9, x.intersp = 0.7,
       legend = c("SomnoPose (2s)", "Embletta (2s median)")) 
par(op_xpd)
par(op)


#' # How?
#' 
#' Starting Tuesday, September 2: You should form groups of 3 if possible. There may be one or two groups of 2. 
#'
#' This will continue on Thursday, September 4 (maybe along with some other
#' things during class time), with a final assignment submission (group) due
#' before class on Tuesday, September 9.
  #'
