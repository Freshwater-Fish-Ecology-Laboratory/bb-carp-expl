# Code to generate dates and times for randomized creel surveys at Carp Lake
# Liz Hirsch
# February 23, 2023

# Random sampling of approx. 50% of the available sampling period
# Stratified by weekday/weekend and by time of day AM/PM
# Range of available dates: June 17 - Sept. 17

start_avail_dates <- as.Date("2023-06-17")
end_avail_dates <- as.Date("2023-09-17")
range_avail <- seq(from = start_avail_dates, to = end_avail_dates, by = 'day')
length(range_avail)

# 93 total days.
  ## 65 week days / 5 = 13 weeks
  ## 28 weekend days / 2 = 14 weekends

# Then randomly sample half of these days (but keep weekdays in 5-day blocks and weekends
# in two-day blocks)
sample(1:13, 6, replace=FALSE) # weeks: 12, 3, 1, 13, 9, 5
sample(1:14, 7, replace = FALSE) # weekends: 9, 4, 8, 2, 3, 13, 12
# total 44 days

#THen, within days randomly pick between AM or PM shift
# AM: 07:00 - 14:00 ("option 0")
# PM: 14:00 - 21:00 ("option 1")
sample(0:1, 30, replace = TRUE) # AM or PM for each day within week days
# 1 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 0 0 1 0 1 1 0
sample(0:1, 14, replace = TRUE) # for each day within weekend days
# 0 1 1 0 1 1 1 1 1 0 1 1 0 1

# Make a calendar! See calendar_fieldprep_23
# And determine field crews, when Zoe will join for interviews and when others maybe do 
# some creels

