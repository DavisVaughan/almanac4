#' Holidays
#'
#' @description
#' This page lists a number of pre-created holidays that can be added to a
#' calendar created with [rcalendar()]. This list makes no attempt to be
#' comprehensive. If you need to create your own holiday, you can do so with
#' [rholiday()].
#'
#' It also makes no attempt to be historically accurate, i.e. Juneteenth was
#' created in 2021, but `hol_us_juneteenth()` will generate event dates before
#' that. Because [rholiday()] takes an arbitrary rschedule object, you can
#' always create an rschedule that is historically accurate and use that
#' instead.
#'
#' @details
#' Note that _relative_ holidays, such as New Year's Eve, which is 1 day before
#' New Year's Day, aren't pre-created in a way that allows you to define
#' observance rules for them that depend on the observance rules of the holiday
#' they are relative to. If you need to do this, you should start with the base
#' holiday, here [hol_new_years_day()], and use [hol_observe()] and
#' [hol_offset()] on that to generate a New Year's Eve holiday that matches
#' your required observance rules. See the examples of [hol_offset()] for more
#' information.
#'
#' @param since `[Date(1)]`
#'
#'   A lower bound on the event set to generate.
#'
#'   Defaults to [almanac_since()] if not set.
#'
#' @param until `[Date(1)]`
#'
#'   An upper bound on the event set to generate.
#'
#'   Defaults to [almanac_until()] if not set.
#'
#' @name holidays
#' @examples
#' on_christmas <- hol_christmas()
#' on_christmas
#'
#' # These are like any other rschedule object
#' alma_events(on_christmas, year = 2020:2025)
#'
#' # But they can also be added into an rcalendar
#' cal <- rcalendar(
#'   on_christmas,
#'   hol_halloween(),
#'   hol_new_years_day(),
#'   hol_us_presidents_day()
#' )
#' cal
#'
#' # Which gives you access to a number of `cal_*()` functions
#' cal_events(cal, year = 2020:2022)
NULL

# ------------------------------------------------------------------------------
# Global

#' @export
#' @rdname holidays
hol_christmas <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "December")
  out <- recur_on_day_of_month(out, 25L)

  rholiday(
    rschedule = out,
    name = "Christmas"
  )
}

#' @export
#' @rdname holidays
hol_christmas_eve <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "December")
  out <- recur_on_day_of_month(out, 24L)

  rholiday(
    rschedule = out,
    name = "Christmas Eve"
  )
}

#' @export
#' @rdname holidays
hol_easter <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_easter(out)

  rholiday(
    rschedule = out,
    name = "Easter"
  )
}

#' @export
#' @rdname holidays
hol_good_friday <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_easter(out)
  out <- roffset(out, by = -2L)

  rholiday(
    rschedule = out,
    name = "Good Friday"
  )
}

#' @export
#' @rdname holidays
hol_halloween <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "October")
  out <- recur_on_day_of_month(out, 31L)

  rholiday(
    rschedule = out,
    name = "Halloween"
  )
}

#' @export
#' @rdname holidays
hol_new_years_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "January")
  out <- recur_on_day_of_month(out, 1L)

  rholiday(
    rschedule = out,
    name = "New Year's Day"
  )
}

#' @export
#' @rdname holidays
hol_new_years_eve <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "December")
  out <- recur_on_day_of_month(out, 31L)

  rholiday(
    rschedule = out,
    name = "New Year's Eve"
  )
}

#' @export
#' @rdname holidays
hol_st_patricks_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "March")
  out <- recur_on_day_of_month(out, 17L)

  rholiday(
    rschedule = out,
    name = "Saint Patrick's Day"
  )
}

#' @export
#' @rdname holidays
hol_valentines_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "February")
  out <- recur_on_day_of_month(out, 14L)

  rholiday(
    rschedule = out,
    name = "Valentine's Day"
  )
}

#' @export
#' @rdname holidays
hol_easter_monday <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_easter(out)
  out <- roffset(out, by = 1L)
  
  rholiday(
    rschedule = out,
    name = "Easter Monday"
  )
}

#' @export
#' @rdname holidays
hol_maundy_thursday <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_easter(out)
  out <- roffset(out, by = -3L)
  
  rholiday(
    rschedule = out,
    name = "Maundy Thursday"
  )
}

#' @export
#' @rdname holidays
hol_ascension_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_easter(out)
  out <- roffset(out, by = 40L)
  
  rholiday(
    rschedule = out,
    name = "Ascension Day"
  )
}

#' @export
#' @rdname holidays
hol_whit_monday <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_easter(out)
  out <- roffset(out, by = 51L)
  
  rholiday(
    rschedule = out,
    name = "Whit Monday"
  )
}

#' @export
#' @rdname holidays
hol_epiphany <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "January")
  out <- recur_on_day_of_month(out, 6L)
  
  rholiday(
    rschedule = out,
    name = "Epiphany"
  )
}

#' @export
#' @rdname holidays
hol_assumption_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "August")
  out <- recur_on_day_of_month(out, 15L)
  
  rholiday(
    rschedule = out,
    name = "Assumption of Mary"
  )
}

#' @export
#' @rdname holidays
hol_all_saints_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "November")
  out <- recur_on_day_of_month(out, 1L)
  
  rholiday(
    rschedule = out,
    name = "All Saint's Day"
  )
}

#' @export
#' @rdname holidays
hol_immaculuate_conception_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "December")
  out <- recur_on_day_of_month(out, 8L)
  
  rholiday(
    rschedule = out,
    name = "Feast of the Immaculate Conception"
  )
}


# ------------------------------------------------------------------------------
# US

#' @export
#' @rdname holidays
hol_us_election_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "November")
  out <- recur_on_day_of_week(out, "Tuesday")
  out <- recur_on_day_of_month(out, 2:8)

  rholiday(
    rschedule = out,
    name = "US Election Day"
  )
}

#' @export
#' @rdname holidays
hol_us_fathers_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "June")
  out <- recur_on_day_of_week(out, "Sunday", nth = 3L)

  rholiday(
    rschedule = out,
    name = "US Father's Day"
  )
}

#' @export
#' @rdname holidays
hol_us_independence_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "July")
  out <- recur_on_day_of_month(out, 4L)

  rholiday(
    rschedule = out,
    name = "US Independence Day"
  )
}

#' @export
#' @rdname holidays
hol_us_indigenous_peoples_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "October")
  out <- recur_on_day_of_week(out, "Monday", nth = 2L)

  rholiday(
    rschedule = out,
    name = "US Indigenous Peoples' Day"
  )
}

#' @export
#' @rdname holidays
hol_us_juneteenth <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "June")
  out <- recur_on_day_of_month(out, 19L)

  rholiday(
    rschedule = out,
    name = "US Juneteenth"
  )
}

#' @export
#' @rdname holidays
hol_us_labor_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "September")
  out <- recur_on_day_of_week(out, "Monday", nth = 1L)

  rholiday(
    rschedule = out,
    name = "US Labor Day"
  )
}

#' @export
#' @rdname holidays
hol_us_martin_luther_king_junior_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "January")
  out <- recur_on_day_of_week(out, "Monday", nth = 3L)

  rholiday(
    rschedule = out,
    name = "US Martin Luther King Jr. Day"
  )
}

#' @export
#' @rdname holidays
hol_us_memorial_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "May")
  out <- recur_on_day_of_week(out, "Monday", nth = -1L)

  rholiday(
    rschedule = out,
    name = "US Memorial Day"
  )
}

#' @export
#' @rdname holidays
hol_us_mothers_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "May")
  out <- recur_on_day_of_week(out, "Sunday", nth = 2L)

  rholiday(
    rschedule = out,
    name = "US Mother's Day"
  )
}

#' @export
#' @rdname holidays
hol_us_presidents_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "February")
  out <- recur_on_day_of_week(out, "Monday", nth = 3L)

  rholiday(
    rschedule = out,
    name = "US Presidents' Day"
  )
}

#' @export
#' @rdname holidays
hol_us_thanksgiving <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "November")
  out <- recur_on_day_of_week(out, "Thursday", nth = 4L)

  rholiday(
    rschedule = out,
    name = "US Thanksgiving"
  )
}

#' @export
#' @rdname holidays
hol_us_veterans_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "November")
  out <- recur_on_day_of_month(out, 11L)

  rholiday(
    rschedule = out,
    name = "US Veterans Day"
  )
}

