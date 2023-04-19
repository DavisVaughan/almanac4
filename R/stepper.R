#' Create a new stepper
#'
#' @description
#' - `stepper()` returns a function that can be used to add or subtract a
#'   number of days from a Date, "stepping" over events specified by an
#'   rschedule. You supply it the rschedule to step relative to, and then
#'   call the returned function with the number of days to step by.
#'
#' - `workdays()` is a convenient stepper for stepping over the weekend.
#'
#' - `%s+%` steps forwards.
#'
#' - `%s-%` steps backwards.
#'
#' You _must_ use `%s+` and `%s-%` to control the stepping. `+` and `-` will
#' not work due to limitations in R's S3 dispatch system. Alternatively, you
#' can call [vctrs::vec_arith()] directly, which powers `%s+%` with a correct
#' double dispatch implementation.
#'
#' @details
#' Internally, a stepper is just powered by [alma_step()], so feel free to
#' use that directly.
#'
#' @inheritParams alma_step
#' @inheritParams daily
#'
#' @param x,y `[objects]`
#'
#'   Objects to perform step arithmetic on. Typically Dates or steppers.
#'
#' @return
#' - `stepper()` returns a function of 1 argument, `n`, that can be used to
#'   step by `n` days, relative to the rschedule.
#'
#' - `workdays()` return a new stepper object.
#'
#' - `%s+%` and `%s-%` return a new shifted Date vector.
#'
#' @export
#' @examples
#' # A Thursday and Friday
#' x <- as.Date(c("1970-01-01", "1970-01-02"))
#'
#' # Thursday is stepped forward 1 working day to Friday,
#' # and then 1 more working day to Monday.
#' # Friday is stepped forward 1 working day to Monday,
#' # and then 1 more working day to Tuesday
#' x %s+% workdays(2)
#'
#' # ---------------------------------------------------------------------------
#'
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' on_christmas <- yearly() %>%
#'   recur_on_day_of_month(25) %>%
#'   recur_on_month_of_year("Dec")
#'
#' rb <- runion(on_weekends, on_christmas)
#'
#' workday <- stepper(rb)
#'
#' # Friday before Christmas, which was on a Monday
#' friday_before_christmas <- as.Date("2000-12-22")
#'
#' # Steps over the weekend and Christmas to the following Tuesday
#' friday_before_christmas %s+% workday(1)
#'
#' # ---------------------------------------------------------------------------
#'
#' # Christmas in 2005 was on a Sunday, but your company probably "observed"
#' # it on Monday. So when you are on the Friday before Christmas in 2005,
#' # stepping forward 1 working day should go to Tuesday.
#'
#' # We'll adjust the previous rule for Christmas to roll to the nearest
#' # non-weekend day, if it happened to fall on a weekend.
#' on_observed_christmas <- radjusted(
#'   on_christmas,
#'   adjust_on = on_weekends,
#'   adjustment = adj_nearest
#' )
#'
#' # Note that the "observed" date for Christmas is the 26th
#' alma_search("2005-01-01", "2006-01-01", on_observed_christmas)
#'
#' rb2 <- runion(on_weekends, on_observed_christmas)
#'
#' workday2 <- stepper(rb2)
#'
#' friday_before_christmas_2005 <- as.Date("2005-12-23")
#'
#' # Steps over the weekend and the observed Christmas date
#' # of 2005-12-26 to Tuesday the 27th.
#' friday_before_christmas_2005 %s+% workday2(1)
stepper <- function(rschedule) {
  function(n) {
    n <- vec_cast(n, integer(), x_arg = "n")
    new_stepper(n = n, rschedule = rschedule)
  }
}

# ------------------------------------------------------------------------------

#' @rdname stepper
#' @export
`%s+%` <- function(x, y) {
  vec_arith("+", x, y)
}

#' @rdname stepper
#' @export
`%s-%` <- function(x, y) {
  vec_arith("-", x, y)
}

# ------------------------------------------------------------------------------

#' @rdname stepper
#' @export
workdays <- function(n, since = NULL, until = NULL) {
  rschedule <- weekly(since = since, until = until)
  rschedule <- recur_on_weekends(rschedule)
  workdays_stepper <- stepper(rschedule)
  workdays_stepper(n)
}

# ------------------------------------------------------------------------------

new_stepper <- function(n = integer(), rschedule = daily()) {
  if (!is_integer(n)) {
    abort("`n` must be an integer.")
  }

  check_rschedule(rschedule)

  new_vctr(
    .data = n,
    rschedule = rschedule,
    class = "almanac_stepper",
    inherit_base_type = FALSE
  )
}

# ------------------------------------------------------------------------------

#' @export
vec_ptype_abbr.almanac_stepper <- function(x, ...) {
  "stepper"
}

#' @export
vec_ptype_full.almanac_stepper <- function(x, ...) {
  "stepper"
}

# ------------------------------------------------------------------------------

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow stepper objects to
#' work with vctrs.
#'
#' @param x,y,to Objects.
#' @param op An arithmetic operator as a string.
#' @param x_arg,y_arg,to_arg Used to pass along error message information.
#' @param ... Used to pass along error message information.
#'
#' @return
#' See the corresponding vctrs function for the exact return value.
#'
#' @name almanac-vctrs-compat
#' @keywords internal
#'
#' @examples
#' library(vctrs)
#' vec_arith("+", as.Date("2019-01-04"), workdays(1))
NULL

# ------------------------------------------------------------------------------

#' @rdname almanac-vctrs-compat
#' @export vec_arith.almanac_stepper
#' @method vec_arith almanac_stepper
#' @export
vec_arith.almanac_stepper <- function(op, x, y, ...) {
  UseMethod("vec_arith.almanac_stepper", y)
}

#' @method vec_arith.almanac_stepper default
#' @export
vec_arith.almanac_stepper.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.almanac_stepper MISSING
#' @export
vec_arith.almanac_stepper.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    `+` = plus_stepper_missing(x),
    `-` = minus_stepper_missing(x),
    stop_incompatible_op(op, x, y)
  )
}

plus_stepper_missing <- function(x) {
  x
}

minus_stepper_missing <- function(x) {
  rschedule <- stepper_rschedule(x)
  x <- unclass(x)
  x <- -x
  new_stepper(x, rschedule)
}

# Note that `stepper(1, daily()) - Sys.Date()` is not defined!

#' @method vec_arith.almanac_stepper Date
#' @export
vec_arith.almanac_stepper.Date <- function(op, x, y, ...) {
  switch(
    op,
    `+` = plus_stepper_date(x, y),
    stop_incompatible_op(op, x, y)
  )
}

plus_stepper_date <- function(x, y) {
  rschedule <- stepper_rschedule(x)
  n <- unclass(x)
  alma_step(y, n, rschedule)
}

#' @method vec_arith.Date almanac_stepper
#' @export
vec_arith.Date.almanac_stepper <- function(op, x, y, ...) {
  switch(
    op,
    `+` = plus_date_stepper(x, y),
    `-` = minus_date_stepper(x, y),
    stop_incompatible_op(op, x, y)
  )
}

plus_date_stepper <- function(x, y) {
  rschedule <- stepper_rschedule(y)
  n <- unclass(y)
  alma_step(x, n, rschedule)
}

minus_date_stepper <- function(x, y) {
  rschedule <- stepper_rschedule(y)
  n <- unclass(y)
  n <- -n
  alma_step(x, n, rschedule)
}

# ------------------------------------------------------------------------------

#' @rawNamespace
#' if (getRversion() >= "4.3.0") {
#'   S3method(chooseOpsMethod,almanac_stepper)
#' }
chooseOpsMethod.almanac_stepper <- function(x, y, mx, my, cl, reverse) {
  TRUE
}

# ------------------------------------------------------------------------------
# vec_ptype2()

#' @rdname almanac-vctrs-compat
#' @export
vec_ptype2.almanac_stepper.almanac_stepper <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!stepper_identical_rschedules(x, y)) {
    details <- "Steppers must have identical rschedules to be coercible."
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg, details = details)
  }
  new_stepper(rschedule = stepper_rschedule(x))
}

# ------------------------------------------------------------------------------
# vec_cast()

#' @rdname almanac-vctrs-compat
#' @export
vec_cast.almanac_stepper.almanac_stepper <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (!stepper_identical_rschedules(x, to)) {
    details <- "Steppers must have identical rschedules to be coercible."
    stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
  }
  x
}

# ------------------------------------------------------------------------------
# slider_plus() / slider_minus()

# @export - .onLoad()
slider_plus.Date.almanac_stepper <- function(x, y) {
  vec_arith("+", x, y)
}

# @export - .onLoad()
slider_minus.Date.almanac_stepper <- function(x, y) {
  vec_arith("-", x, y)
}

# ------------------------------------------------------------------------------

stepper_rschedule <- function(x) {
  attr(x, "rschedule", exact = TRUE)
}

stepper_identical_rschedules <- function(x, y) {
  identical(stepper_rschedule(x), stepper_rschedule(y))
}
