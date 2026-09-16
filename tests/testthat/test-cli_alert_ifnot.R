test_that("cli_alert_ifnot works", {
  expect_message(
    cli_alert_if("Alert", TRUE),
    "Alert"
  )
  expect_message(
    cli_alert_ifnot("Alert", FALSE),
    "Alert"
  )
  expect_message(
    cli_success_if("Success", TRUE),
    "Success"
  )
  expect_message(
    cli_success_ifnot("Success", FALSE),
    "Success"
  )
  expect_message(
    cli_info_if("Info", TRUE),
    "Info"
  )
  expect_message(
    cli_info_ifnot("Info", FALSE),
    "Info"
  )
  expect_message(
    cli_danger_if("Danger", TRUE),
    "Danger"
  )
  expect_message(
    cli_danger_ifnot("Danger", FALSE),
    "Danger"
  )
  expect_message(
    cli_danger_if("Warning", TRUE),
    "Warning"
  )
  expect_message(
    cli_danger_ifnot("Warning", FALSE),
    "Warning"
  )
  expect_message(
    cli_warning_if("Warning", TRUE),
    "Warning"
  )
  expect_message(
    cli_warning_ifnot("Warning", FALSE),
    "Warning"
  )
})

test_that("cli_alert_if and cli_alert_ifnot do nothing when condition is not met", {
  expect_no_message(
    cli_alert_if("Alert", FALSE)
  )
  expect_no_message(
    cli_alert_ifnot("Alert", TRUE)
  )
  expect_no_message(
    cli_alert_if("Alert", condition = NULL)
  )
})

test_that("cli_alert_if errors for an invalid .fn", {
  expect_error(
    cli_alert_if("Alert", TRUE, .fn = "not-a-valid-option")
  )
  expect_error(
    cli_alert_if("Alert", TRUE, .fn = 123)
  )
})

test_that("cli_alert_if accepts a cli function directly as .fn", {
  expect_message(
    cli_alert_if("Alert", TRUE, .fn = cli::cli_alert_success),
    "Alert"
  )
})
