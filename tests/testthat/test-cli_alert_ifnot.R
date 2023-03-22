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
})
