test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("app_sys works", {
  expect_true(
    app_sys("golem-config.yml") != ""
  )
})

test_that("golem-config works", {
  config_file <- app_sys("golem-config.yml")
  skip_if(config_file == "")

  expect_true(
    get_golem_config(
      "app_prod",
      config = "production",
      file = config_file
    )
  )
  expect_false(
    get_golem_config(
      "app_prod",
      config = "dev",
      file = config_file
    )
  )
})

# Smoke-test server: modules attach and first flush completes without error
test_that("app_server testServer smoke", {
  shiny::testServer(app_server, {
    session$flushReact()
    expect_true(inherits(session, "MockShinySession"))
  })
})

# Configure this test to fit your need
test_that("app launches", {
  golem::expect_running(sleep = 5)
})
