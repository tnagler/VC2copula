context("Vine copula models")
test_that("Vine copula models work", {
  expect_silent(cop <- vineCopula(as.integer(3)))
  expect_silent(cop <- vineCopula(as.integer(3), type = "DVine"))
  RVM <- VineCopula::C2RVine(1:3, rep(1, 3), rep(0.5, 3))
  expect_silent(cop <- vineCopula(RVM))

  u <- rCopula(30, cop)
  expect_equal(dim(u), c(30, 3))
  expect_length(dCopula(u, cop), 30)

  fitCopula(cop, u)
  fitCopula(cop, u, method = list(StructureSelect = FALSE))
  expect_output(show(cop))

  expect_error(vineCopula(2))
  expect_error(vineCopula(as.integer(1)))
  expect_error(
    new(getClass("vineCopula", where = "VC2copula"),
        copulas = lapply(1:3, function(i) "a"),
        dimension = as.integer(3),
        RVM = RVM,
        parameters = rep(0.5, 3),
        param.names = rep("a", 3),
        param.lowbnd = rep(0, 3),
        param.upbnd = rep(1, 3))
  )
  expect_error(
    new(getClass("vineCopula", where = "VC2copula"),
        copulas = lapply(1:3, function(i) normalCopula()),
        dimension = 3,
        RVM = RVM,
        parameters = rep(0.5, 3),
        param.names = rep("a", 3),
        param.lowbnd = rep(0, 3),
        param.upbnd = rep(1, 3))
  )
  expect_error(
    new(getClass("vineCopula", where = "VC2copula"),
        copulas = lapply(1:3, function(i) normalCopula()),
        dimension = as.integer(2),
        RVM = RVM,
        parameters = rep(0.5, 3),
        param.names = rep("a", 3),
        param.lowbnd = rep(0, 3),
        param.upbnd = rep(1, 3))
  )
  expect_error(
    new(getClass("vineCopula", where = "VC2copula"),
        copulas = lapply(1:3, function(i) normalCopula()),
        dimension = as.integer(3),
        RVM = RVM,
        parameters = rep(0.5, 3),
        param.names = rep("a", 3),
        param.lowbnd = rep(0, 2),
        param.upbnd = rep(1, 3))
  )
  expect_error(
    new(getClass("vineCopula", where = "VC2copula"),
        copulas = lapply(1:3, function(i) normalCopula()),
        dimension = as.integer(3),
        RVM = RVM,
        parameters = rep(0.5, 3),
        param.names = rep("a", 3),
        param.lowbnd = rep(0, 3),
        param.upbnd = rep(1, 2))
  )
})
