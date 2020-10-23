context("Bivariate copula models")

models <- c(
  "BB1Copula", "surBB1Copula", "r90BB1Copula", "r270BB1Copula",
  "BB6Copula", "surBB6Copula", "r90BB6Copula", "r270BB6Copula",
  "BB7Copula", "surBB7Copula", "r90BB7Copula", "r270BB7Copula",
  "BB8Copula", "surBB8Copula", "r90BB8Copula", "r270BB8Copula",
  "surClaytonCopula", "r90ClaytonCopula", "r270ClaytonCopula",
  "surGumbelCopula", "r90GumbelCopula", "r270GumbelCopula",
  "joeBiCopula", "surJoeBiCopula", "r90JoeBiCopula", "r270JoeBiCopula",
  "tawnT1Copula", "surTawnT1Copula", "r90TawnT1Copula", "r270TawnT1Copula",
  "tawnT2Copula", "surTawnT2Copula", "r90TawnT2Copula", "r270TawnT2Copula"
)

for (model in models) { # model <- models[1]
  test_that(paste("Bivariate model", model, "works"), {
    expect_silent(cop <- eval(parse(text = paste0(model, "()"))))

    u <- rCopula(2, cop)
    expect_length(u, 4)

    expect_length(dCopula(c(0.1, 0.1), cop), 1)
    expect_length(pCopula(c(0.1, 0.1), cop), 1)
    expect_length(dduCopula(c(0.1, 0.1), cop), 1)
    expect_length(ddvCopula(c(0.1, 0.1), cop), 1)

    expect_length(dCopula(u, cop), 2)
    expect_length(pCopula(u, cop), 2)
    expect_length(dduCopula(u, cop), 2)
    expect_length(ddvCopula(u, cop), 2)

    A(cop, 0.5)
    expect_lt(tau(cop), 1)
    expect_gt(tau(cop), -1)
    expect_length(lambda(cop), 2)
  })
}

# inverse Kendall's tau

modelsSngParPosTau <- c("surClaytonCopula", "surGumbelCopula",
                        "joeBiCopula", "surJoeBiCopula")

for (model in modelsSngParPosTau) { # model <- models[1]
  test_that(paste("Bivariate model", model, "works"), {
    expect_silent(cop <- eval(parse(text = paste0(model, "()"))))

    expect_length(iTau(cop, 0.2), 1)
  })
}

modelsSngParNegTau <- c("r90ClaytonCopula",  "r270ClaytonCopula",
                        "r90GumbelCopula", "r270GumbelCopula",
                        "r90JoeBiCopula", "r270JoeBiCopula")

for (model in modelsSngParNegTau) { # model <- models[1]
  test_that(paste("Bivariate model", model, "works"), {
    expect_silent(cop <- eval(parse(text = paste0(model, "()"))))

    expect_length(iTau(cop, -0.2), 1)
  })
}
