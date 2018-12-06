context("rollAR")

library(xts)

AR1 <- -0.6
AR2 <- -0.4
AR <- c(AR1, AR2)

data <- xts(data.frame(
    A = c( 0.0100,  0.0050,  0.0220),
    B = c(-0.0010,  0.0025,  0.0110),
    C = c(-0.0045, -0.0086,  0.011),
    D = c(-0.0123, -0.0234, -0.0345)),
    order.by = as.Date("2018-12-03") + 1:3)

expect <- xts(
    apply(data, 2, function(x){c(NA,sum(rev(x[1:2])*AR), sum(rev(x[2:3])*AR))}),
    order.by = index(data))

result <- rollAR(data, AR)

test_that("rollAR result matches manual computation", {
    expect_identical(result, expect)
})
