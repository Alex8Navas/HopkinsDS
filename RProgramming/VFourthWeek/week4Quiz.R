# Quiz Week 4

library(datasets)
library(ggplot2)
library(profr)

set.seed(1)

rpois(5, 2)


set.seed(10)
x <- rep(0:1, each = 5)
x
e <- rnorm(10, 0, 20)
e
y <- 0.5 + 2 * x + e
y


x1 <- rep(0:1, each = 5)
x2 <- seq(1, 10, by = 1)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x1 + x2 + e

Rprof("results/profile1.out")
fit <- lm(y ~ x1 + x2)
fit
Rprof(NULL)


summaryRprof("results/profile1.out")
profr(parse_rprof("profile1.out"))
plot(profr(parse_rprof("profile1.out")))
ggplot(profr(parse_rprof("profile1.out"))) + theme_light()
