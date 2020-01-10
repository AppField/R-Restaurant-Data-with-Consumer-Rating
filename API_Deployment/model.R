x = 1:10
y = x + rnorm(x)
m = lm(y ~ x)
save(m, file = "model.rda")
