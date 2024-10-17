library(tidyverse)


finalNotu <- studentfile$final*10
midterm <- studentfile$midterm*10


plot(midterm, finalNotu, main = "final notu ve midterm notu",
     xlab = "Independent Variable (midterm)", ylab = "Dependent Variable (final)",
     pch = 19, col = "blue")

lm(finalNotu ~ midterm)


model <- lm(y ~ x)

summary(model)

