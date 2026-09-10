# galapagos データの解析
# 応用編
# Greg Nishihara
# 

# パッケージの読み込み
library(tidyverse)
library(emmeans)
library(mgcv)
library(gratia)
library(patchwork)

data(galapagos, package = "GLMsData")
galadf = galapagos |> as_tibble()
galadf3  = 
  galadf |> 
  select(Plants, Area, Elevation, Nearest, StCruz, Adjacent)

nb01 = MASS::glm.nb(Plants ~ Area + Adjacent + Elevation + Nearest + StCruz, data = galadf3)

galadf2 = 
  galadf3 |> 
  mutate(zansa = statmod::qresiduals(nb01),
         fit = predict(nb01))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa)))) +
  geom_smooth(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = Plants)) + 
  geom_abline(intercept = 0, slope = 1)
plot01 + plot02 + plot03 + plot_layout(ncol = 2)
summary(nb01)




nb02 = MASS::glm.nb(Plants ~ Area + Adjacent + Elevation, data = galadf3)
galadf2 = 
  galadf3 |> 
  mutate(zansa = statmod::qresiduals(nb02), fit = predict(nb02))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa)))) +
  geom_smooth(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = Plants)) + 
  geom_abline(intercept = 0, slope = 1)

pdata = galadf |> expand(Area, Adjacent = mean(Adjacent), Elevation = mean(Elevation))
pdata1 = pdata |> mutate(Area = log(Area))
tmp = predict(nb02, newdata = pdata1, se.fit = TRUE) |> as_tibble()
pdata = bind_cols(pdata, tmp)

plot04 = ggplot() +
  geom_point(aes(x = Area, y = Plants), 
             data = galadf) +
  geom_line(aes(x = Area, y = exp(fit)), data = pdata) +
  scale_color_viridis_c()
plot01 + plot02 + plot03 +plot04 + plot_layout(ncol = 2)
summary(nb02)



galadf3  = galadf |> select(Plants, Area, Elevation, Adjacent)
galadf3 = galadf3 |> 
  mutate(logArea = log(Area), logAdjacent = log(Adjacent), logElevation = log(Elevation))

nb03 = MASS::glm.nb(Plants ~ logArea + logAdjacent + logElevation, data = galadf3)
galadf2 = 
  galadf3 |> 
  mutate(zansa = statmod::qresiduals(nb03), fit = predict(nb03))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa)))) +
  geom_smooth(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = Plants)) + 
  geom_abline(intercept = 0, slope = 1)

pdata = galadf |> expand(Area, Adjacent = mean(Adjacent), Elevation = mean(Elevation))
pdata1 = pdata |> mutate(logArea = log(Area), logAdjacent = log(Adjacent), logElevation = log(Elevation))
tmp = predict(nb03, newdata = pdata1, se.fit = TRUE) |> as_tibble()
pdata = bind_cols(pdata, tmp)

plot04 = ggplot() +
  geom_point(aes(x = Area, y = Plants), 
             data = galadf) +
  geom_line(aes(x = Area, y = exp(fit)), data = pdata) +
  scale_color_viridis_c()
plot01 + plot02 + plot03 +plot04 + plot_layout(ncol = 2)

summary(nb03)








galadf3  = galadf |> select(Plants, Area, Elevation, Adjacent)
galadf3 = galadf3 |> mutate(logArea = log(Area))

lgaladf2 = galadf3 |> 
  mutate(zansa = statmod::qresiduals(nb04), fit = predict(nb04))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa)))) +
  geom_smooth(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = Plants)) + 
  geom_abline(intercept = 0, slope = 1)

pdata = galadf |> expand(Area = exp(seq(min(log(Area)), max(log(Area)), length = 91)))
pdata1 = pdata |> mutate(logArea = log(Area))
tmp = predict(nb04, newdata = pdata1, se.fit = TRUE) |> as_tibble()
pdata = bind_cols(pdata, tmp)

plot04 = ggplot() +
  geom_point(aes(x = Area, y = Plants), 
             data = galadf) +
  geom_line(aes(x = Area, y = exp(fit)), data = pdata) +
  geom_ribbon(
    aes(
      x = Area, ymin = exp(fit - 1.96 * se.fit), ymax = exp(fit + 1.96 * se.fit)
    ),
    data = pdata,
    alpha = 0.5
  ) +
  scale_x_log10() +
  scale_color_viridis_c()
plot01 + plot02 + plot03 + plot04 + plot_layout(ncol = 2)

summary(nb04)


























