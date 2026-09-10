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

# Plants: 植物の種数
# PlantEnd: 島ごとの植物の固有種
# Area: 島の面積 (km2)
# Elevation: 島の最も高い高度 (m)
# Nearest: 最も近い島からの距離 (km)
# StCruz: StCruz からの距離 (km)
# Adjacent: 隣の島の面積 (km^2)
ggplot(galadf) +
  geom_point(
    aes(x = Island, y = Plants)
  )

ggplot(galadf) +
  geom_point(
    aes(x = Island, y = PlantEnd)
  )

ggplot(galadf) + geom_point(aes(x = Plants, y = PlantEnd))

ggplot(galadf) + geom_col(aes(x = Island, y = Area))
ggplot(galadf) + geom_col(aes(x = Island, y = Elevation))
ggplot(galadf) + geom_col(aes(x = Island, y = Nearest))
ggplot(galadf) + geom_col(aes(x = Island, y = StCruz))
ggplot(galadf) + geom_col(aes(x = Island, y = Adjacent))

ggplot(galadf) + geom_point(aes(x = Area, y = Adjacent))

################################################################################
# 解析はここから
# 目的：　どの説明変数が固有種の説明につながるのか
# PlantEnd は離散型のデータなので、離散型の確率分布を使う
# 離散型分布： Poisson (ポアソン分布), Negative Binomial (負の二項分布)


m0 = glm(PlantEnd ~ Area + Adjacent + Elevation + Nearest + StCruz, 
      data = galadf, family = poisson("log"))

galadf2 = 
  galadf |> 
  mutate(zansa = statmod::qresiduals(m0),
         fit = predict(m0))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = PlantEnd)) + 
  geom_abline(intercept = 0, slope = 1)

plot01 + plot02 + plot03 + plot_layout(ncol = 2)

summary(m0) # 残渣デビアス (Residual deviance) は 自由度と異なるので、モデルを却下する




m1 = glm(PlantEnd ~ Area + Adjacent + Elevation + StCruz, 
         data = galadf, family = poisson("log"))

galadf2 = 
  galadf |> 
  mutate(zansa = statmod::qresiduals(m1),
         fit = predict(m1))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = PlantEnd)) + 
  geom_abline(intercept = 0, slope = 1)

plot01 + plot02 + plot03 + plot_layout(ncol = 2)

summary(m1) # 残渣デビアス (Residual deviance) は 自由度と異なるので、モデルを却下する
# ところが、その他の変数はすべて有意だったので、外す変数がない。


# なら、負の二項分布をつかう

nb01 = MASS::glm.nb(PlantEnd ~ Area + Adjacent + Elevation + Nearest + StCruz, 
                    data = galadf)

galadf2 = 
  galadf |> 
  mutate(zansa = statmod::qresiduals(nb01),
         fit = predict(nb01))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa)))) +
  geom_smooth(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = PlantEnd)) + 
  geom_abline(intercept = 0, slope = 1)

plot01 + plot02 + plot03 + plot_layout(ncol = 2)

summary(nb01)


# Nearest と StCruz が p > 0.05 だったので外す
nb02 = MASS::glm.nb(PlantEnd ~ Area + Adjacent + Elevation, 
                    data = galadf)

galadf2 = 
  galadf |> 
  mutate(zansa = statmod::qresiduals(nb02),
         fit = predict(nb02))

plot01 = ggplot(galadf2) + geom_point(aes(x = fit, y = sqrt(abs(zansa)))) +
  geom_smooth(aes(x = fit, y = sqrt(abs(zansa))))
plot02 = ggplot(galadf2) + geom_qq(aes(sample = zansa)) + geom_qq_line(aes(sample = zansa))
plot03 = ggplot(galadf2) + 
  geom_point(aes(x = exp(fit), y = PlantEnd)) + 
  geom_abline(intercept = 0, slope = 1)

plot01 + plot02 + plot03 + plot_layout(ncol = 2)

summary(nb02)

# 当てはめたモデルと観測値の図

pdata = galadf |> 
  expand(
    Area = seq(min(Area), max(Area), length = 21),
    Adjacent = seq(min(Adjacent), max(Adjacent), length = 21),
    Elevation = seq(min(Elevation), max(Elevation), length = 21)
  )

pdata1 = galadf |> 
  expand(
    Area = seq(min(Area), max(Area), length = 21),
    Adjacent = median(Adjacent),
    Elevation = median(Elevation)
  )

pdata2 = galadf |> 
  expand(
    Adjacent = seq(min(Adjacent), max(Adjacent), length = 21),
    Area = median(Area),
    Elevation = median(Elevation)
  )

pdata3 = galadf |> 
  expand(
    Elevation = seq(min(Elevation), max(Elevation), length = 21),
    Adjacent = median(Adjacent),
    Area = median(Area)
  )

tmp = predict(nb02, newdata = pdata, se.fit = TRUE) |> as_tibble()
pdata2 = bind_cols(pdata, tmp)

ggplot() + 
  geom_point(aes(x = Area, y = PlantEnd), data = galadf) + 
  geom_line(aes(x = Area, y = exp(fit), ))






