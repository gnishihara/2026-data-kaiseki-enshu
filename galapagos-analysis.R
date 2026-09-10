# galapagos データの解析
# 応用編
# Greg Nishihara
# 

# パッケージの読み込み
library(tidyverse)
library(emmeans)
library(mgcv)
library(gratia)

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

ggplot(galadf) + 
  geom_point(aes(x = Plants, y = PlantEnd))

ggplot(galadf) + geom_col(aes(x = Island, y = Area))
ggplot(galadf) + geom_col(aes(x = Island, y = Elevation))
ggplot(galadf) + geom_col(aes(x = Island, y = Nearest))
ggplot(galadf) + geom_col(aes(x = Island, y = StCruz))
ggplot(galadf) + geom_col(aes(x = Island, y = Adjacent))

ggplot(galadf) + geom_point(aes(x = Area, y = Adjacent))




