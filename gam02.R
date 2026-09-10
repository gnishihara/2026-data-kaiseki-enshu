# 一般化加法モデル (Generalized Additive Models; GAM)
# 2 共変量 の解析
# 2026-09-10
# Greg Nishihara


# パッケージの読み込み
library(tidyverse)
library(mgcv)  　# GAM解析用のパッケージ
library(gratia)　# GAMの結果を確認するためのパッケージ
# install.packages("gamair") # このパッケージに mack とよぶデータセットがあります

# データのもととなる論文：　https://doi.org/10.1139/f97-134
data("mack", package = "gamair")
mackdf = mack |> as_tibble()
mackdf

ggplot(mackdf) + 
  geom_point(
    aes(
      x = lon,
      y = lat,
      color = egg.dens
    )
  )


# m1: isotropic spline (等方性のスプライン)
m1 = gam(egg.count ~ s(lon, lat, k = 40), data = mackdf, family = poisson("log"))
draw(m1)
appraise(m1)

m2 = gam(egg.count ~ s(lon, k = 40) +  s(lat, k = 40), 
         data = mackdf, family = poisson("log"))
draw(m2)
appraise(m2)

# m3: anisotropic spline (異方性のスプライン)
m3 = gam(egg.count ~ te(lon,lat, k = 40), 
         data = mackdf, family = poisson("log"),
         method = "REML")
draw(m3)
appraise(m3)


