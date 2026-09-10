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
