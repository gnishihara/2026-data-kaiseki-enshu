# t検定・分散分析・多重比較
# 2026-09-09
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(marginaleffects)

irisdf = iris |> as_tibble()

# t検定
# 2群の比較

df1 = irisdf |> filter(str_detect(Species, "set", negate = TRUE))
df2 = irisdf |> filter(str_detect(Species, "vir", negate = TRUE))
df3 = irisdf |> filter(str_detect(Species, "ver", negate = TRUE))




