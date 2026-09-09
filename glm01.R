# 一般化線形モデルの解析
# Generalized Linear Model
# 2026-09-09
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(emmeans)
library(marginaleffects)

irisdf = iris |> as_tibble()

ggplot(irisdf) + 
  geom_point(
    aes(
      x = Petal.Length,
      y = Petal.Width,
      color = Species
    )
  )

###################################
# 検討するモデル
# 応答変数： Petal.Width
# 説明変数： Petal.Length, Species
# モデル： Petal.Width ~ Petal.Length + Species + Petal.Length:Species



