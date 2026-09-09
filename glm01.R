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

# モデル比較： AIC

model01 = glm(Petal.Width ~ 1, data = irisdf) # ヌルモデル (null model)
model02 = glm(Petal.Width ~ Species, data = irisdf)
model03 = glm(Petal.Width ~ Petal.Length, data = irisdf)
model04 = glm(Petal.Width ~ Petal.Length + Species, data = irisdf)
model05 = glm(Petal.Width ~ Petal.Length + Species + Petal.Length:Species, data = irisdf)

# AIC 一番低い値がもっともいいモデル
AIC(model01, model02, model03, model04, model05)
# model05 の AICが最も低いので選択する




