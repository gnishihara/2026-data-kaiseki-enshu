# 一般化加法モデル (Generalized Additive Models; GAM)
# 2026-09-10
# Greg Nishihara


# パッケージの読み込み
library(tidyverse)
library(mgcv)  　# GAM解析用のパッケージ
library(gratia)　# GAMの結果を確認するためのパッケージ

orangedf = Orange |> as_tibble()

ggplot(orangedf) + 
  geom_point(
    aes(
      x = age,
      y = circumference,
      color = Tree
    )
  )

# まずは GLM 解析をする

orangedf = orangedf |> mutate(circ = circumference)

m1 = glm(circ ~ age * Tree, data = orangedf, family = Gamma("log"))
m2 = glm(circ ~ age, data = orangedf, family = Gamma("log"))

AIC(m1, m2) # 低いAICのモデルのほうがいい

# 診断図

orangedf = orangedf |> 
  mutate(
    zansa = statmod::qresiduals(m1),
    predict = predict(m1)
    )

# 図を二つ：　sqrt(abs(zansa))プロットとQQプロット

ggplot(orangedf) +
  geom_point(
    aes(x = predict, y = sqrt(abs(zansa)), color = Tree)
  ) +
  labs(
    subtitle = "標準化したランダム化残渣プロット"
  )

ggplot(orangedf) +
  geom_qq(aes(sample = zansa)) +
  geom_qq_line(aes(sample = zansa)) +
  labs(subtitle = "QQプロット")

ggplot(orangedf) +
  geom_point(
    aes(x = age, y = circ, color = Tree)
  ) +
  geom_line(
    aes(x = age, y = exp(predict), color = Tree)
  ) +
  labs(subtitle = "観測値と期待値のプロット") +
  facet_wrap(vars(Tree))






