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

summary(model05) # 計数表

# モデルの診断

ggplot(irisdf) + 
  geom_point(aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_smooth(aes(x = Petal.Length, y = Petal.Width, color = Species),
              method = "glm", formula = y ~ x) +
  geom_smooth(aes(x = Petal.Length, y = Petal.Width),
              method = "glm", formula = y ~ x) 
  

irisdf2 = 
  irisdf |> 
  mutate(
    zansa = residuals(model05),
    predict = predict(model05)
  ) 

# 残渣・期待値のプロット
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = zansa,
      color = Species
    )
  ) +
  geom_hline(yintercept = 0)

# 残渣・期待値のプロット 2 
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = sqrt(abs(zansa)),
      color = Species
    )
  ) 

# QQプロット（残渣が正規分布に従うかを確認）
ggplot(irisdf2) + 
  geom_qq(aes(sample = zansa)) +
  geom_qq_line(aes(sample = zansa))



###################################
# 検討するモデル
# 分布：ガンマ
# リンク：log
# 応答変数： Petal.Width
# 説明変数： Petal.Length, Species
# モデル： Petal.Width ~ Petal.Length + Species + Petal.Length:Species

# モデル比較： AIC

model01 = glm(Petal.Width ~ 1, data = irisdf, family = Gamma("log")) # ヌルモデル (null model)
model02 = glm(Petal.Width ~ Species, data = irisdf, family = Gamma("log"))
model03 = glm(Petal.Width ~ Petal.Length, data = irisdf, family = Gamma("log"))
model04 = glm(Petal.Width ~ Petal.Length + Species, data = irisdf, family = Gamma("log"))
model05 = glm(Petal.Width ~ Petal.Length + Species + Petal.Length:Species, data = irisdf, family = Gamma("log"))

# AIC 一番低い値がもっともいいモデル
AIC(model01, model02, model03, model04, model05)
# model05 の AICが最も低いので選択する

# qresiduals: ランダム化残渣
# glm（正規分布以外の） の残渣に使う残渣
irisdf2 = 
  irisdf |> 
  mutate(
    zansa = statmod::qresiduals(model05),
    predict = predict(model05)
  )

# 残渣・期待値のプロット
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = zansa,
      color = Species
    )
  ) +
  geom_hline(yintercept = 0)

# 残渣・期待値のプロット 2 
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = sqrt(abs(zansa)),
      color = Species
    )
  ) 

# QQプロット（残渣が正規分布に従うかを確認）
ggplot(irisdf2) + 
  geom_qq(aes(sample = zansa)) +
  geom_qq_line(aes(sample = zansa))

#########################################
ggplot(irisdf) + 
  geom_point(
    aes(
      x = Petal.Length,
      y = Petal.Width,
      color = Species
    )
  )


fullmodel = glm(Petal.Width ~ Petal.Length * Sepal.Length * Species, data = irisdf, family = Gamma("log"))
model01 = glm(Petal.Width ~ Petal.Length + Sepal.Length + Species, data = irisdf, family = Gamma("log"))
model02 = glm(Petal.Width ~ Petal.Length * Sepal.Length + Species, data = irisdf, family = Gamma("log"))
model03 = glm(Petal.Width ~ (Petal.Length + Sepal.Length) * Species, data = irisdf, family = Gamma("log"))

AIC(fullmodel, model01, model02, model03)

# model02:
# PW ~ PL + SL + Species + PL:SL


# qresiduals: ランダム化残渣
# glm（正規分布以外の） の残渣に使う残渣
irisdf2 = 
  irisdf |> 
  mutate(
    zansa = statmod::qresiduals(model03),
    predict = predict(model03)
  )

# 残渣・期待値のプロット
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = zansa,
      color = Species
    )
  ) +
  geom_hline(yintercept = 0)

# 残渣・期待値のプロット 2 
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = sqrt(abs(zansa)),
      color = Species
    )
  ) 

# QQプロット（残渣が正規分布に従うかを確認）
ggplot(irisdf2) + 
  geom_qq(aes(sample = zansa)) +
  geom_qq_line(aes(sample = zansa))



#########################################
ggplot(irisdf) + 
  geom_point(
    aes(
      y = (Petal.Length),
      x = (Petal.Width),
      color = Species
    )
  )

irisdf = irisdf |> mutate(PW2 = Petal.Width, PL2 = Petal.Length)

fullmodel = glm(PL2 ~ PW2 * Species, data = irisdf, family = Gamma("log"))
model01   = glm(PL2 ~ PW2 + Species, data = irisdf, family = Gamma("log"))

AIC(fullmodel, model01)

# fullmodel:
# PL2 ~ PW2 + Species PW2:Species


# qresiduals: ランダム化残渣
# glm（正規分布以外の） の残渣に使う残渣
irisdf2 = 
  irisdf |> 
  mutate(
    zansa = statmod::qresiduals(fullmodel),
    predict = predict(fullmodel)
  )

# 残渣・期待値のプロット
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = zansa,
      color = Species
    )
  ) +
  geom_hline(yintercept = 0)

# 残渣・期待値のプロット 2 
ggplot(irisdf2) + 
  geom_point(
    aes(
      x = predict, 
      y = sqrt(abs(zansa)),
      color = Species
    )
  )+
  geom_smooth(
    aes(
      x = predict, 
      y = sqrt(abs(zansa))
    )
  )

# QQプロット（残渣が正規分布に従うかを確認）
ggplot(irisdf2) + 
  geom_qq(aes(sample = zansa)) +
  geom_qq_line(aes(sample = zansa))

summary(fullmodel)


# モデルとデータの図

## モデル線を描くための情報を整える
pdata = irisdf2 |> 
  group_by(Species) |> 
  expand(Petal.Width = seq(min(Petal.Width), max(Petal.Width), length = 21)) |> 
  mutate(PW2 = Petal.Width) |> 
  ungroup()
tmp = predict(fullmodel, newdata = pdata, se.fit = TRUE, type = "link") |> 
  as_tibble()
pdata = bind_cols(pdata, tmp)


ggplot() +
  geom_point(
    aes(
      x = Petal.Width,
      y = Petal.Length,
      color = Species
    ),
    data = irisdf2
  ) +
  geom_line(
    aes(
      x = Petal.Width,
      y = exp(fit),
      color = Species
    ),
    data = pdata
  ) +
  geom_ribbon(
    aes(
      x = Petal.Width,
      ymin = exp(fit - 1.96 * se.fit),
      ymax = exp(fit + 1.96 * se.fit),
      fill = Species
    ),
    data = pdata,
    alpha = 0.5
  ) +
  scale_x_continuous("Petal width (cm)", limits = c(0, 3)) +
  scale_y_continuous("Petal length (cm)", limits = c(0, 8)) +
  scale_color_discrete(
    breaks = c("setosa", "versicolor", "virginica"),
    labels = c("I. setosa", "I. versicolor", "I. virginica")
  ) +
  guides(fill = "none") +
  theme(
    legend.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0,1),
    legend.justification = c(0,1),
    legend.title = element_blank()
  )

summary(fullmodel)


