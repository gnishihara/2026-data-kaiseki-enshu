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

# Petal.Length: versicolor vs virginica
t.test(Petal.Length ~ Species, data = df1)
# ウェルチのt検定を行った結果、virginica の花弁の長さ (5.55) は、versicolor (4.26) よりも有意に違うことが示された (t(95.57) = -12.60, p < 0.001)。

# Petal.Length: setosa vs virginica
t.test(Petal.Length ~ Species, data = df3)

# Petal.Length: setosa vs versicolor
t.test(Petal.Length ~ Species, data = df2)

# 花弁の長さは種によって異なる
# 一元配置分散分析 (Analysis of Variance; ANOVA)
# 応答変数 ~ 説明変数 R モデルの構造
fullmodel = lm(Petal.Length ~ Species, data = irisdf)

ggplot(irisdf) + geom_boxplot(aes(x = Species, y = Petal.Length))

summary(fullmodel)

#一元配置分散分析（または線形回帰モデル）の結果、アヤメの種（Species）は花弁の長さに対して極めて有意な主効果を示した (F(2, 147) = 1180, p < 0.001, R^2(adj) = 0.94)。

