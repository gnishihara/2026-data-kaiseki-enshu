# 一般化線形混合モデル
# Generalized Linear Mixed-Models (GLMM)
# Greg Nishihara
# 2026-09-11

# パッケージの読み込み
library(tidyverse)
library(lme4) # 一般化線形混合モデル用パッケージ
library(emmeans)


# 疑似データの生成

# シードを設定する
set.seed(2026) # 疑似乱数のシード (seed) を固定する

# タンク（水槽）　ランダム効果 (Random Effect)
# 合計 12 タンク: 6 コントロール　６ 高水温

tanks = tibble(
  tank = str_c("tank_", str_pad(1:12, 2, pad = "0")),
  treatment = rep(c("control", "high_temp"), each = 6),
  tank_effect = rnorm(12, mean = 0, sd = 0.8)
)
tanks
ggplot(tanks) + geom_point(aes(x = treatment, y = tank_effect), position = position_jitter(width = 0.2))

seaweeddf = 
  tanks |> 
  uncount(30) |> 
  mutate(
    shubyoid = row_number(),
    fixed_effect = if_else(str_detect(treatment, "Con"), 1, -0.5),
    true_prob = plogis(fixed_effect + tank_effect),
    survival = rbinom(n(), size = 1, prob = true_prob)
  ) |> 
  select(shubyoid, tank, treatment, survival) |> 
  mutate(
    tank = as_factor(tank),
    treatment = fct_relevel(as_factor(treatment), "control")
  )

seaweeddf |> 
  group_by(tank, treatment) |> 
  reframe(total = sum(survival)) |> 
  ggplot() + 
  geom_point(aes(x = treatment, y = total), position = position_jitter(width = 0.1))

## GLM 解析 (ランダム効果を無視した解析・水槽ごとの微妙な違いを無視したときの結果)
m01 = glm(survival ~ treatment, data = seaweeddf, family = binomial("logit"))
summary(m01)
eout = emmeans(m01, ~ treatment, type = "response")
contrast(eout, "pairwise")

## GLMM 
g01 = glmer(survival ~ treatment + (1 | tank), 
            data = seaweeddf,
            family = binomial("logit"))
summary(g01)
eout = emmeans(g01, ~ treatment, type = "response")
contrast(eout, "pairwise")





