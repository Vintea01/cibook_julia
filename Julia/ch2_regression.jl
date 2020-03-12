# (1) パッケージをインストールする（初回のみ）
## ] + addで

# (2) ライブラリの読み出し
using DataFrames
using DataFramesMeta
using GLM
using RCall
@rlibrary readr
@rimport tidyr
@rlibrary broom

# (3) データの読み込み
email_data = rcopy(R"readr::read_csv('http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv')")

# (4) 女性向けメールが配信されたデータを削除したデータを作成
@linq male_df = email_data |>
  where(:segment .!= "Womens E-Mail") |> # 女性向けメールが配信されたデータを削除
  transform(treatment = ifelse.(:segment
  .== "Mens E-Mail", 1, 0)) #介入を表すtreatment変数を追加

# (5) セレクションバイアスのあるデータを作成
## seedを固定
R"set.seed(1)"

## 条件に反応するサンプルの量を半分にする
obs_rate_c = 0.5
obs_rate_t = 0.5

## バイアスのあるデータを作成
@linq biased_data = male_df |>
  transform(
    obs_rate_c = ifelse.(
      (:history .> 300) .| (:recency .< 6) .| (:channel .== "Multichannel"),
      obs_rate_c, 1
    ),
    obs_rate_t = ifelse.(
      (:history .> 300) .| (:recency .< 6) .| (:channel .== "Multichannel"),
      1, obs_rate_t
    ),
    random_number = rcopy(R"runif($(nrow(male_df)))") # 乱数を合わせるためRに作らせる
  ) |>
  where(((:treatment .== 0) .& (:random_number .< :obs_rate_c)) .| ((:treatment .== 1) .& (:random_number .< :obs_rate_t)))

# (6) バイアスのあるデータでの回帰分析
## 回帰分析の実行
## JuliaのGLM.jlの結果をRobjectに変換することは現状不可能。逆はOrderedDictになる
biased_reg = lm(@formula(spend ~ treatment + history), biased_data)
# robject(biased_reg) # エラーになる
biased_reg_R = R"summary(lm(formula = spend ~ treatment + history, $biased_data))"
rcopy(biased_reg_R) # OrderedDictになる

## 分析結果のレポート
## lmのsummaryに比べ情報量が少ない。
println(biased_reg)
println(R"summary($biased_reg_R)")

## 推定されたパラメーターの取り出し
## coeftableでできるがbroomのようにデータフレームにはしてくれない。
biased_reg_coef = coeftable(biased_reg)

biased_reg_coef_R = rcopy(R"broom::tidy($biased_reg_R)")

## broom::tidyと同じ働きをする関数を自作
function broom_tidy_julia(model)
  table = coeftable(model)
  return DataFrame(
  term = coefnames(model),
  estimate = table.cols[1],
  stderror = table.cols[2],
  statistic = table.cols[3],
  pvalue = table.cols[4]
  )
end

broom_tidy_julia(biased_reg)

# (7) RCTデータでの回帰分析とバイアスのあるデータでの回帰分析の比較
## RCTデータでの単回帰
rct_reg = lm(@formula(spend ~ treatment), male_df)
rct_reg_coef = broom_tidy_julia(rct_reg)

## バイアスのあるデータでの単回帰
nonrct_reg = lm(@formula(spend ~ treatment), biased_data
)
nonrct_reg_coef = broom_tidy_julia(nonrct_reg)

## バイアスのあるデータでの重回帰
nonrct_mreg = lm(@formula(spend ~ treatment + recency + channel + history), biased_data)
nonrct_mreg_coef = broom_tidy_julia(nonrct_mreg)

# (8) OVBの確認
## (a) history抜きの回帰分析とパラメーターの取り出し
short_coef = biased_data |>
  data -> lm(@formula(spend ~ treatment + recency + channel), data) |>
  broom_tidy_julia

## aの結果から介入効果に関するパラメーターのみを取り出す
## pullがないので少し面倒
@linq alpha_1_p = short_coef |>
  where(:term .== "treatment") |>
  select(:estimate)
alpha_1 = alpha_1_p[1,1]

## (b) historyを追加した回帰分析とパラメーターの取り出し
long_coef = biased_data |>
  data -> lm(@formula(spend ~ treatment + recency + channel + history), data) |>
  broom_tidy_julia

## bの結果から介入とhistoryに関するパラメーターを取り出す
@linq beta_1_p = long_coef |> where(:term .== "treatment") |> select(:estimate)
beta_1 = beta_1_p[1,1]
@linq beta_2_p = long_coef |> where(:term .== "history") |> select(:estimate)
beta_2 = beta_2_p[1,1]

## (c) 脱落した変数と介入変数での回帰分析
omitted_coef = biased_data |>
  data -> lm(@formula(history ~ treatment + channel + recency), data) |>
  broom_tidy_julia
## cの結果から介入変数に関するパラメーターを取り出す
@linq gamma_1_p = omitted_coef |>
  where(:term .== "treatment") |>
  select(:estimate)
gamma_1 = gamma_1_p[1,1]

## OVBの確認
beta_2*gamma_1
alpha_1 - beta_1

# (9) OVBの確認(broomを利用した場合)
## formulaをデータフレーム化しまとめて処理してみる
## モデル式のベクトルを用意
formula_vec = [@formula(spend ~ treatment + recency + channel), # モデルA
               @formula(spend ~ treatment + recency + channel + history), # モデルB
               @formula(history ~ treatment + channel + recency)
               ] # モデルC

## formulaに名前を付ける
names_formula_vec = ["reg" * "_" * letter for letter in 'A':'C']

## モデル式のデータフレーム化
models = DataFrame(model_index = names_formula_vec, formula = formula_vec)

## まとめて回帰分析を実行
@linq df_models = models |>
  transform(model = map(formula -> lm(formula, biased_data), :formula)) |>
  transform(lm_result = map(model -> broom_tidy_julia(model), :model))

## モデルの結果を整形
## formulaは元から文字列のように表示されるのでそのまま
## unnestはJuliaにはないのでRCallで
@linq df_results = df_models |>
  select(:formula, :model_index, :lm_result) |>
  robject() |>
  tidyr.unnest(cols=:lm_result) |>
  rcopy()

## モデルA,B,Cでのtreatmentのパラメータを抜き出す
@linq treatment_coef = df_results |>
  where(:term .== "treatment") |>
  select(:estimate)

## モデルBからhistoryのパラメータを抜き出す
@linq history_coef = df_results |>
  where(:model_index .== "reg_B",
         :term .== "history") |>
  select(:estimate)

## OVBの確認
OVB = history_coef[1,1]*treatment_coef[3,1]
coef_gap = treatment_coef[1,1] - treatment_coef[2,1]
OVB # beta_2*gamma_1
coef_gap # alpha_1 - beta_1

# (10) 入れてはいけない変数を入れてみる
#visitとtreatmentとの相関
cor_visit_treatment = lm(@formula(treatment ~ visit + channel + recency + history), biased_data) |>
  broom_tidy_julia

# visitを入れた回帰分析を実行
bad_control_reg = lm(@formula(spend ~ treatment + channel + recency + history + visit), biased_data) |>
  broom_tidy_julia
