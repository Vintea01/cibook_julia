# (1) 利用するデータを持つライブラリのインストール（初回のみ）
## ] + addで
# using RCall
# @rlibrary devtools
# R"devtools::install_github('itamarcaspi/experimentdatar')"

# (2) ライブラリとデータの読み込み
using DataFrames
using DataFramesMeta
using GLM
using RCall
@rimport tidyr
@rlibrary experimentdatar
vouchers = rcopy(R"experimentdatar::vouchers")

## 可視化用。@rlibraryでは都度ライブラリの指定が必要なのでR"library()"でインポートする
R"library(ggplot2)"
R"library(magrittr)"

## broom::tidyと同じ働きをする関数を自作
function broom_tidy_julia(model)
  table = coeftable(model)
  return DataFrame(
    term = coefnames(model),
    estimate = table.cols[1],
    stderror = table.cols[2],
    statistic = table.cols[3],
    pvalue = table.cols[4],
  )
end

# (3) Angrist(2002)のTable 3. bogota 1995の再現
## 回帰式の準備
### 回帰式で使う文字列の準備
formula_x_base = :VOUCH0
formula_x_covariates = [
  :SVY,
  :HSVISIT,
  :AGE,
  :STRATA1,
  :STRATA2,
  :STRATA3,
  :STRATA4,
  :STRATA5,
  :STRATA6,
  :STRATAMS,
  :D1993,
  :D1995,
  :D1997,
  :DMONTH1,
  :DMONTH2,
  :DMONTH3,
  :DMONTH4,
  :DMONTH5,
  :DMONTH6,
  :DMONTH7,
  :DMONTH8,
  :DMONTH9,
  :DMONTH10,
  :DMONTH11,
  :DMONTH12,
  :SEX2,
]
formula_y = [
  :TOTSCYRS,
  :INSCHL,
  :PRSCH_C,
  :USNGSCH,
  :PRSCHA_1,
  :FINISH6,
  :FINISH7,
  :FINISH8,
  :REPT6,
  :REPT,
  :NREPT,
  :MARRIED,
  :HASCHILD,
  :HOURSUM,
  :WORKING3,
]

### formula_yの各要素に対して共変量を含まない回帰式を作成
base_reg_formula =
  [FormulaTerm(Term(y), Term(formula_x_base)) for y in formula_y]
base_reg_formula_names = String.(formula_y) .* "_base"

### formula_yの各要素に対して共変量を含む回帰式を作成
covariate_reg_formula = [
  FormulaTerm(
    Term(y),
    Tuple(Term.(vcat(formula_x_base, formula_x_covariates))),
  ) for y in formula_y
]
covariate_reg_formula_names = String.(formula_y) .* "_covariate"

### モデル式のベクトルを作成
### 直接データフレーム化するのでスキップ

### モデル式のベクトルをデータフレーム化する
models = DataFrame(
  model_index = vcat(base_reg_formula_names, covariate_reg_formula_names),
  formula = vcat(base_reg_formula, covariate_reg_formula),
)

## 回帰分析を実行
### bogota 1995のデータを抽出する
@linq regression_data = vouchers |> where(:TAB3SMPL .== 1, :BOG95SMP .== 1)

### まとめて回帰分析を実行
@linq df_models =
  models |> transform(
    model = map(formula -> lm(formula, regression_data, true), :formula),
  ) |> transform(lm_result = map(broom_tidy_julia, :model))

### モデルの結果を整形
@linq df_results =
  df_models |> select(:formula, :model_index, :lm_result) |>
  tidyr.unnest(cols = :lm_result) |> rcopy()

# ------------------
## 通学率と奨学金の利用傾向の可視化(ch2_plot2.png)
### PRSCHA_1, USNGSCHに対するVOUCH0の効果を取り出す
@linq using_voucher_results =
  df_results |> where(
    :term .== "VOUCH0",
    match.(r"PRSCHA_1|USNGSCH", :model_index) .!= nothing,
  ) |> select(:model_index, :term, :estimate, :stderror, :pvalue) |>
  orderby(:model_index)

### 取り出した効果をggplotで可視化
### RCallでggplot2にObjectを渡してやればいい
R"$(using_voucher_results) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + stderror*1.96,
                    ymin = estimate - stderror*1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        plot.margin = margin(0.5,1,0.5,1, 'cm'))"

## 留年の傾向を可視化
### PRSCH_C,INSCHL,FINISH6-8,REPTに対するVOUCH0の効果を取り出す
@linq going_private_results =
  df_results |> where(
    :term .== "VOUCH0",
    match.(r"PRSCH_C|INSCHL|FINISH|REPT", :model_index) .!= nothing,
  ) |> select(:model_index, :term, :estimate, :stderror, :pvalue) |>
  orderby(:model_index)

### 取り出した効果をggplotで可視化
going_private_results_gg =
  @where(going_private_results, match.(r"covariate", :model_index) .!= nothing)

R"$(going_private_results_gg) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + stderror*1.96,
                    ymin = estimate - stderror*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        plot.margin = margin(0.5,1,0.5,1, 'cm'))"

# (4) Angrist(2002)のTable.4 & 6 bogota 1995の再現
## table4に使うデータを抜き出す
@linq data_tbl4_bog95 =
  vouchers |> where(
    :BOG95SMP .== 1,
    :TAB3SMPL .== 1,
    :SCYFNSH .!== missing,
    :FINISH6 .!== missing,
    :PRSCHA_1 .!== missing,
    :REPT6 .!== missing,
    :NREPT .!== missing,
    :INSCHL .!== missing,
    :FINISH7 .!== missing,
    :PRSCH_C .!== missing,
    :FINISH8 .!== missing,
    :PRSCHA_2 .!== missing,
    :TOTSCYRS .!== missing,
    :REPT .!== missing,
  ) |> select(
    :VOUCH0,
    :SVY,
    :HSVISIT,
    :DJAMUNDI,
    :PHONE,
    :AGE,
    :STRATA1,
    :STRATA2,
    :STRATA3,
    :STRATA4,
    :STRATA5,
    :STRATA6,
    :STRATAMS,
    :DBOGOTA,
    :D1993,
    :D1995,
    :D1997,
    :DMONTH1,
    :DMONTH2,
    :DMONTH3,
    :DMONTH4,
    :DMONTH5,
    :DMONTH6,
    :DMONTH7,
    :DMONTH8,
    :DMONTH9,
    :DMONTH10,
    :DMONTH11,
    :DMONTH12,
    :SEX_MISS,
    :FINISH6,
    :FINISH7,
    :FINISH8,
    :REPT6,
    :REPT,
    :NREPT,
    :SEX2,
    :TOTSCYRS,
    :MARRIED,
    :HASCHILD,
    :HOURSUM,
    :WORKING3,
    :INSCHL,
    :PRSCH_C,
    :USNGSCH,
    :PRSCHA_1,
  )

## 女子生徒のみのデータでの回帰分析
### 女子生徒のデータだけ取り出す
@linq regression_data = data_tbl4_bog95 |> where(:SEX2 .== 0)

### まとめて回帰分析を実行
@linq df_models =
  models |> transform(
    model = map(formula -> lm(formula, regression_data, true), :formula),
  ) |> transform(lm_result = map(broom_tidy_julia, :model))

### モデルの結果を整形
@linq df_results_female =
  df_models |> transform(gender = fill("female", length(:model_index))) |>
  select(:formula, :model_index, :lm_result, :gender) |>
  tidyr.unnest(cols = :lm_result) |> rcopy()

## 男子生徒のみのデータでの回帰分析
@linq regression_data = data_tbl4_bog95 |> where(:SEX2 .== 1)

### まとめて回帰分析を実行
@linq df_models =
  models |> transform(
    model = map(formula -> lm(formula, regression_data, true), :formula),
  ) |> transform(lm_result = map(broom_tidy_julia, :model))

### モデルの結果を整形
@linq df_results_male =
  df_models |> transform(gender = fill("male", length(:model_index))) |>
  select(:formula, :model_index, :lm_result, :gender) |>
  tidyr.unnest(cols = :lm_result) |> rcopy()

## 通学傾向への分析結果の可視化(ch2_plot3.png)
### PRSCHA_1,USNGSCHに対する分析結果を抜き出す
@linq using_voucher_results_gender =
  vcat(df_results_male, df_results_female) |> where(
    :term .== "VOUCH0",
    match.(r"PRSCHA_1|USNGSCH", :model_index) .!= nothing,
  ) |> select(:gender, :model_index, :term, :estimate, :stderror, :pvalue) |>
  orderby(:gender, :model_index) |>
  where(match.(r"covariate", :model_index) .!= nothing)

### ggplotによる可視化
R"$(using_voucher_results_gender) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + stderror*1.96,
                    ymin = estimate - stderror*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position ='bottom',
        plot.margin = margin(0.5,1,0.5,1, 'cm')) +
  facet_grid(gender ~ .)"

## 留年と通学年数への分析結果の可視化(ch2_plot4.png)
### PRSCH_C,INSCHL,REPT,TOTSCYRS,FINISHに対する分析結果を抜き出す
@linq going_private_results_gender =
  vcat(df_results_male, df_results_female) |> where(
    :term .== "VOUCH0",
    match.(r"PRSCH_C|INSCHL|REPT|TOTSCYRS|FINISH", :model_index) .!= nothing,
  ) |> select(:gender, :model_index, :term, :estimate, :stderror, :pvalue) |>
  orderby(:model_index) |> where(match.(r"covariate", :model_index) .!= nothing)

### ggplotによる可視化
R"$(going_private_results_gender) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + stderror*1.96,
                    ymin = estimate - stderror*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        plot.margin = margin(0.5,1,0.5,1, 'cm')) +
  facet_grid(gender ~ .)"

## 労働時間に対する分析結果の可視化(ch2_plot5.png)
### HOURに対する分析結果を抜き出す
@linq working_hour_results_gender =
  vcat(df_results_male, df_results_female) |>
  where(:term .== "VOUCH0", match.(r"HOUR", :model_index) .!= nothing) |>
  select(:gender, :model_index, :term, :estimate, :stderror, :pvalue) |>
  orderby(:gender, :model_index) |>
  where(match.(r"covariate", :model_index) .!= nothing)

### ggplotによる可視化
R"$(working_hour_results_gender) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + stderror*1.96,
                    ymin = estimate - stderror*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        plot.margin = margin(0.5,1,0.5,1, 'cm')) +
  facet_grid(. ~ gender)"
