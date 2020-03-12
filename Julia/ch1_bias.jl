# (1) パッケージをインストールする（初回のみ）
## ] + addで

# (2) ライブラリの読み出し
## dplyr的に操作したいのでDataFramesMetaを使う
using DataFrames
using DataFramesMeta
using HypothesisTests
using RCall
@rlibrary readr

# (3) データの読み込み
## CSV.read()だとネット上のファイルは読み込めないのでRCallを使う。遅い。
email_data = rcopy(R"readr::read_csv('http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv')")

# (4) データの準備
## 女性向けメールが配信されたデータを削除したデータを作成
@linq male_df = email_data |>
  where(:segment .!= "Womens E-Mail") |> # 女性向けメールが配信されたデータを削除
  transform(treatment = ifelse.(:segment
  .== "Mens E-Mail", 1, 0)) #介入を表すtreatment変数を追加

# (5) 集計による比較
## byでgroupbyとtransformを同時にできる
@linq summary_by_segment = male_df |>
  by(:treatment,
  conversion_rate = mean(:conversion), # グループごとのconversionの平均
  spend_mean = mean(:spend), # グループごとのspendの平均
  count = length(:conversion)) # グループごとのデータ数

# (6) t検定を行う
## (a)男性向けメールが配信されたグループの購買データを得る
@linq mens_mail = male_df |>
  where(:treatment .== 1)

## (b)メールが配信されなかったグループの購買データを得る
@linq no_mail = male_df |>
  where(:treatment .== 0)

## (a)(b)の平均の差に対して有意差検定を実行する
## HypothesisTestsの等分散t検定を使う
rct_ttest = EqualVarianceTTest(mens_mail[:, :spend], no_mail[:, :spend])

# (7) セレクションバイアスのあるデータの作成
## seedを固定する
## 書籍とseedを合わせるためRCallを使う
R"set.seed(1)"

## 条件に反応するサンプルの量を半分にする
obs_rate_c = 0.5
obs_rate_t = 0.5

## バイアスのあるデータの作成
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

# (8) セレクションバイアスのあるデータで平均を比較
## group_byとsummairseを使って集計(Biased)
## byだけでできる
@linq summary_by_segment_biased = biased_data |>
  by(:treatment,
  conversion_rate = mean(:conversion),
  spend_mean = mean(:spend),
  count = length(:conversion))

# (9) Rの関数であるt.testを使ってt検定を行う(Biased)
## (a)男性向けメールが配信されたグループの購買データを得る
@linq mens_mail_biased = biased_data |>
  where(:treatment .== 1)

## (b)メールが配信されなかったグループの購買データを得る
@linq no_mail_biased = biased_data |>
  where(:treatment .== 0)

## (a)(b)の平均の差に対して有意差検定を実行
rct_ttest_biased = EqualVarianceTTest(mens_mail_biased[:,:spend], no_mail_biased[:,:spend])
