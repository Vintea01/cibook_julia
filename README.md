# cibook

## 効果検証入門

技術評論社発行の書籍のサンプルコード(R)をJulia化したものです。

https://gihyo.jp/book/2020/978-4-297-11117-5

## 実行環境

julia> versioninfo()
Julia Version 1.3.0
Commit 46ce4d7933 (2019-11-26 06:09 UTC)
Platform Info:
  OS: macOS (x86_64-apple-darwin19.0.0)
  CPU: Intel(R) Core(TM) i5-8210Y CPU @ 1.60GHz
  WORD_SIZE: 64
  LIBM: libopenlibm
  LLVM: libLLVM-6.0.1 (ORCJIT, skylake)
Environment:
  JULIA_EDITOR = atom  -a
  JULIA_NUM_THREADS = 2

(v1.3) pkg> status RCall DataFrames DataFramesMeta GLM HypothesisTests
    Status `~/.julia/environments/v1.3/Project.toml`
  [a93c6f00] DataFrames v0.20.2
  [1313f7d8] DataFramesMeta v0.5.0
  [31c24e10] Distributions v0.22.5
  [38e38edf] GLM v1.3.7
  [09f84164] HypothesisTests v0.9.1
  [6f49c342] RCall v0.13.4
  [276daf66] SpecialFunctions v0.10.0
  [2913bbd2] StatsBase v0.32.2
