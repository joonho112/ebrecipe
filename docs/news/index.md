# Changelog

## ebrecipe 0.5.0

본 release 는 v0.5.0 (pre-release, experimental) 으로, 10 vignette +
완전 재작성된 README + pkgdown site + hex sticker 를 포함하는
documentation overhaul 의 산출물이다. R CMD check 0 ERROR / 0 WARN / 1
NOTE (environment-related, harmless), pkgdown 87 HTML / 4,666 internal
link 0 broken, 47/47 export coverage, frozen-core 12 file SHA256 lock
HELD. Pre-release 단계로, 정식 1.0.0 release 는 precomputed cache 도입
및 CRAN-ready submission 시점까지 보류.

### Major changes

- **10-vignette 구조 확립.** 응용 트랙 5 편 (`a1-getting-started` →
  `a5-visualization-cookbook`) + 방법론 트랙 5 편
  (`m1-eb-recipe-foundations` → `m5-replication-and-reproducibility`) 의
  dual-track 구조로 전면 재편성. 각 vignette 은 ~165–250 source line 의
  자기-완결적 walkthrough 로, 응용 트랙은 workflow 중심 (estimate →
  denoise → decide), 방법론 트랙은 수식 + 증명 + 검증 invariant 중심.
- **Hex sticker 채택.** EB shrinkage 의 시각적 은유 (raw dots →
  posterior cluster) 를 표현한 디자인. `man/figures/logo.svg` +
  `man/figures/logo.png` 양쪽 형식 제공.
- **pkgdown site.** `_pkgdown.yml` 신규 작성, 47 export reference index,
  10 vignette articles, navbar 2-track grouping.
  `https://joonho112.github.io/ebrecipe/` 으로 deploy 가능.
- **`cd78_selection_count()` 단일 source of truth 도입.** CD-78
  canonical 27 (DEC-197-2) 결정값을 helper 함수로 추출하여
  `vignettes/_setup.R`, `vignettes/companion-helpers.R`, `inst/scripts/`
  의 mirror 위치에 byte-identical 으로 유지. Vignette + README + test 가
  모두 동일한 정수를 참조.

### Breaking changes

- **기존 4 vignette 삭제.** `vignettes/ebrecipe.Rmd`,
  `discrimination.Rmd`, `school-vam.Rmd`, `visualization.Rmd` 가 모두
  제거되고 신규 a1–a5 / m1–m5 으로 대체됨. 외부에서 위 vignette URL 을
  직접 참조하던 경우 신규 slug 로 redirect 필요.
- **DESCRIPTION Suggests 축소.** 미사용 의존성 `ggdist`, `rlang`,
  `vctrs` 3 개를 Suggests 에서 제거.

### New features

- **`cd78_selection_count()`** — CD-78 canonical 27 을 단일 정수로
  반환하는 헬퍼. `vignettes/_setup.R` +
  `vignettes/companion-helpers.R` + `inst/scripts/` 3 위치에
  byte-identical 으로 유지. 신규 테스트
  `tests/testthat/test-cd78-invariant.R` 가 `expect_identical(., 27L)`
  과 mirror parity 양쪽을 검증.
- **`_pkgdown.yml`** — pkgdown site configuration. 2-track navbar,
  custom palette, 47 reference grouping, articles index.
- **`vignettes/_setup.R` + `companion-helpers.R`** — 10 vignette 공유
  preamble.
  [`library(ebrecipe)`](https://github.com/joonho112/ebrecipe), ggplot2
  theme, dataset fixture path, helper 함수 등 일괄 제공.
- **`vignettes/references.bib`** — 17 entry bibliography. Walters
  (2024), Efron (2012, 2014, 2019), Stone (1990), Koenker–Mizera (2014),
  Kline–Walters (2021), Chetty–Friedman–Rockoff (2014) 등 핵심 인용
  정비.

### Bug fixes

- **README 5-분 예제 정상화.** 기존
  `eb_classify(..., posterior = fit$posterior)` 가 dim/scale mismatch 로
  즉시 에러하던 문제를 stepwise pipeline
  (`eb_deconvolve → eb_shrink → eb_classify`) 으로 교체. README 의 인용
  단락이 그대로 복사하여 R session 에서 실행 가능.
- **m4 vignette CD-78 chunk 정정.** m4 의 selected-units chunk 가 `19`
  를 산출하면서 같은 페이지에서 `Expected: 27` 을 표시하던 모순을
  stepwise `eb_classify(method = "qvalue")` 로 교정.
  `stopifnot(identical(n_selected, cd78_selection_count()))` 로 strict
  검증.
- **`vignettes/references.bib` DOI 정정.** 4 entry 의 DOI 가 unrelated
  paper 로 resolve 되던 문제를 정정 (KRW QJE 2022, Angrist et al. QJE
  2017, Laird JASA 1978, Efron Biometrika 2014).
- **`R/plot-mixing-distribution.R` `@examples`**: `scale = "theta"`
  (없는 레벨) 를 `scale = "r"` 로 수정.
- **`R/plot-posterior-overlay.R` `@examples`**: `\donttest{}` 블록을
  `\dontrun{}` 로 교체 (R CMD check policy).
- **`tests/testthat/test-frozen-checksums.R`**: 설치된 패키지 컨텍스트
  (source `R/` 부재) 에서 graceful skip 처리.

### Documentation

- **10 신규 vignette** (~2,532 LOC) — Applied track `a1`–`a5` (Getting
  Started / Discrimination Workflow / School VAM Workflow / Diagnostics
  / Visualization Cookbook) + Methodological track `m1`–`m5` (EB Recipe
  Foundations / Linear EB Normal-Normal / Logspline Deconvolution /
  Precision Dependence and FDR / Replication and Reproducibility).
- **README.md 완전 재작성** — 270 line, 8+1 section (Overview, Install,
  5-minute example, Two interfaces, Track structure, Data, Status &
  scope, References, BibTeX) + hex header + 작동하는 5-분 예제.
- **Hex sticker** — `man/figures/logo.svg` + `logo.png`, EB shrinkage
  convergence metaphor. README header / pkgdown navbar 에 일관 적용.
- **pkgdown site** — 87 HTML 페이지 (10 vignettes + index + 69
  reference + auxiliary), 4,666 internal link 모두 정상, sitemap clean.
- **Bibliography 정비** — `vignettes/references.bib` 17 entries.

### Internal

- **Frozen-core HELD**. 12 source file 의 SHA256 잠금이 byte-stable 로
  유지됨. `inst/locked-core-checksums.txt` +
  `tests/testthat/test-frozen-checksums.R`.
- **47-entry export ledger parity**. NAMESPACE ↔︎ `man/*.Rd` ↔︎
  `docs/reference/` 의 3-way parity 가 47/47/47 로 검증됨
  (`test-namespace-ledger.R`).
- **Build hygiene** — `.Rbuildignore` 에 internal artifact 디렉터리 제외
  entries 추가. 미사용 Suggests 3 개 제거.
- **Helper mirror** — `vignettes/_setup.R` 와 `companion-helpers.R` 의
  정본을 `inst/scripts/` 에도 복사하여 vignette 빌드 시
  [`system.file()`](https://rdrr.io/r/base/system.file.html) 로 발견
  가능. parity guard 테스트 신설.
- **Test suite 확장** — 2,395 PASS / 0 FAIL / 18 SKIP. 신규
  `test-cd78-invariant.R`, `test-namespace-ledger.R`,
  `test-companion-parity-*.R`, `test-source-receipt.R`,
  `test-figure-data.R` 등.
- **Vignette build timing** — 총 233.5s (개별 vignette 별 1–91s 범위).
  차기 release 에서 precomputed cache 도입 시 단축 예정.
