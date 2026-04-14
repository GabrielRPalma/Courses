# Day 2 — Fundamentals of Time Series Analysis I

> **Course:** METR01 — Machine Learning for Ecological Time Series  
> **Instructor:** Gabriel Rodrigues Palma  
> **Date:** Tuesday, April 14, 2026 | 9:30–17:30  
> **Live Q&A:** Tuesday 19:00–20:30

---

## Day Overview

Day 2 opens the time series module. Students move from R foundations (Day 1) into the world of temporal data: how to represent it in R, how to visualise and transform it, and how to build the first classical forecasting models. The ecological thread — monitoring butterfly populations in an Irish meadow — runs through both sessions, grounding every concept in a realistic field-science scenario.

By the end of Day 2, students have built, diagnosed, and forecasted with ARIMA and ARIMAX models, and can evaluate their accuracy against benchmark methods.

---

## Session 3 — The Time Series Setting (9:30–12:30)

### Topic 1: The Structure of a Time Series in Ecology (9:30–10:30)

**Story:** A field ecologist recording butterfly abundance at 12 sites for 10 years. The notebook is a time series — how does R understand its temporal structure?

**Subtopics covered:**
- What is a time series? — Definition, index structure, regularity
- The `tsibble` object — Creating, converting, keys, index
- Time plots — `autoplot()` and visual interpretation
- Seasonal plots — `gg_season()`, `gg_subseries()`
- Lag plots — `gg_lag()` for uncovering seasonal structure
- ACF — `ACF()` and autocorrelation interpretation

**What students learn:**
Students understand why the ORDER of observations matters and how `tsibble` makes R time-aware. They can produce and interpret time plots, seasonal overlays, lag plots, and ACF plots for ecological data. They learn to read autocorrelation patterns as ecological signals (population momentum, seasonal breeding cycles).

---

### Topic 2: Time Series Operations (10:30–12:30)

**Story:** The butterfly data shows an upward trend and a summer peak. Before modelling it, students must transform "wild" data into something stationary.

**Subtopics covered:**
- Stationarity — Concept and visual identification
- KPSS test — `unitroot_kpss()`, `unitroot_ndiffs()` for objective testing
- First-order differencing — `difference()` to remove linear trend
- Seasonal differencing — `difference(lag = 12)` to remove annual cycle
- Classical decomposition — Additive vs. multiplicative
- STL decomposition — `STL()`, robustness to outliers and changing seasonality

**What students learn:**
Students can test for stationarity formally (KPSS) and transform non-stationary series via differencing. They understand the distinction between decomposition (for understanding components) and differencing (for modelling). They can run and interpret both classical and STL decompositions on ecological data.

---

## Session 4 — Classical Models I (13:30–16:30)

### Topic 3: Modelling Ecological Time Series (13:30–14:30)

**Story:** Can we predict next month's butterfly count? The Box-Jenkins approach gives a systematic workflow.

**Subtopics covered:**
- The two dominant approaches — ETS vs. ARIMA
- Stationarity revisited for modelling
- Transformations — log, Box-Cox (`guerrero()`)
- Benchmark models — `MEAN()`, `NAIVE()`, `SNAIVE()`, `RW()`
- Residual diagnostics — `gg_tsresiduals()`
- Accuracy metrics — RMSE, MAE, MAPE with `accuracy()`

**What students learn:**
Students understand why variance stabilisation matters and can apply Box-Cox transformations. They learn to establish benchmark performance before building complex models, and to diagnose model fit through residual analysis and accuracy metrics.

---

### Topic 4: Classical Models — AR, MA, ARIMA (14:30–15:45)

**Story:** AR models say "the future depends on the past"; MA models say "the future depends on past surprises"; ARIMA combines both with differencing.

**Subtopics covered:**
- AR(p) — Autoregressive models, stationarity conditions
- MA(q) — Moving average models, invertibility
- ARMA(p,q) — Combined model
- ARIMA(p,d,q) — Adding integration (differencing)
- Backshift operator notation
- Model selection — AIC, AICc, BIC
- Automatic selection — `ARIMA()` in fable
- Forecasting with `forecast()` and confidence intervals

**What students learn:**
Students can identify appropriate ARIMA orders from ACF/PACF patterns, fit models manually and automatically, and produce forecasts with uncertainty bounds. They understand model selection criteria and the critical rule that time series data must always be split temporally.

---

### Topic 5: Incorporating Exogenous Series — ARIMAX (15:45–16:30)

**Story:** Butterflies don't live in a vacuum — they respond to temperature, rainfall, and habitat. Dynamic regression lets us include these external drivers.

**Subtopics covered:**
- Regression with ARIMA errors — conceptual decomposition
- The two error terms — regression error vs. ARIMA error
- Fitting with `ARIMA(y ~ x1 + x2)` syntax
- Forecasting under climate scenarios (e.g., +2°C warming)
- Ecological application — climate-driven abundance forecasting

**What students learn:**
Students can build ARIMAX models that combine external environmental predictors with ARIMA error structure. They learn scenario-based forecasting and how to compare ARIMA vs. ARIMAX accuracy to assess whether external drivers add predictive value.

---

## R Scripts in This Folder

| File | Description |
|------|-------------|
| `00_source.r` | Loads all required packages and shared helper functions for Day 2 |
| `generate_datasets.R` | Generates and saves the butterfly abundance and climate datasets used throughout Day 2 |
| `01_The_Structure_of_a_Time_Series_in_Ecology.R` | Creating tsibbles, time plots, seasonal plots, lag plots, and ACF analysis |
| `02_Time_Series_Operations.R` | Stationarity testing (KPSS), differencing (first and seasonal), classical and STL decomposition |
| `03_Modelling_Ecological_Time_Series.R` | Box-Cox transformations, benchmark models, residual diagnostics, accuracy metrics |
| `04_Classical_Models.R` | AR, MA, ARIMA model fitting (manual and automatic), model selection, forecast plots |
| `05_Incorporating_Exogenous_Time_Series.R` | ARIMAX: fitting dynamic regression, scenario forecasting, ARIMA vs. ARIMAX comparison |

---

## Equations (`equations/` folder)

| File | What it represents |
|------|--------------------|
| `eq_01_time_series_definition.png` | Formal definition: \(y = \{y_1, y_2, \ldots, y_T\}\) — an ordered sequence of observations indexed by time |
| `eq_02_autocorrelation_function.png` | ACF at lag \(k\): \(r_k = \frac{\sum_{t=k+1}^{T}(y_t - \bar{y})(y_{t-k} - \bar{y})}{\sum_{t=1}^{T}(y_t - \bar{y})^2}\) — measures the correlation between a series and its own lagged values |
| `eq_03_first_difference.png` | First difference: \(y'_t = y_t - y_{t-1}\) — removes a linear trend by computing observation-to-observation changes |
| `eq_04_seasonal_difference.png` | Seasonal difference: \(y'_t = y_t - y_{t-m}\) — removes a repeating seasonal pattern by subtracting the same season from the previous year |
| `eq_05_additive_decomposition.png` | Additive decomposition: \(y_t = T_t + S_t + R_t\) — trend, seasonal, and remainder components sum to the original series |
| `eq_06_multiplicative_decomposition.png` | Multiplicative decomposition: \(y_t = T_t \times S_t \times R_t\) — used when seasonal variation scales with the level of the series |
| `eq_07_box_cox.png` | Box-Cox transformation: \(w_t = \log(y_t)\) if \(\lambda = 0\), else \((y_t^\lambda - 1)/\lambda\) — stabilises non-constant variance before modelling |
| `eq_08_rmse.png` | RMSE: \(\sqrt{\frac{1}{T}\sum_{t=1}^{T}(y_t - \hat{y}_t)^2}\) — penalises large forecast errors more heavily than small ones |
| `eq_09_mae.png` | MAE: \(\frac{1}{T}\sum_{t=1}^{T}|y_t - \hat{y}_t|\) — gives equal weight to all forecast errors regardless of size |
| `eq_10_ar_p.png` | AR(p): \(y_t = c + \phi_1 y_{t-1} + \cdots + \phi_p y_{t-p} + \varepsilon_t\) — the current value is a linear function of its p most recent past values |
| `eq_11_ma_q.png` | MA(q): \(y_t = c + \varepsilon_t + \theta_1 \varepsilon_{t-1} + \cdots + \theta_q \varepsilon_{t-q}\) — the current value depends on q recent forecast errors (innovations) |
| `eq_12_arima.png` | Full ARIMA(p,d,q) in backshift notation — combines AR, integration (differencing), and MA in a unified polynomial framework |
| `eq_13_backshift.png` | Backshift operator: \(B y_t = y_{t-1}\), \(B^k y_t = y_{t-k}\) — compact notation for expressing lagged values algebraically |
| `eq_14_aicc.png` | AICc: \(\text{AIC} + \frac{2k(k+1)}{T-k-1}\) — corrected information criterion for model selection, penalising complexity in small samples |
| `eq_15_dynamic_regression.png` | ARIMAX mean equation: \(y_t = \beta_0 + \beta_1 x_{1,t} + \cdots + \beta_k x_{k,t} + \eta_t\), where \(\eta_t\) follows an ARIMA process |
| `eq_16_arimax_full.png` | Full ARIMAX model combining the regression component with the ARIMA error structure in one expression |

---

## Illustrations (`illustrations/` folder)

| File | What it shows |
|------|---------------|
| `ill_01_box_jenkins_flowchart.png` | The Box-Jenkins workflow: Identify (ACF/PACF) → Estimate (fit) → Diagnose (residuals) → Forecast — the structured approach to ARIMA modelling |
| `ill_02_decomposition_diagram.png` | Decomposition components diagram showing how a time series separates into Trend, Seasonal, and Remainder |
| `ill_03_acf_pacf_patterns.png` | ACF/PACF signature patterns for AR, MA, and ARMA models — the identification guide used to determine model order |
| `ill_04_temporal_vs_random_split.png` | Comparison of temporal train/test split (correct) vs. random split (incorrect) — shows why data leakage is catastrophic for time series |
| `ill_05_arimax_two_components.png` | Two-component diagram separating the regression part (external drivers) from the ARIMA error part in dynamic regression |

---

## Code Snippets (`code_snippets/` folder)

| File | What it demonstrates |
|------|----------------------|
| `snippet_01_create_tsibble.png` | Creating a `tsibble` from a butterfly abundance data frame — setting the index and key columns |
| `snippet_02_autoplot.png` | `autoplot()` on ecological abundance data — producing a time plot with ggplot2 aesthetics |
| `snippet_03_gg_season.png` | `gg_season()` on CO2 or butterfly data — overlaying multiple years to reveal seasonal patterns |
| `snippet_04_kpss_stationarity.png` | KPSS stationarity test using `unitroot_kpss()` and `unitroot_ndiffs()` to determine differencing order |
| `snippet_05_stl_decomposition.png` | STL decomposition with `STL()` in fable — extracting and plotting trend, seasonal, and remainder components |
| `snippet_06_arima_fit.png` | Fitting ARIMA automatically with `ARIMA()` in fable — showing model selection output and fitted coefficients |
| `snippet_07_forecast.png` | Generating forecasts with `forecast()` and plotting confidence intervals — the full prediction workflow |
| `snippet_08_arimax.png` | Fitting an ARIMAX model with `ARIMA(y ~ temperature)` syntax and interpreting covariate coefficients |
| `snippet_09_benchmark_comparison.png` | Comparing `MEAN()`, `NAIVE()`, and `SNAIVE()` benchmark models using `accuracy()` |
| `snippet_10_residual_diagnostics.png` | `gg_tsresiduals()` producing the three-panel residual diagnostic plot (residuals, ACF, histogram) |

---

## Key R Packages

| Package | Role |
|---------|------|
| `fpp3` | Meta-package loading the full tidyverts ecosystem for forecasting |
| `tsibble` | Time-aware data frames — the foundational data structure for all Day 2 work |
| `feasts` | Feature extraction and statistics: `ACF()`, `gg_season()`, `gg_lag()`, `gg_tsresiduals()` |
| `fable` | Forecasting models: `ARIMA()`, `ETS()`, `MEAN()`, `NAIVE()`, `forecast()` |

---

## Learning Objectives

By the end of Day 2, students will be able to:

1. Create and manipulate `tsibble` objects from ecological monitoring data
2. Produce and interpret time plots, seasonal plots, lag plots, and ACF plots
3. Test for stationarity using the KPSS test and apply first-order and seasonal differencing
4. Perform additive and STL decomposition and interpret each component
5. Apply Box-Cox transformations to stabilise variance before modelling
6. Fit benchmark models and use RMSE/MAE to establish baseline forecast accuracy
7. Identify AR, MA, and ARIMA model orders from ACF/PACF patterns
8. Fit ARIMA models automatically with `fable::ARIMA()` and generate forecasts
9. Build ARIMAX (dynamic regression) models that include external climate drivers
10. Explain why temporal train/test splitting is mandatory for time series evaluation

---

## References

- Hyndman, R.J., & Athanasopoulos, G. (2021). *Forecasting: Principles and Practice* (3rd ed). OTexts. https://otexts.com/fpp3/
- Palma, G.R., et al. (2025). Forecasting insect abundance using time series embedding and machine learning. *Ecological Informatics*, 85, 102934. https://doi.org/10.1016/j.ecoinf.2024.102934
