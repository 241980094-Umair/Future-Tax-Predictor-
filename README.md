# AI-Powered Future Tax Predictor

An interactive R Shiny application that leverages Machine Learning (Ridge Regression) to forecast individual and corporate tax liabilities for the next three years (2026–2028) based on a 7-year rolling financial history.

---

## 🚀 Key Features
- **Rolling Matrix Forecasting:** Loops forward automatically, shifting historical baselines to project multi-year future trends.
- **Advanced Data Pipeline:** Implements $\log(x+1)$ transformation to gracefully handle multi-scale economies (from standard income to billion-dollar outliers).
- **Modern UI Architecture:** Built with `bslib` (Minty theme) featuring collapsible dashboards and synchronized live visual charts.

---

## 🛠️ Tech Stack & Architecture

### The Synchronized Data Pipeline
To ensure mathematical stability under extreme financial scenarios, the backend processes inputs through a rigorous transformation pipeline:
1. **Log-Compression:** Raw inputs are compressed via $y = \log(x + 1)$ to map working-class data and massive corporate profiles on a stable linear axis.
2. **Strict Matrix Alignment:** Casts real-time UI data into a rigid multi-dimensional matrix matching the exact physical column order expected by the trained model.
3. **Exponential Back-Transformation:** Converts logarithmic outputs back into real-world currencies using $x = \exp(y) - 1$.

### Why Ridge Regression ($L_2$ Regularization)?
Previous phases using *Random Forest* failed because tree-based models cannot extrapolate outside training ceilings. Standard *Ordinary Least Squares (`lm`)* collapsed when massive historical outlier years slid out of the 7-year memory window. 

**Ridge Regression (`glmnet`)** was chosen because its $L_2$ penalty suppresses extreme variance, distributes feature weights evenly across all timeline vectors, and prevents structural baseline collapses.

---

## 📦 Current Limitations
- **Non-Cyclical Nature:** Being a regression model rather than a sequential time-series model (like ARIMA/Prophet), it evaluates flat multi-year averages and cannot capture alternating or cyclical tax-paying sequences.
- **Trend Smoothing:** The $L_2$ penalty heavily smooths sudden, single-year multi-million dollar drops or spikes to preserve timeline consistency.

---

## 🔧 How to Run Locally

### Prerequisites
Make sure you have R and the following packages installed:
```r
install.packages(c("shiny", "bslib", "shinyjs", "shinycssloaders", "ggplot2", "glmnet", "caret", "Metrics"))
