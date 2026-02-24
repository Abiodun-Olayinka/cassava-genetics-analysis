# Analysis Outputs

This folder contains all results from the path analysis.

## 📂 figures/
| File | Description |
|------|-------------|
| `path_diagram_circle.png` | Path diagram in circle layout showing relationships between traits |
| `path_diagram_tree.png` | Path diagram in tree layout showing hierarchical relationships |

## 📂 tables/
| File | Description |
|------|-------------|
| `path_analysis_summary.txt` | Complete model output with fit statistics |
| `path_parameters.csv` | All parameter estimates with standard errors and p-values |
| `fit_measures.csv` | Model fit indices (CFI, RMSEA, SRMR, etc.) |
| `r_squared_values.csv` | R² values for endogenous variables |
| `README_summary.txt` | Key findings summary for quick reference |

## 📊 Key Results Summary

- **RTWT → FYLD**: Estimate = 1.333
- **HI → RTWT**: Estimate = 55.160 (p < 0.001)
- **R² for FYLD**: 1.000
- **R² for RTWT**: 0.578

*Generated on: Tue Feb 24 12:17:09 2026 *

## 📊 Correlation Analysis

### 📂 figures/
| File | Description |
|------|-------------|
| `correlation_plot.png` | Circle plot of genotypic correlations |
| `correlation_heatmap.png` | Heatmap visualization of correlations |

### 📂 tables/
| File | Description |
|------|-------------|
| `correlation_matrix_full.csv` | Complete correlation matrix with all values |
| `correlation_matrix_lower.csv` | Lower triangle matrix (for publication) |
| `correlation_summary.txt` | Summary statistics and strongest correlations |

### 🔍 Key Findings

- **Strongest correlation**: LODG & BRNLEV9 ( NA )
- **Correlation range**: Inf to -Inf 
- **Number of traits**: 20 


## 📈 NDVI Linear Regression Model

**Model:** DM ~ NDVI3

### 📂 figures/
| File | Description |
|------|-------------|
| `ndvi_observed_vs_predicted.png` | Observed vs predicted values on test data |
| `ndvi_residual_plot.png` | Residual plot to check model assumptions |
| `ndvi_regression_line.png` | Regression line with training data |

### 📂 tables/
| File | Description |
|------|-------------|
| `ndvi_linear_summary.txt` | Complete model output and validation metrics |
| `ndvi_linear_predictions.csv` | Observed vs predicted values for test data |
| `ndvi_linear_coefficients.csv` | Model coefficients with statistics |
| `ndvi_linear_metrics.csv` | Validation metrics summary |

### 🔍 Key Results

- **R² (test data):** 0.326 
- **RMSE (test data):** 2.89 
- **MAE (test data):** 2.31 
- **Training samples:** 35 
- **Testing samples:** 7 
- **Model equation:** DM = 22.51 + 13.03 × NDVI3


## 📈 Polynomial Regression Model

**Model:** Second-degree polynomial regression for FYLD prediction

### 📂 figures/
| File | Description |
|------|-------------|
| `polynomial_diagnostics_4in1.png` | Standard 4-panel diagnostic plots |
| `polynomial_residuals_fitted.png` | Residuals vs fitted values with smoother |
| `polynomial_qqplot.png` | Q-Q plot for normality of residuals |
| `polynomial_predicted_vs_actual.png` | Predicted vs actual FYLD values |
| `polynomial_variable_importance.png` | Variable importance based on t-values |

### 📂 tables/
| File | Description |
|------|-------------|
| `polynomial_summary.txt` | Complete model output with all statistics |
| `polynomial_coefficients.csv` | Model coefficients with significance |
| `polynomial_vif.csv` | Variance Inflation Factor (multicollinearity check) |
| `polynomial_predictions.csv` | Actual vs predicted values with residuals |

### 🔍 Key Results

- **R²:** 0.448 
- **Adjusted R²:** 0.436 
- **RMSE:** 7.86 
- **RMSE (% of mean):** 30 %
- **Sample size:** 767 
- **Significant predictors (p < 0.05):** LODG
- **Marginal predictors (p < 0.1):** PLTHT, STMDI

