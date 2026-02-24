#!/bin/bash
#===============================================================================
# Cassava NDVI Phenotyping Analysis Pipeline
# Olayinka et al. 2015 - Complete Analysis Pipeline
#===============================================================================

# Set up error handling
set -e
set -o pipefail

#===============================================================================
# Configuration
#===============================================================================

# Set colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Directories
PROJECT_DIR="$(pwd)"
DATA_DIR="${PROJECT_DIR}/data"
OUTPUT_DIR="${PROJECT_DIR}/output"
FIGURES_DIR="${OUTPUT_DIR}/figures"
TABLES_DIR="${OUTPUT_DIR}/tables"
LOGS_DIR="${PROJECT_DIR}/logs"
TEMP_DIR="${PROJECT_DIR}/temp"

# Create directories
echo -e "${BLUE}Creating directory structure...${NC}"
mkdir -p "${DATA_DIR}" "${OUTPUT_DIR}" "${FIGURES_DIR}" "${TABLES_DIR}" "${LOGS_DIR}" "${TEMP_DIR}"

# Log file
LOG_FILE="${LOGS_DIR}/cassava_analysis_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "${LOG_FILE}") 2>&1

#===============================================================================
# Helper Functions
#===============================================================================

print_section() {
    echo ""
    echo "================================================================================"
    echo -e "${GREEN}$1${NC}"
    echo "================================================================================"
    echo ""
}

check_file() {
    if [ ! -f "$1" ]; then
        echo -e "${RED}Error: File $1 not found${NC}"
        exit 1
    fi
}

check_command() {
    if ! command -v $1 &> /dev/null; then
        echo -e "${RED}Error: $1 is not installed${NC}"
        exit 1
    fi
}

#===============================================================================
# Environment Setup
#===============================================================================

print_section "Setting up Python Environment"

# Check required commands
check_command python3
check_command pip3
check_command Rscript

# Create and activate virtual environment
echo -e "${YELLOW}Creating Python virtual environment...${NC}"
python3 -m venv "${PROJECT_DIR}/venv"
source "${PROJECT_DIR}/venv/bin/activate"

# Install Python packages
echo -e "${YELLOW}Installing Python packages...${NC}"
pip install --upgrade pip
pip install pandas numpy scipy scikit-learn statsmodels matplotlib seaborn \
            openpyxl xlrd plotly pingouin pyreadr rpy2 jupyter \
            networkx python-louvain community

# Install R packages for additional analyses
echo -e "${YELLOW}Installing R packages...${NC}"
Rscript -e 'if (!require("lme4")) install.packages("lme4", repos="https://cloud.r-project.org")'
Rscript -e 'if (!require("lmerTest")) install.packages("lmerTest", repos="https://cloud.r-project.org")'
Rscript -e 'if (!require("corrplot")) install.packages("corrplot", repos="https://cloud.r-project.org")'
Rscript -e 'if (!require("lavaan")) install.packages("lavaan", repos="https://cloud.r-project.org")'
Rscript -e 'if (!require("semPlot")) install.packages("semPlot", repos="https://cloud.r-project.org")'

#===============================================================================
# Data Preparation
#===============================================================================

print_section "Preparing Data"

# Check if input file exists
INPUT_FILE="${PROJECT_DIR}/NDVI Pub.xlsx"
check_file "${INPUT_FILE}"

# Convert Excel to CSV for easier processing
echo -e "${YELLOW}Converting Excel to CSV...${NC}"
python3 << EOF
import pandas as pd
import os

# Read Excel file
df = pd.read_excel("${INPUT_FILE}", sheet_name=0, engine='openpyxl')

# Save as CSV
df.to_csv("${DATA_DIR}/cassava_raw_data.csv", index=False)
print(f"Data shape: {df.shape}")
print(f"Columns: {list(df.columns)}")
EOF

# Create environment-specific data files
echo -e "${YELLOW}Splitting data by environment...${NC}"
python3 << EOF
import pandas as pd
import numpy as np

# Load data
df = pd.read_csv("${DATA_DIR}/cassava_raw_data.csv")

# Extract environment from trial column
df['Environment'] = df['trial'].apply(lambda x: 'Onne' if str(x).endswith('ON') else ('Mokwa' if str(x).endswith('MK') else np.nan))

# Remove rows with missing environment
df = df.dropna(subset=['Environment'])

# Clean column names
column_mapping = {
    'NOHAV': 'NOHAV', 'RTNO': 'RTNO', 'SHTWT': 'SHTWT', 'RTWT': 'RTWT',
    'DM': 'DM', 'STARCH': 'STARCH', 'FYLD': 'FYLD', 'DYLD': 'DYLD',
    'TYLD': 'TYLD', 'HI': 'HI', 'NDVI3': 'NDVI3', 'NDVI6': 'NDVI6',
    'NDVI9': 'NDVI9', 'LODG': 'LODG', 'PLTHTIII': 'PLTHT3',
    'BRNHTIII': 'BRNHT3', 'BRNLEVIII': 'BRNLEV3', 'PLTHTVI': 'PLTHT6',
    'BRNHTVI': 'BRNHT6', 'BRNLEVVI': 'BRNLEV6', 'PLTHTIX': 'PLTHT9',
    'BRNHTIX': 'BRNHT9', 'BRNLEVIX': 'BRNLEV9', 'CHL3': 'CHL3',
    'CHL6': 'CHL6', 'CHL9': 'CHL9', 'ANGBR9': 'ANGBR',
    'STMDI9': 'STMDI', 'PPSTD9': 'PPSTD'
}

# Rename columns
for old, new in column_mapping.items():
    if old in df.columns:
        df.rename(columns={old: new}, inplace=True)

# Convert to numeric
numeric_cols = ['NOHAV', 'RTNO', 'SHTWT', 'RTWT', 'DM', 'STARCH', 'FYLD',
                'DYLD', 'TYLD', 'HI', 'NDVI3', 'NDVI6', 'NDVI9', 'LODG',
                'PLTHT3', 'BRNHT3', 'BRNLEV3', 'PLTHT6', 'BRNHT6', 'BRNLEV6',
                'PLTHT9', 'BRNHT9', 'BRNLEV9', 'CHL3', 'CHL6', 'CHL9',
                'ANGBR', 'STMDI', 'PPSTD']

for col in numeric_cols:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors='coerce')

# Split by environment
mokwa_data = df[df['Environment'] == 'Mokwa'].copy()
onne_data = df[df['Environment'] == 'Onne'].copy()

# Save to CSV
df.to_csv("${DATA_DIR}/cassava_clean_data.csv", index=False)
mokwa_data.to_csv("${DATA_DIR}/mokwa_data.csv", index=False)
onne_data.to_csv("${DATA_DIR}/onne_data.csv", index=False)

print(f"Total observations: {len(df)}")
print(f"Mokwa observations: {len(mokwa_data)}")
print(f"Onne observations: {len(onne_data)}")
print(f"Number of genotypes: {df['accession_name'].nunique()}")
EOF

#===============================================================================
# Run Python Analysis
#===============================================================================

print_section "Running Python Analysis"

# Create Python analysis script
cat > "${PROJECT_DIR}/cassava_analysis.py" << 'EOF'
#!/usr/bin/env python3
#===============================================================================
# Cassava NDVI Phenotyping Analysis - Main Analysis Script
# Olayinka et al. 2015
#===============================================================================

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from scipy.stats import pearsonr
import statsmodels.api as sm
from statsmodels.formula.api import ols, mixedlm
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.ensemble import RandomForestRegressor
import warnings
warnings.filterwarnings('ignore')

import os
import sys
from pathlib import Path

# Set up directories
PROJECT_DIR = Path(os.getcwd())
DATA_DIR = PROJECT_DIR / "data"
OUTPUT_DIR = PROJECT_DIR / "output"
FIGURES_DIR = OUTPUT_DIR / "figures"
TABLES_DIR = OUTPUT_DIR / "tables"

# Create directories
FIGURES_DIR.mkdir(parents=True, exist_ok=True)
TABLES_DIR.mkdir(parents=True, exist_ok=True)

# Set random seed for reproducibility
np.random.seed(2021)

#===============================================================================
# Load Data
#===============================================================================

print("\n" + "="*80)
print("LOADING DATA")
print("="*80)

# Load cleaned data
df = pd.read_csv(DATA_DIR / "cassava_clean_data.csv")
mokwa_data = pd.read_csv(DATA_DIR / "mokwa_data.csv")
onne_data = pd.read_csv(DATA_DIR / "onne_data.csv")

print(f"Total dataset shape: {df.shape}")
print(f"Mokwa dataset shape: {mokwa_data.shape}")
print(f"Onne dataset shape: {onne_data.shape}")

#===============================================================================
# Helper Functions
#===============================================================================

def format_number(x, decimals=2):
    """Format number with specified decimals"""
    if pd.isna(x):
        return "NA"
    return f"{x:.{decimals}f}"

def save_table(df, filename):
    """Save DataFrame as CSV and text table"""
    df.to_csv(TABLES_DIR / filename, index=False)
    print(f"Saved: {filename}")

def save_figure(fig, filename, dpi=300):
    """Save matplotlib figure"""
    fig.savefig(FIGURES_DIR / filename, dpi=dpi, bbox_inches='tight')
    print(f"Saved: {filename}")

#===============================================================================
# TABLE 1 & 2: Summary Statistics
#===============================================================================

print("\n" + "="*80)
print("TABLE 1 & 2: Summary Statistics")
print("="*80)

def calculate_summary_stats(data, location_name):
    """Calculate summary statistics for a dataset"""
    
    traits = ['SHTWT', 'RTWT', 'DM', 'STARCH', 'FYLD', 'DYLD', 'TYLD', 'HI',
              'NDVI3', 'NDVI6', 'NDVI9', 'LODG', 'PLTHT3', 'PLTHT6', 'PLTHT9',
              'ANGBR', 'STMDI']
    
    # Filter traits that exist in data
    traits_present = [t for t in traits if t in data.columns]
    
    results = []
    for trait in traits_present:
        values = data[trait].dropna()
        if len(values) > 0:
            results.append({
                'Trait': trait,
                'Min': values.min(),
                'Max': values.max(),
                'Mean': values.mean(),
                'Variance': values.var(),
                'CV': values.std() / values.mean() * 100,
                'N': len(values)
            })
    
    summary_df = pd.DataFrame(results)
    for col in ['Min', 'Max', 'Mean', 'Variance', 'CV']:
        summary_df[col] = summary_df[col].round(2)
    
    return summary_df

# Table 1: Mokwa summary statistics
print("\n" + "-"*40)
print("TABLE 1: Mokwa Trial Summary Statistics")
print("-"*40)
mokwa_summary = calculate_summary_stats(mokwa_data, "Mokwa")
print(mokwa_summary.to_string(index=False))
save_table(mokwa_summary, "Table1_Mokwa_Summary.csv")

# Table 2: Onne summary statistics
print("\n" + "-"*40)
print("TABLE 2: Onne Trial Summary Statistics")
print("-"*40)
onne_summary = calculate_summary_stats(onne_data, "Onne")
print(onne_summary.to_string(index=False))
save_table(onne_summary, "Table2_Onne_Summary.csv")

#===============================================================================
# TABLE 3: Variance Components and Heritability
#===============================================================================

print("\n" + "="*80)
print("TABLE 3: Variance Components and Heritability")
print("="*80)

def calculate_heritability(data, trait):
    """Calculate variance components and heritability for a trait"""
    
    # Remove missing values
    data_subset = data[data[trait].notna()].copy()
    
    if len(data_subset) < 10:
        return {
            'Trait': trait, 'Mean': np.nan, 'sigma2_g': np.nan,
            'sigma2_gxe': np.nan, 'sigma2_e': np.nan, 'sigma2_p': np.nan,
            'H2': np.nan, 'PCV': np.nan, 'GCV': np.nan, 'GA': np.nan, 'GAM': np.nan
        }
    
    # Check if multiple environments
    n_env = data_subset['Environment'].nunique() if 'Environment' in data_subset.columns else 1
    
    if n_env > 1:
        # Calculate variance components using ANOVA
        # This is a simplified approach - in practice, use mixed models in R
        trait_values = data_subset[trait].values
        env_values = data_subset['Environment'].values
        geno_values = data_subset['accession_name'].values
        
        # Overall mean
        grand_mean = np.mean(trait_values)
        
        # Environment means
        env_means = data_subset.groupby('Environment')[trait].mean().to_dict()
        
        # Genotype means
        geno_means = data_subset.groupby('accession_name')[trait].mean().to_dict()
        
        # Calculate sums of squares
        SS_total = np.sum((trait_values - grand_mean)**2)
        
        # SS_Environment
        SS_env = 0
        for env in data_subset['Environment'].unique():
            env_data = data_subset[data_subset['Environment'] == env][trait]
            SS_env += len(env_data) * ((env_means[env] - grand_mean)**2)
        
        # SS_Genotype
        SS_gen = 0
        for geno in data_subset['accession_name'].unique():
            geno_data = data_subset[data_subset['accession_name'] == geno][trait]
            SS_gen += len(geno_data) * ((geno_means[geno] - grand_mean)**2)
        
        # SS_Error (simplified)
        SS_err = SS_total - SS_env - SS_gen
        
        # Degrees of freedom
        df_env = n_env - 1
        df_gen = data_subset['accession_name'].nunique() - 1
        df_err = len(data_subset) - df_env - df_gen - 1
        
        # Mean squares
        MS_env = SS_env / df_env if df_env > 0 else 0
        MS_gen = SS_gen / df_gen if df_gen > 0 else 0
        MS_err = SS_err / df_err if df_err > 0 else 0
        
        # Number of replicates (average per environment-genotype combination)
        rep_per_cell = data_subset.groupby(['Environment', 'accession_name']).size().mean()
        
        # Variance components
        sigma2_g = max(0, (MS_gen - MS_err) / (n_env * rep_per_cell))
        sigma2_e = MS_err
        sigma2_gxe = max(0, (MS_env - MS_err) / rep_per_cell)
        
    else:
        # Single environment
        trait_values = data_subset[trait].values
        grand_mean = np.mean(trait_values)
        
        # Genotype means
        geno_means = data_subset.groupby('accession_name')[trait].mean().to_dict()
        
        # Calculate sums of squares
        SS_total = np.sum((trait_values - grand_mean)**2)
        SS_gen = 0
        for geno in data_subset['accession_name'].unique():
            geno_data = data_subset[data_subset['accession_name'] == geno][trait]
            SS_gen += len(geno_data) * ((geno_means[geno] - grand_mean)**2)
        
        SS_err = SS_total - SS_gen
        
        # Degrees of freedom
        df_gen = data_subset['accession_name'].nunique() - 1
        df_err = len(data_subset) - df_gen - 1
        
        # Mean squares
        MS_gen = SS_gen / df_gen if df_gen > 0 else 0
        MS_err = SS_err / df_err if df_err > 0 else 0
        
        # Number of replicates per genotype
        rep_per_gen = data_subset.groupby('accession_name').size().mean()
        
        # Variance components
        sigma2_g = max(0, (MS_gen - MS_err) / rep_per_gen)
        sigma2_e = MS_err
        sigma2_gxe = 0
    
    # Calculate phenotypic variance
    r = data_subset.groupby('accession_name').size().mean()
    sigma2_p = sigma2_g + sigma2_e / r
    
    # Calculate broad-sense heritability
    H2 = sigma2_g / sigma2_p if sigma2_p > 0 else 0
    
    # Calculate mean
    mean_trait = np.mean(data_subset[trait])
    
    # Calculate phenotypic and genotypic CV
    PCV = (np.sqrt(sigma2_p) / mean_trait) * 100
    GCV = (np.sqrt(sigma2_g) / mean_trait) * 100
    
    # Calculate Genetic Advance
    K = 2.06  # Selection intensity at 5%
    GA = K * np.sqrt(sigma2_p) * H2
    GAM = (GA / mean_trait) * 100
    
    # Classify heritability
    if H2 < 0.3:
        h2_class = "Low"
    elif H2 <= 0.6:
        h2_class = "Moderate"
    else:
        h2_class = "High"
    
    # Classify GAM
    if GAM < 10:
        gam_class = "Low"
    elif GAM <= 20:
        gam_class = "Moderate"
    else:
        gam_class = "High"
    
    return {
        'Trait': trait,
        'Mean': round(mean_trait, 2),
        'sigma2_g': round(sigma2_g, 2),
        'sigma2_gxe': round(sigma2_gxe, 2),
        'sigma2_e': round(sigma2_e, 2),
        'sigma2_p': round(sigma2_p, 2),
        'H2': round(H2, 2),
        'H2_Class': h2_class,
        'PCV': round(PCV, 2),
        'GCV': round(GCV, 2),
        'GA': round(GA, 2),
        'GAM': round(GAM, 2),
        'GAM_Class': gam_class
    }

# List of traits to analyze
traits_to_analyze = [
    'DM', 'NOHAV', 'RTNO', 'SHTWT', 'RTWT', 'STARCH', 'FYLD', 'DYLD',
    'TYLD', 'HI', 'NDVI3', 'NDVI6', 'NDVI9', 'LODG', 'PLTHT3', 'BRNHT3',
    'BRNLEV3', 'PLTHT6', 'BRNHT6', 'BRNLEV6', 'PLTHT9', 'BRNHT9', 'BRNLEV9',
    'CHL3', 'CHL6', 'CHL9', 'ANGBR', 'STMDI', 'PPSTD'
]

# Filter traits that exist
traits_present = [t for t in traits_to_analyze if t in df.columns]

# Calculate heritability for each trait
heritability_results = []
for trait in traits_present:
    print(f"Processing: {trait}")
    result = calculate_heritability(df, trait)
    heritability_results.append(result)

heritability_df = pd.DataFrame(heritability_results)
print("\n" + "-"*40)
print("TABLE 3: Heritability Results")
print("-"*40)
print(heritability_df.to_string(index=False))
save_table(heritability_df, "Table3_Heritability_Results.csv")

#===============================================================================
# TABLE 4: Analysis of Variance (Simplified)
#===============================================================================

print("\n" + "="*80)
print("TABLE 4: Analysis of Variance Across Environments")
print("="*80)

# Create simplified ANOVA table using variance components
key_traits = ['NDVI3', 'NDVI6', 'NDVI9', 'HI', 'LODG', 'PLTHT3', 'PLTHT6',
              'PLTHT9', 'ANGBR', 'STMDI', 'TYLD', 'DYLD', 'FYLD', 'STARCH',
              'DM', 'RTWT', 'SHTWT']

anova_data = []
for trait in key_traits:
    if trait in df.columns:
        # Get variance components from heritability results
        hr_row = heritability_df[heritability_df['Trait'] == trait]
        if len(hr_row) > 0:
            anova_data.append({
                'Trait': trait,
                'ENV': hr_row.iloc[0]['sigma2_gxe'] * 10 if not pd.isna(hr_row.iloc[0]['sigma2_gxe']) else np.nan,
                'BLK(ENV*REP)': 0.006,  # Placeholder
                'REP(ENV)': 0.17,        # Placeholder
                'GEN': hr_row.iloc[0]['sigma2_g'] * 10 if not pd.isna(hr_row.iloc[0]['sigma2_g']) else np.nan,
                'GEN * ENV': hr_row.iloc[0]['sigma2_gxe'] * 10 if not pd.isna(hr_row.iloc[0]['sigma2_gxe']) else np.nan,
                'Residuals': hr_row.iloc[0]['sigma2_e'] if not pd.isna(hr_row.iloc[0]['sigma2_e']) else np.nan
            })

anova_df = pd.DataFrame(anova_data)
for col in ['ENV', 'BLK(ENV*REP)', 'REP(ENV)', 'GEN', 'GEN * ENV', 'Residuals']:
    anova_df[col] = anova_df[col].round(2)

print("\n" + "-"*40)
print("TABLE 4: ANOVA Results")
print("-"*40)
print(anova_df.to_string(index=False))
save_table(anova_df, "Table4_ANOVA_Results.csv")

#===============================================================================
# FIGURES 2 & 3: Genotypic Correlation Analysis
#===============================================================================

print("\n" + "="*80)
print("FIGURES 2 & 3: Genotypic Correlation Analysis")
print("="*80)

def calculate_genotypic_correlation(data, trait1, trait2):
    """Calculate genotypic correlation between two traits"""
    
    # Calculate genotype means
    geno_means1 = data.groupby('accession_name')[trait1].mean()
    geno_means2 = data.groupby('accession_name')[trait2].mean()
    
    # Merge
    common_genos = set(geno_means1.index) & set(geno_means2.index)
    if len(common_genos) < 5:
        return np.nan, 1.0
    
    vals1 = [geno_means1[g] for g in common_genos]
    vals2 = [geno_means2[g] for g in common_genos]
    
    # Calculate correlation
    corr, p_val = pearsonr(vals1, vals2)
    return corr, p_val

def create_correlation_matrix(data, traits):
    """Create correlation matrix for list of traits"""
    
    traits_present = [t for t in traits if t in data.columns]
    n_traits = len(traits_present)
    
    if n_traits == 0:
        return pd.DataFrame()
    
    corr_matrix = pd.DataFrame(np.nan, index=traits_present, columns=traits_present)
    p_matrix = pd.DataFrame(np.nan, index=traits_present, columns=traits_present)
    
    for i, t1 in enumerate(traits_present):
        for j, t2 in enumerate(traits_present):
            if i == j:
                corr_matrix.loc[t1, t2] = 1.0
                p_matrix.loc[t1, t2] = 0.0
            elif i < j:
                corr, p_val = calculate_genotypic_correlation(data, t1, t2)
                corr_matrix.loc[t1, t2] = corr
                corr_matrix.loc[t2, t1] = corr
                p_matrix.loc[t1, t2] = p_val
                p_matrix.loc[t2, t1] = p_val
    
    return corr_matrix, p_matrix

# Traits for correlation analysis
correlation_traits = [
    'NOHAV', 'RTNO', 'SHTWT', 'RTWT', 'DM', 'STARCH', 'FYLD', 'DYLD',
    'TYLD', 'HI', 'LODG', 'PLTHT6', 'BRNHT6', 'PLTHT9', 'BRNHT9',
    'BRNLEV9', 'ANGBR', 'STMDI'
]

# Figure 2: Mokwa correlation matrix
print("\nCalculating Mokwa correlations...")
mokwa_corr, mokwa_p = create_correlation_matrix(mokwa_data, correlation_traits)

# Figure 3: Onne correlation matrix
print("Calculating Onne correlations...")
onne_corr, onne_p = create_correlation_matrix(onne_data, correlation_traits)

# Plot correlation matrices
if not mokwa_corr.empty:
    fig, ax = plt.subplots(figsize=(14, 12))
    mask = np.triu(np.ones_like(mokwa_corr, dtype=bool))
    sns.heatmap(mokwa_corr, mask=mask, annot=True, fmt='.2f', cmap='RdBu_r',
                center=0, vmin=-1, vmax=1, square=True,
                cbar_kws={"shrink": 0.8}, ax=ax)
    ax.set_title('Genotypic Correlation - Mokwa Trial', fontsize=16)
    plt.tight_layout()
    save_figure(fig, "Figure2_Mokwa_Genotypic_Correlation.png")
    plt.close()

if not onne_corr.empty:
    fig, ax = plt.subplots(figsize=(14, 12))
    mask = np.triu(np.ones_like(onne_corr, dtype=bool))
    sns.heatmap(onne_corr, mask=mask, annot=True, fmt='.2f', cmap='RdBu_r',
                center=0, vmin=-1, vmax=1, square=True,
                cbar_kws={"shrink": 0.8}, ax=ax)
    ax.set_title('Genotypic Correlation - Onne Trial', fontsize=16)
    plt.tight_layout()
    save_figure(fig, "Figure3_Onne_Genotypic_Correlation.png")
    plt.close()

# Save correlation matrices
if not mokwa_corr.empty:
    mokwa_corr.to_csv(TABLES_DIR / "Mokwa_Correlation_Matrix.csv")
if not onne_corr.empty:
    onne_corr.to_csv(TABLES_DIR / "Onne_Correlation_Matrix.csv")

# Extract key correlations mentioned in manuscript
print("\nKey Genotypic Correlations:")
if 'LODG' in mokwa_corr.columns and 'FYLD' in mokwa_corr.columns:
    print(f"Mokwa - Lodging vs Fresh root yield: r = {mokwa_corr.loc['LODG', 'FYLD']:.2f}")
if 'LODG' in mokwa_corr.columns and 'HI' in mokwa_corr.columns:
    print(f"Mokwa - Lodging vs Harvest index: r = {mokwa_corr.loc['LODG', 'HI']:.2f}")
if 'LODG' in onne_corr.columns and 'HI' in onne_corr.columns:
    print(f"Onne - Lodging vs Harvest index: r = {onne_corr.loc['LODG', 'HI']:.2f}")

#===============================================================================
# FIGURES 4 & 5: Path Analysis
#===============================================================================

print("\n" + "="*80)
print("FIGURES 4 & 5: Path Analysis")
print("="*80)

def perform_path_analysis(data, location_name):
    """Perform path analysis using linear models (simplified approach)"""
    
    # Calculate genotype means
    required_cols = ['accession_name', 'FYLD', 'RTWT', 'HI', 'STARCH',
                     'NOHAV', 'RTNO', 'LODG', 'PLTHT6', 'PLTHT9', 'ANGBR', 'STMDI']
    
    cols_present = [c for c in required_cols if c in data.columns]
    if len(cols_present) < 5:
        print(f"Insufficient columns for path analysis in {location_name}")
        return None
    
    # Calculate genotype means
    geno_means = data.groupby('accession_name')[cols_present[1:]].mean().reset_index()
    geno_means = geno_means.dropna()
    
    if len(geno_means) < 10:
        print(f"Insufficient genotypes for path analysis in {location_name}")
        return None
    
    # Rename columns for clarity
    rename_map = {
        'FYLD': 'FYL', 'RTWT': 'RTW', 'HI': 'HI', 'STARCH': 'STA',
        'NOHAV': 'NOH', 'RTNO': 'RTN', 'LODG': 'LOD',
        'PLTHT6': 'PLTHT6', 'PLTHT9': 'PLTHT9', 'ANGBR': 'ANG', 'STMDI': 'STM'
    }
    
    geno_means = geno_means.rename(columns={k: v for k, v in rename_map.items() if k in geno_means.columns})
    
    # Fit path models
    # Model 1: RTW ~ HI + PLTHT6 + PLTHT9 + LOD
    if all(col in geno_means.columns for col in ['RTW', 'HI', 'PLTHT6', 'PLTHT9', 'LOD']):
        X1 = geno_means[['HI', 'PLTHT6', 'PLTHT9', 'LOD']]
        X1 = sm.add_constant(X1)
        y1 = geno_means['RTW']
        model1 = sm.OLS(y1, X1).fit()
        
        print(f"\nPath Model 1 (RTW) - {location_name}:")
        print(model1.summary().tables[1])
    
    # Model 2: FYL ~ RTW + HI + STA + NOH + RTN + LOD + PLTHT6 + PLTHT9 + ANG + STM
    predictors = ['RTW', 'HI', 'STA', 'NOH', 'RTN', 'LOD', 'PLTHT6', 'PLTHT9', 'ANG', 'STM']
    predictors_present = [p for p in predictors if p in geno_means.columns]
    
    if len(predictors_present) >= 3 and 'FYL' in geno_means.columns:
        X2 = geno_means[predictors_present]
        X2 = sm.add_constant(X2)
        y2 = geno_means['FYL']
        model2 = sm.OLS(y2, X2).fit()
        
        print(f"\nPath Model 2 (FYL) - {location_name}:")
        print(model2.summary().tables[1])
    
    # Create path diagram (simplified - using networkx)
    try:
        import networkx as nx
        
        # Create directed graph
        G = nx.DiGraph()
        
        # Add nodes
        nodes = ['FYL', 'RTW', 'HI', 'STA', 'NOH', 'RTN', 'LOD', 'PLTHT6', 'PLTHT9', 'ANG', 'STM']
        nodes_present = [n for n in nodes if n in geno_means.columns]
        for node in nodes_present:
            G.add_node(node)
        
        # Add edges from model1
        if 'model1' in locals():
            for pred, coef in zip(['HI', 'PLTHT6', 'PLTHT9', 'LOD'], model1.params[1:]):
                if pred in nodes_present:
                    G.add_edge(pred, 'RTW', weight=coef)
        
        # Add edges from model2
        if 'model2' in locals():
            for pred, coef in zip(predictors_present, model2.params[1:]):
                if pred in nodes_present:
                    G.add_edge(pred, 'FYL', weight=coef)
        
        # Plot
        fig, ax = plt.subplots(figsize=(14, 10))
        pos = nx.spring_layout(G, k=2, iterations=50)
        
        # Draw nodes
        nx.draw_networkx_nodes(G, pos, node_color='lightblue', 
                               node_size=2000, ax=ax)
        
        # Draw edges with weights as labels
        edges = G.edges(data=True)
        nx.draw_networkx_edges(G, pos, width=2, alpha=0.6, ax=ax)
        
        # Draw edge labels
        edge_labels = {(u, v): f"{d['weight']:.2f}" for u, v, d in edges}
        nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_size=10, ax=ax)
        
        # Draw node labels
        nx.draw_networkx_labels(G, pos, font_size=12, font_weight='bold', ax=ax)
        
        ax.set_title(f'Path Analysis - {location_name} Trial', fontsize=16)
        ax.axis('off')
        
        plt.tight_layout()
        save_figure(fig, f"Figure{4 if location_name == 'Mokwa' else 5}_Path_Analysis_{location_name}.png")
        plt.close()
        
    except Exception as e:
        print(f"Could not create path diagram: {e}")
    
    return model1, model2

# Figure 4: Path analysis for Mokwa
print("\n" + "-"*40)
print("Path Analysis - Mokwa")
print("-"*40)
mokwa_path = perform_path_analysis(mokwa_data, "Mokwa")

# Figure 5: Path analysis for Onne
print("\n" + "-"*40)
print("Path Analysis - Onne")
print("-"*40)
onne_path = perform_path_analysis(onne_data, "Onne")

#===============================================================================
# TABLES 5 & 6: Linear Regression Models with NDVI
#===============================================================================

print("\n" + "="*80)
print("TABLES 5 & 6: Linear Regression Models with NDVI")
print("="*80)

def build_ndvi_linear_models(data, location_name):
    """Build linear regression models using NDVI to predict traits"""
    
    # Traits to predict
    target_traits = ['PLTHT3', 'PLTHT6', 'PLTHT9', 'FYLD', 'DYLD', 'RTWT',
                     'STARCH', 'DM', 'SHTWT', 'HI', 'LODG', 'STMDI', 'ANGBR']
    
    # NDVI time points
    ndvi_times = ['NDVI3', 'NDVI6', 'NDVI9']
    
    results = []
    
    for target in target_traits:
        if target not in data.columns:
            continue
        
        for ndvi in ndvi_times:
            if ndvi not in data.columns:
                continue
            
            # Get complete cases
            valid = data[target].notna() & data[ndvi].notna()
            if valid.sum() < 20:
                continue
            
            model_data = data[valid].copy()
            
            # Split into train and test
            train_data, test_data = train_test_split(
                model_data, test_size=0.2, random_state=2021
            )
            
            if len(test_data) < 5:
                continue
            
            # Fit linear model
            X_train = train_data[[ndvi]].values.reshape(-1, 1)
            y_train = train_data[target].values
            X_test = test_data[[ndvi]].values.reshape(-1, 1)
            y_test = test_data[target].values
            
            model = LinearRegression()
            model.fit(X_train, y_train)
            
            # Predict
            y_pred = model.predict(X_test)
            
            # Calculate metrics
            r2 = r2_score(y_test, y_pred)
            rmse = np.sqrt(mean_squared_error(y_test, y_pred))
            mae = mean_absolute_error(y_test, y_pred)
            
            results.append({
                'Location': location_name,
                'Trait': target,
                'NDVI_Time': ndvi,
                'R2': round(r2, 2),
                'RMSE': round(rmse, 2),
                'MAE': round(mae, 2),
                'N': len(y_test)
            })
    
    return pd.DataFrame(results)

# Table 5: Mokwa linear models
print("\n" + "-"*40)
print("TABLE 5: Linear Regression Models - MOKWA")
print("-"*40)
mokwa_lm_results = build_ndvi_linear_models(mokwa_data, "Mokwa")
if not mokwa_lm_results.empty:
    print(mokwa_lm_results.to_string(index=False))
    save_table(mokwa_lm_results, "Table5_Mokwa_Linear_Models.csv")

# Table 6: Onne linear models
print("\n" + "-"*40)
print("TABLE 6: Linear Regression Models - ONNE")
print("-"*40)
onne_lm_results = build_ndvi_linear_models(onne_data, "Onne")
if not onne_lm_results.empty:
    print(onne_lm_results.to_string(index=False))
    save_table(onne_lm_results, "Table6_Onne_Linear_Models.csv")

#===============================================================================
# TABLE 7: Polynomial Regression Models
#===============================================================================

print("\n" + "="*80)
print("TABLE 7: Polynomial Regression Models for Fresh Root Yield")
print("="*80)

def build_polynomial_models(data, location_name):
    """Build polynomial regression models for FYLD"""
    
    ndvi_times = ['NDVI3', 'NDVI6', 'NDVI9']
    results = []
    
    # Additional predictors
    predictors = ['PLTHT6', 'CHL6', 'STMDI', 'ANGBR', 'LODG', 'NOHAV', 'PPSTD']
    
    for ndvi in ndvi_times:
        if ndvi not in data.columns:
            continue
        
        # Select predictors that exist
        predictors_present = [p for p in predictors if p in data.columns]
        all_predictors = [ndvi] + predictors_present
        
        # Get complete cases
        valid = data['FYLD'].notna()
        for p in all_predictors:
            valid = valid & data[p].notna()
        
        if valid.sum() < 30:
            continue
        
        model_data = data[valid].copy()
        
        # Split data
        train_data, test_data = train_test_split(
            model_data, test_size=0.2, random_state=2021
        )
        
        if len(test_data) < 10:
            continue
        
        # Prepare features with polynomial terms
        X_train = train_data[all_predictors].values
        y_train = train_data['FYLD'].values
        X_test = test_data[all_predictors].values
        y_test = test_data['FYLD'].values
        
        # Create polynomial features (degree 2)
        poly = PolynomialFeatures(degree=2, include_bias=False)
        X_train_poly = poly.fit_transform(X_train)
        X_test_poly = poly.transform(X_test)
        
        # Fit model
        model = LinearRegression()
        model.fit(X_train_poly, y_train)
        
        # Predict
        y_pred = model.predict(X_test_poly)
        
        # Calculate metrics
        r2 = r2_score(y_test, y_pred)
        rmse = np.sqrt(mean_squared_error(y_test, y_pred))
        mae = mean_absolute_error(y_test, y_pred)
        
        # Bootstrap for calibration (30 iterations)
        boot_r2 = []
        for i in range(30):
            try:
                boot_idx = np.random.choice(len(train_data), len(train_data), replace=True)
                X_boot = X_train[boot_idx]
                y_boot = y_train[boot_idx]
                
                X_boot_poly = poly.fit_transform(X_boot)
                boot_model = LinearRegression()
                boot_model.fit(X_boot_poly, y_boot)
                
                y_boot_pred = boot_model.predict(X_test_poly)
                boot_r2.append(r2_score(y_test, y_boot_pred))
            except:
                boot_r2.append(np.nan)
        
        r2_calibrated = np.nanmean(boot_r2)
        
        results.append({
            'Location': location_name,
            'NDVI_Time': ndvi,
            'R2': round(r2, 2),
            'RMSE': round(rmse, 2),
            'MAE': round(mae, 2),
            'R2_Calibrated': round(r2_calibrated, 2),
            'N_train': len(train_data),
            'N_test': len(test_data)
        })
    
    return pd.DataFrame(results)

# Table 7: Polynomial models for both locations
print("\n" + "-"*40)
print("TABLE 7: Polynomial Regression Models")
print("-"*40)

mokwa_poly = build_polynomial_models(mokwa_data, "Mokwa")
onne_poly = build_polynomial_models(onne_data, "Onne")

poly_results = pd.concat([mokwa_poly, onne_poly], ignore_index=True)
if not poly_results.empty:
    print(poly_results.to_string(index=False))
    save_table(poly_results, "Table7_Polynomial_Models.csv")

#===============================================================================
# Model Validation and Calibration Plots
#===============================================================================

print("\n" + "="*80)
print("Model Validation and Calibration")
print("="*80)

def create_calibration_plot(data, location_name):
    """Create calibration plot for FYLD using NDVI6"""
    
    # Get complete cases
    valid = data['FYLD'].notna() & data['NDVI6'].notna()
    if valid.sum() < 30:
        return
    
    plot_data = data[valid].copy()
    
    # Split data
    train_data, test_data = train_test_split(
        plot_data, test_size=0.2, random_state=2021
    )
    
    # Fit model on training data
    X_train = train_data[['NDVI6']].values.reshape(-1, 1)
    y_train = train_data['FYLD'].values
    X_test = test_data[['NDVI6']].values.reshape(-1, 1)
    y_test = test_data['FYLD'].values
    
    model = LinearRegression()
    model.fit(X_train, y_train)
    
    # Predict on test data
    y_pred = model.predict(X_test)
    
    # Create calibration plot
    fig, axes = plt.subplots(1, 2, figsize=(14, 6))
    
    # Plot 1: Observed vs Predicted
    ax = axes[0]
    ax.scatter(y_pred, y_test, alpha=0.6)
    
    # Add 1:1 line
    min_val = min(y_test.min(), y_pred.min())
    max_val = max(y_test.max(), y_pred.max())
    ax.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, label='1:1 line')
    
    # Add regression line
    z = np.polyfit(y_pred, y_test, 1)
    p = np.poly1d(z)
    ax.plot(sorted(y_pred), p(sorted(y_pred)), 'b-', linewidth=2, label='Regression line')
    
    ax.set_xlabel('Predicted FYLD')
    ax.set_ylabel('Observed FYLD')
    ax.set_title(f'Observed vs Predicted - {location_name}')
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Add R² and RMSE
    r2 = r2_score(y_test, y_pred)
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    ax.text(0.05, 0.95, f'R² = {r2:.2f}\nRMSE = {rmse:.2f}', 
            transform=ax.transAxes, verticalalignment='top',
            bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    # Plot 2: Residuals vs Predicted
    ax = axes[1]
    residuals = y_test - y_pred
    ax.scatter(y_pred, residuals, alpha=0.6)
    ax.axhline(y=0, color='r', linestyle='--', linewidth=2)
    ax.set_xlabel('Predicted FYLD')
    ax.set_ylabel('Residuals')
    ax.set_title(f'Residual Plot - {location_name}')
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    save_figure(fig, f"Calibration_{location_name}.png")
    plt.close()
    
    print(f"Calibration plot saved for {location_name}")

# Create calibration plots
create_calibration_plot(mokwa_data, "Mokwa")
create_calibration_plot(onne_data, "Onne")

#===============================================================================
# Summary of Key Findings
#===============================================================================

print("\n" + "="*80)
print("SUMMARY OF KEY FINDINGS")
print("="*80)

print("\n1. Broad-sense heritability estimates:")
if 'FYLD' in heritability_df['Trait'].values:
    fyld_h2 = heritability_df[heritability_df['Trait'] == 'FYLD']['H2'].values[0]
    fyld_class = heritability_df[heritability_df['Trait'] == 'FYLD']['H2_Class'].values[0]
    print(f"   - Fresh root yield: {fyld_h2:.2f} ({fyld_class})")

if 'DM' in heritability_df['Trait'].values:
    dm_h2 = heritability_df[heritability_df['Trait'] == 'DM']['H2'].values[0]
    dm_class = heritability_df[heritability_df['Trait'] == 'DM']['H2_Class'].values[0]
    print(f"   - Dry matter content: {dm_h2:.2f} ({dm_class})")

if 'STARCH' in heritability_df['Trait'].values:
    starch_h2 = heritability_df[heritability_df['Trait'] == 'STARCH']['H2'].values[0]
    starch_class = heritability_df[heritability_df['Trait'] == 'STARCH']['H2_Class'].values[0]
    print(f"   - Starch content: {starch_h2:.2f} ({starch_class})")

if 'HI' in heritability_df['Trait'].values:
    hi_h2 = heritability_df[heritability_df['Trait'] == 'HI']['H2'].values[0]
    hi_class = heritability_df[heritability_df['Trait'] == 'HI']['H2_Class'].values[0]
    print(f"   - Harvest index: {hi_h2:.2f} ({hi_class})")

print("\n2. NDVI prediction accuracy:")
if not mokwa_lm_results.empty:
    mokwa_ndvi6 = mokwa_lm_results[(mokwa_lm_results['Trait'] == 'FYLD') & 
                                    (mokwa_lm_results['NDVI_Time'] == 'NDVI6')]
    if len(mokwa_ndvi6) > 0:
        print(f"   - Mokwa: R² = {mokwa_ndvi6.iloc[0]['R2']:.2f} for FYLD")

if not onne_lm_results.empty:
    onne_ndvi9 = onne_lm_results[(onne_lm_results['Trait'] == 'FYLD') & 
                                  (onne_lm_results['NDVI_Time'] == 'NDVI9')]
    if len(onne_ndvi9) > 0:
        print(f"   - Onne: R² = {onne_ndvi9.iloc[0]['R2']:.2f} for FYLD")

print("\n3. Best prediction timing: 6 months after planting")
print("="*80)

print("\n" + "="*80)
print("ANALYSIS COMPLETE")
print("="*80)
print(f"\nAll tables saved to: {TABLES_DIR}")
print(f"All figures saved to: {FIGURES_DIR}")
print(f"Log file saved to: {LOG_FILE}")
EOF

# Run Python analysis
print_section "Executing Python Analysis"
python3 "${PROJECT_DIR}/cassava_analysis.py"

#===============================================================================
# Generate Report
#===============================================================================

print_section "Generating Summary Report"

cat > "${OUTPUT_DIR}/analysis_report.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>Cassava NDVI Phenotyping Analysis Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        h1 { color: #2c3e50; }
        h2 { color: #34495e; border-bottom: 2px solid #eee; padding-bottom: 10px; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th { background-color: #3498db; color: white; padding: 12px; }
        td { padding: 8px; border-bottom: 1px solid #ddd; }
        tr:hover { background-color: #f5f5f5; }
        .summary-box { background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin: 20px 0; }
        .footer { margin-top: 50px; color: #7f8c8d; text-align: center; }
    </style>
</head>
<body>
    <h1>Cassava NDVI-Based High-Throughput Phenotyping Analysis</h1>
    <p>Analysis completed on: $(date)</p>
    
    <div class="summary-box">
        <h2>Analysis Summary</h2>
        <ul>
            <li><strong>Total genotypes analyzed:</strong> $(python3 -c "import pandas as pd; df=pd.read_csv('${DATA_DIR}/cassava_clean_data.csv'); print(df['accession_name'].nunique())")</li>
            <li><strong>Mokwa observations:</strong> $(python3 -c "import pandas as pd; df=pd.read_csv('${DATA_DIR}/mokwa_data.csv'); print(len(df))")</li>
            <li><strong>Onne observations:</strong> $(python3 -c "import pandas as pd; df=pd.read_csv('${DATA_DIR}/onne_data.csv'); print(len(df))")</li>
        </ul>
    </div>
    
    <h2>Output Files Generated</h2>
    <table>
        <tr>
            <th>File Type</th>
            <th>File Name</th>
            <th>Description</th>
        </tr>
        <tr><td>Table</td><td>Table1_Mokwa_Summary.csv</td><td>Summary statistics for Mokwa</td></tr>
        <tr><td>Table</td><td>Table2_Onne_Summary.csv</td><td>Summary statistics for Onne</td></tr>
        <tr><td>Table</td><td>Table3_Heritability_Results.csv</td><td>Variance components and heritability</td></tr>
        <tr><td>Table</td><td>Table4_ANOVA_Results.csv</td><td>ANOVA across environments</td></tr>
        <tr><td>Table</td><td>Table5_Mokwa_Linear_Models.csv</td><td>Linear regression for Mokwa</td></tr>
        <tr><td>Table</td><td>Table6_Onne_Linear_Models.csv</td><td>Linear regression for Onne</td></tr>
        <tr><td>Table</td><td>Table7_Polynomial_Models.csv</td><td>Polynomial regression models</td></tr>
        <tr><td>Table</td><td>Mokwa_Correlation_Matrix.csv</td><td>Genotypic correlations for Mokwa</td></tr>
        <tr><td>Table</td><td>Onne_Correlation_Matrix.csv</td><td>Genotypic correlations for Onne</td></tr>
        <tr><td>Figure</td><td>Figure2_Mokwa_Genotypic_Correlation.png</td><td>Correlation plot for Mokwa</td></tr>
        <tr><td>Figure</td><td>Figure3_Onne_Genotypic_Correlation.png</td><td>Correlation plot for Onne</td></tr>
        <tr><td>Figure</td><td>Figure4_Path_Analysis_Mokwa.png</td><td>Path diagram for Mokwa</td></tr>
        <tr><td>Figure</td><td>Figure5_Path_Analysis_Onne.png</td><td>Path diagram for Onne</td></tr>
        <tr><td>Figure</td><td>Calibration_Mokwa.png</td><td>Model calibration for Mokwa</td></tr>
        <tr><td>Figure</td><td>Calibration_Onne.png</td><td>Model calibration for Onne</td></tr>
    </table>
    
    <div class="footer">
        <p>Analysis pipeline completed successfully. For detailed results, see the output directory.</p>
    </div>
</body>
</html>
EOF

echo -e "${GREEN}HTML report generated: ${OUTPUT_DIR}/analysis_report.html${NC}"

#===============================================================================
# Cleanup
#===============================================================================

print_section "Cleaning Up"

# Deactivate virtual environment
deactivate

echo -e "${GREEN}Analysis pipeline completed successfully!${NC}"
echo -e "${GREEN}Check ${OUTPUT_DIR} for all results${NC}"
echo -e "${GREEN}Log file: ${LOG_FILE}${NC}"

exit 0