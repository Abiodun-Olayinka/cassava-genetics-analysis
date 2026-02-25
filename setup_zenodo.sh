#!/bin/bash
#===============================================================================
# SCRIPT: setup_zenodo.sh
# PURPOSE: Guides Zenodo DOI setup
#===============================================================================

GITHUB_USERNAME="Abiodun-Olayinka"
REPO_NAME="cassava-genetics-analysis"
RELEASE_VERSION="v1.0.0"
DOI_OUTPUT_FILE="zenodo_doi_info.txt"

echo "================================================================================"
echo "ZENODO DOI ARCHIVING SCRIPT"
echo "================================================================================"
echo ""
echo "Part A: Connect Zenodo to GitHub"
echo "--------------------------------"
echo "1. Go to: https://zenodo.org"
echo "2. Click 'Sign in with GitHub'"
echo "3. Authorize Zenodo"
echo "4. Go to profile → 'GitHub'"
echo "5. Find '$REPO_NAME' and toggle ON"
echo ""
read -p "Press Enter after completing Part A... "

echo ""
echo "Part B: Create GitHub Release"
echo "------------------------------"
git tag -a "$RELEASE_VERSION" -m "Release for publication"
git push origin "$RELEASE_VERSION"

echo ""
echo "Part C: Create Release on GitHub"
echo "--------------------------------"
echo "1. Go to: https://github.com/$GITHUB_USERNAME/$REPO_NAME/releases"
echo "2. Click 'Create a new release'"
echo "3. Choose tag: $RELEASE_VERSION"
echo "4. Title: 'Cassava Genetics Analysis - Version for Publication'"
echo "5. Description: 'R scripts for path analysis, correlations, and NDVI models'"
echo "6. Click 'Publish release'"
echo ""
read -p "Press Enter after creating the release... "

echo ""
echo "Part D: Enter Your DOI"
echo "----------------------"
echo "Zenodo will email you the DOI. Enter it below:"
read -p "DOI (e.g., 10.5281/zenodo.XXXXXXX): " ZENODO_DOI

echo "$ZENODO_DOI" > "$DOI_OUTPUT_FILE"
echo "YOUR DOI: $ZENODO_DOI" >> "$DOI_OUTPUT_FILE"
echo ""
echo "✅ DOI saved to: $DOI_OUTPUT_FILE"
echo ""
echo "📝 Add this to your manuscript:"
echo "   \"The R scripts are permanently archived in Zenodo with the DOI: $ZENODO_DOI\""
