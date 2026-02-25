#!/bin/bash
#===============================================================================
# SCRIPT: create_anonymous_link.sh
# PURPOSE: Creates an anonymous review link
#===============================================================================

REPO_URL="https://github.com/Abiodun-Olayinka/cassava-genetics-analysis"
OUTPUT_FILE="anonymous_review_link.txt"

echo "================================================================================"
echo "ANONYMOUS REVIEW LINK GENERATOR"
echo "================================================================================"
echo ""
echo "Repository URL: $REPO_URL"
echo ""
echo "Please complete these steps in your browser:"
echo ""
echo "1. Go to: https://anonymous.4open.science"
echo "2. Click 'Upload your code for anonymous review'"
echo "3. Authenticate with GitHub (login: Abiodun-Olayinka)"
echo "4. Enter repository URL: $REPO_URL"
echo "5. Check 'I need a double-blind review'"
echo "6. Click 'Create anonymous repository'"
echo "7. Copy the generated anonymous link"
echo ""
read -p "Paste the anonymous link here: " ANONYMOUS_LINK

if [[ $ANONYMOUS_LINK == *"anonymous.4open.science"* ]]; then
    echo "$ANONYMOUS_LINK" > "$OUTPUT_FILE"
    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    echo "$ANONYMOUS_LINK" > "anonymous_link_$TIMESTAMP.txt"
    echo ""
    echo "✅ Anonymous link saved to: $OUTPUT_FILE"
    echo "✅ Link: $ANONYMOUS_LINK"
    echo ""
    echo "📁 Link also saved to: anonymous_link_$TIMESTAMP.txt"
else
    echo ""
    echo "❌ Invalid link. Please make sure it contains 'anonymous.4open.science'"
    exit 1
fi
