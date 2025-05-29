# Kiva Lender Behavior Analysis

This project analyzes lender behavior on [Kiva](https://www.kiva.org/), a microfinance crowdfunding platform, using real-world datasets.  
Our goal was to identify meaningful lender profiles based on their past lending patterns and preferences, and build a model to predict lender types for personalized loan recommendations.

---

## Objectives

- Explore how different lender characteristics (like occupation, gender preferences, loan amounts, etc.) influence lending decisions.
- Apply clustering to segment lenders into behavioral groups.
- Train a classification model to predict these groups based on engineered features.

---

## Project Structure

- `data/`: Raw input datasets from the Kiva platform (CSV files).
- `scripts/`: R scripts for data preprocessing, clustering, feature engineering, and modeling.
- `report/`: Final PDF report summarizing methods and findings.
- `README.md`: This file.

---

## Tools & Techniques

- Language: **R**
- Data Manipulation: `dplyr`, `tidyr`, `stringr`
- Clustering: `kmeans`
- Modeling: `randomForest`
- Visualization: `ggplot2`, `factoextra`
- Feature Engineering: Custom rules and occupation classification

---

## Results

- Used **12-cluster k-means** to identify lender groups
- Engineered features included:
  - Gender preference of borrower
  - Average borrower age
  - Loan sector & loan size ranges
  - Most common borrower region (continent/country)
  - Classified occupations into higher-level categories
- Achieved **80.9% accuracy** using a Random Forest model to predict cluster membership
- Created summary visualizations to explain behavior patterns per group

---

## Authors

Project by:
- Shaked Shabat
- Oren Raz
- Ilay Damary
- Guy Dulberg

Done as part of our Data Analysis course.

---

## License

This project is for educational and demonstration purposes only. Not affiliated with Kiva.org.

