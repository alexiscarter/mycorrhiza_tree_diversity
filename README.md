# mycorrhiza_tree_diversity

This repository contains the custom code to replicate the analyses published in Carteron Alexis, Vellend Mark and Laliberté Etienne. Mycorrhizal dominance reduces local tree species diversity across US forests. Preprint: https://doi.org/10.1101/2021.01.23.427902. Published article: [add DOI].

For this study, we used publicly available data from the U.S. Department of Agriculture Forest Service, known as the Forest Inventory and Analysis (FIA) program. Data can be accessed from https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html. For more information see https://www.fia.fs.fed.us/library/database-documentation/index.php.  
Mycorrhizal types for each tree species was determined using a published database available at https://doi.org/10.4231/R76D5R7S.

The scripts are ordered in this way:
- `import.R` (for accessing the data)
- `manipulation.R` (for processing the data)
- `modeling.R` (for modeling)
- `null_model.R` (for the null model approach)
- `figure.R` (for the figures)
