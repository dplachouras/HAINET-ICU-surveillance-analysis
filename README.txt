HAI-Net ICU surveillance data download, analysis and report production
9 July 2026

1. Download data (data_download.R in R folder or download csv files from TESSy to raw folder) - needs haidatamanager library from Azure devops repository
2. [Move data from data/HAIICU folder to data/raw folder if data_download.R used] - to be streamlined
3. Clean and analyse the data with haiicu_cleaning.R
4. Produce report in MS Word or html format with running render_haineticu from shell (Rscript R/render_haineticu.R word or Rscript R/render_haineticu.R html)
5. [Produce html report with Quarto - under development]