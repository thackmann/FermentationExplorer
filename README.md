<p align="center"> <a href="https://timothy-hackmann.shinyapps.io/FermentationExplorer/"><img src="https://github.com/thackmann/FermentationExplorer/blob/main/logo.svg" width="400"/></a> </p>

## A resource for showing the incredible diversity of fermentative metabolism 

### Web version
Click [here](https://timothy-hackmann.shinyapps.io/FermentationExplorer) to access.

### Download as R Shiny app
1)  In the menu above, click `Code` -> `Download ZIP`.
2) Upzip the downloaded folder, locate `app.R`, open in [R Studio](https://posit.co/download/rstudio-desktop/), and click `Run App`.

### Download as Docker image
1)  In command prompt, run
 `
docker run -p 3838:3838 tjhackmann/fermentationexplorer:latest
`
2)  Open browser with address http://localhost:3838/.  Refresh browser periodically until app loads.

### More information
Hackmann TJ, Zhang B. The phenotype and genotype of fermentative prokaryotes. Sci Adv. 2023 Sep 29;9(39):eadg8687. doi: 10.1126/sciadv.adg8687 [PMID 37756392](https://pubmed.ncbi.nlm.nih.gov/37756392/)

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
[![DOI](https://zenodo.org/badge/579197407.svg)](https://zenodo.org/badge/latestdoi/579197407)
