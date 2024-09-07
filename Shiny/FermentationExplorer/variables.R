# Load Internal Data for Shiny App
# This script defines variables for the app.  These are
# variables used by multiple modules.  
# Author: Timothy Hackmann
# Date: 16 August 2024

#Colors for plots
  #Matches those in Powerpoint
  red_color=rgb(red=255, green=0, blue=0, maxColorValue = 255)
  gold_color=rgb(red=255, green=192, blue=0, maxColorValue = 255)
  green_color=rgb(red=0, green=176, blue=80, maxColorValue = 255)
  blue_color=rgb(red=0, green=112, blue=192, maxColorValue = 255)
  purple_color=rgb(red=112, green=48, blue=160, maxColorValue = 255)
  grey_color = rgb(red=127, green=127, blue=127, maxColorValue = 255) 
  
#Links to external websites
  url_GOLD <- a("GOLD", href="https://gold.jgi.doe.gov/", target="_blank")
  url_IMG <- a("IMG/M", href="https://img.jgi.doe.gov/m/", target="_blank")
  url_JGI <- a("this notice.", href="https://jgi.doe.gov/disclaimer/", target="_blank")
  url_BacDive <- a("BacDive", href="https://bacdive.dsmz.de/", target="_blank")
  url_CC = a("CC by 4.0 license", href="https://creativecommons.org/licenses/by/4.0/", target="_blank")
  url_Bergey = a("Bergey's Manual of Systematics of Archaea and Bacteria", href="https://onlinelibrary.wiley.com/doi/book/10.1002/9781118960608", target="_blank")
  url_fairuse = a("fair use", href="https://www.copyright.gov/fair-use/", target="_blank")
  url_KAAS =  a("KAAS", href="https://www.genome.jp/kegg/kaas/", target="_blank")
  url_QIIME2 = a("QIIME2", href="https://qiime2.org/", target="_blank")
  url_DADA2 = a("DADA2", href="https://benjjneb.github.io/dada2/", target="_blank")
  url_fbar = a("fbar", href="https://cran.r-project.org/web/packages/fbar/index.html", target="_blank")
  url_FermentationExplorer = a("this manuscript", href="https://www.science.org/doi/10.1126/sciadv.adg8687", target="_blank")
