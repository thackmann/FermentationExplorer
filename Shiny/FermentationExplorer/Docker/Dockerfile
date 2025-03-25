#Get base Docker image
FROM rocker/shiny:latest

# Install system dependencies for Plotly and other packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libboost-all-dev \
    pandoc \
    pandoc-citeproc \
    libglpk-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required by the Shiny app
RUN R -e "install.packages('ape')"
RUN R -e "install.packages('bslib')"
RUN R -e "install.packages('colorspace')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('fbar')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('grDevices')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('import')"
RUN R -e "install.packages('jqbr')"
RUN R -e "install.packages('phyloseq')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('rlang')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinycssloaders')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "remotes::install_github('YuLab-SMU/ggtree')"

# Copy app files
COPY . /srv/shiny-server/

#Select port
EXPOSE 3838

#Allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

#Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

#Run app
CMD ["/usr/bin/shiny-server.sh"]
