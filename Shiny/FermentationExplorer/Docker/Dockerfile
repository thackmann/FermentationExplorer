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
    libglpk-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required by the Shiny app
RUN R -e "install.packages(c( \
  'ape', 'bslib', 'colorspace', 'dplyr', 'DT', 'fbar', 'ggplot2', 'grDevices', \
  'igraph', 'import', 'jqbr', 'phyloseq', 'plotly', 'remotes', 'rlang', \
  'shiny', 'shinycssloaders', 'shinyjs', 'shinyWidgets', 'stringr', 'tidyverse', \
  'BiocManager', 'caret', 'randomForest', 'reactlog', 'rsconnect' \
))"

# Install GitHub packages
RUN R -e "remotes::install_github('YuLab-SMU/ggtree'); remotes::install_github('thackmann/FileLocator/FileLocator')"

# Copy app files
COPY . /srv/shiny-server/

#Select port
EXPOSE 3838

#Allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

#Copy further configuration files into the Docker image
COPY ../Docker/shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

#Run app
CMD ["/usr/bin/shiny-server.sh"]
