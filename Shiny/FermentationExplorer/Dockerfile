#Get base Docker image
FROM rocker/shiny:latest

#Install Python libraries
#RUN apt-get update && apt-get install -y --no-install-recommends build-essential python3-dev python3-pip python3-venv

#Install additional libraries
RUN apt-get update && apt-get install libcurl4-openssl-dev libxml2-dev -y &&\
  mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install R packages
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fbar', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmltools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('igraph', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('treemap', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('harveyl888/queryBuilder')"

#Copy the app and data to Docker image
COPY *.Rproj /srv/shiny-server/
COPY *.R /srv/shiny-server/
COPY data /srv/shiny-server/data
COPY www /srv/shiny-server/www

#Select port
EXPOSE 3838

#Allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

#Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

#Run app
CMD ["/usr/bin/shiny-server.sh"]
