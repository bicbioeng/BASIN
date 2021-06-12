# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libfftw3-3 \
    libtiff5-dev \
    libwebp-dev \
    libzstd-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
COPY /tryBASIN ./app

# install packages
RUN Rscript -e 'install.packages("BiocManager") '
RUN Rscript -e 'install.packages("ijtiff")'
RUN Rscript -e 'BiocManager::install("EBImage")'
RUN Rscript -e 'install.packages("plyr")'
RUN Rscript -e 'install.packages("shinyBS")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("shinycssloaders")'
RUN Rscript -e 'install.packages("shinyWidgets")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("ggpubr")'
RUN Rscript -e 'install.packages("autothresholdr")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
