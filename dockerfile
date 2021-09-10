# Base image https://hub.docker.com/u/rocker/
FROM hvalev/shiny-server-arm
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
RUN apt-get update --fix-missing && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
COPY /tryBASIN ./app

# install packages
RUN R -e "install.packages(c('ijtiff'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('plyr', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('ggpubr', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('autothresholdr', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN apt-get -y install ca-certificates
RUN apt-get -y install curl libcurl4 libtiff5-dev libfftw3-dev
RUN Rscript -e "install.packages(c('tiff', 'jpeg', 'png', 'locfit', 'fftwtools', 'RCurl'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "BiocManager::install('EBImage', site_repository='http://bioconductor.org/packages/3.13/bioc')"
RUN Rscript -e "install.packages(c('rmarkdown'), repos='http://cran.rstudio.com/')"
RUN apt-get install -y pandoc
RUN cp -v /usr/bin/pandoc /usr/local/shiny-server/ext/pandoc/pandoc
COPY /tryBASIN /srv/shiny-server/

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
