# Use an official R runtime as a parent image
# The 4.4.0 version is choosen for compatibility with libpq-dev library
FROM rocker/rstudio:4.2.2
#3.6.0
# try to run in Rstudio image
# Install required libraries
RUN apt-get update && \
  apt-get install -y libglpk-dev && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN install2.r --error \
  shiny \
  lavaan \
  semPlot \
  rhandsontable \
  semTools \
  tidyr

ENV PROJECT_ROOT=/home/rstudio/src
