# IR-TEx

Insecticide Resistance Transcript Explorer is an interactive shiny app designed to allow the user to explore all currently available microarray data comparing resistant and susceptible Anopheles gambiae, coluzzi or arabiensis. In addition to transcript fold changes and associated significance, both a map of transcript expression and a correlation network are generated, the former showing expression across Africa and the latter, allowing the user to infer pathways and transcript function.

## Installing IR-TEx Application

Download the latest version of R software, available at https://cran.r-project.org/mirrors.html

Install R Software

Download the latest version of R studio, available at https://www.rstudio.com/products/rstudio/download/

Install R Studio

Download the .zip file containing the following: IR-TEx.R, Fold Changes.txt and geography.txt. Unzip these files to a suitable, locatable folder on your machine.

Locate R Studio on your machine, and run the software. Click File -> Open File... and locate the IR-TEx.R file that you have unzipped.

**On the first run ONLY**
Type the following into the R command line (as indicated by > and a flashing cursor line)

~~~~
install.packages('shiny')
install.packages('ggmap')
install.packages('mapproj')
install.packages('shinycssloaders')
install.packages('shinythemes')
~~~~

After these files have installed, click 'Run App' under the IR-TEx tab. One tha app is running, click 'Open in Browser' for full functionality.
