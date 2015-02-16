[![Travis-CI Build Status](https://travis-ci.org/AndySouth/rtsetse.png?branch=master)](https://travis-ci.org/AndySouth/rtsetse)

rtsetse
========================================================

rtsetse is an R package for simulating tsetse fly populations to investigate control options.

It follows from [Hat-Trick](http://www.tsetse.org), an Excel Tsetse model.

Development by Andy South in 2014-15. You are welcome to get in touch with comments or questions.

### Installation

    require(devtools)    
    install_github('AndySouth/rtsetse')     


### Code structure
The simulation has been built as a series of components that can be tested in isolation and can contribute to a maintainable and modifiable tool.

Function names in the package are preceded by rt. Functions called rt_run*() perform simulation runs.


rtsetse function | purpose   
------------------------ | -------------
rt_runAspatial()         | age-structured a-spatial population simulation
rt_runGridTestSpread()   | grid simulation, homogenous landscape, spread fro central populated cell
rt_runGrid()             | grid simulation, landscape set from vegetation map, population starts at carrying capacity

User interfaces can be accessed on the web at the links below and are available from a separate [https://github.com/AndySouth/shinytse](repository).

Shiny app link  | What it does  | To run locally
------------- | -------------| -------------
[shinytse7](http://andysouth.shinyapps.io/shinytse7/) | Offer options to run aspatial and gridded models and visualise outputs | require(devtools)<br>install_github('AndySouth/rtsetse')<br>shiny::runGitHub('AndySouth/shinytse', subdir='shinytse7') 
