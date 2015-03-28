# Overview
This repository is for ongoing projects involving analysis of the economic impact of broadband technologies across the United States. The main goals of the analysis are:

* Visualizing broadband growth in the United States.
* Determining whether there is a causal connection between broadband infrastructure and increases in employment.
* Estimating economic impacts associated with broadband deployment.

# Order of Execution
This project utilizes R and RStudio for downloading, tidying, summarizing, and analyzing data across various sources.

0. file: 0-data-compile.R (some data files are not available due to confidentiality)
  + This is not optimized, it is best to compile all the data overnight.
1. file: 1-data.R will tidy up the data, (currently working on updating)
2. file: 2- ...

## Packages Needed
Do need to install the [readxl](https://github.com/hadley/readxl) and [readr](https://github.com/hadley/readr) packages from [Hadley Wickham's Github](https://github.com/hadley) with:

```R
# install.packages("devtools")
devtools::install_github("hadley/readxl")
devtools::install_github("hadley/readr")
```

Further, other packages needed include: `gdata`, `ggplot2`, `maptools`, `plyr`, `raster`, `reshape2`, `rgdal`, `spdep`, `xtable`.


<!--
# Cheat Sheet
Plain text
End a line with two spaces to start a new paragraph.  
*italics* and _italics_  
**bold** and __bold__  
superscript^2^  
~~strikethrough~~  
[link](www.rstudio.com)  

# Header 1  
## Header 2  
### Header 3  
#### Header 4  
##### Header 5  
###### Header 6  

endash: --  
emdash: ---  
ellipsis: ...  
inline equation: $A = \pi*r^{2}$  
image: ![](RStudioSmall.png)  
horizontal rule (or slide break):

***

> block quote

* unordered list
* item 2
  + sub-item 1
  + sub-item 2

1. ordered list
2. item 2
  + sub-item 1
  + sub-item 2

Table Header  | Second Header
------------- |-------------
Table Cell    | Cell 2
Cell 3        | Cell 4

| Tables   |      Are      |  Cool |
|----------|:-------------:|------:|
| col 1 is |  left-aligned | $1600 |
| col 2 is |    centered   |   $12 |
| col 3 is | right-aligned |    $1 |
-->