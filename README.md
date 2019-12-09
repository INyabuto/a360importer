# a360importer <img src="man/figures/a360importer.png" height="139" align = "right"/>
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

a360importer is an R package used in the preparation, transformation and loading of A360 historical file into [production](https://data.psi-mis.org). 

It provides key functions for manipulating and uploading A360 historical files efficiently. 

- `load_files()` effectively loads A360 legacy files into R
- `remove_empty_rows()` checks for the empty rows on the file and removes them.
- `remove_nas()` check for 'NAs' and turns them into an empty strings
- `generate_payload()` compiles the files into a payload ready for upload. 
- `upload()` push the payloads into production  


## Installation

```r

devtools::install_github("INyabuto/a360importer")

```


