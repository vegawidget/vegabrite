![lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

# vlmetabuildr

The goal of vlmetabuildr is to provide functions that can be used to build up an api for vega-lite in R... it is not a package meant to be used to actually build visualizations themselves, just to help build the actual package one would use: [vlbuildr](https://github.com/AliciaSchep/vlbuildr)

The main function is `create_api` which takes in a Vega-Lite schema and creates an API. Note that this function is likely not robust to versions of the schema substantiallly different from the one currently being used...

```r
library("vlmetabuildr")

schema_file <- Sys.glob(file.path(system.file("schema/vega-lite", package = "vegawidget"),"*.json"))
VL_SCHEMA <- jsonlite::read_json(schema_file)

r_api <- create_api(VL_SCHEMA)

r_file_path <- file.path(rprojroot::find_package_root_file(), "R","zzz_autogen_api.R")
cat(r_api, file = r_file_path)
```