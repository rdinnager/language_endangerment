# Global Language Endangement Data Wrangling Code

This code combines a large number of disparate data sources into one data file, matching by language ISO codes and shared geographic distributions. It uses a `drake` project. `drake` is an R package providing a 'make-like' declarative workflow for projects. To run the workflow, run `drake::r_make()`, with the working directory set to this project directory. Alternatively, the `drake` workflow has been converted into a sequential R script '`LangEnd_plan_code.R`' using `drake::plan_to_code()`.

Note, there are a number of data sources missing from this repository, as we do not have the rights to make them publically available, or they are already publically available elsewhere, and interested parties should download the data from those source.

# Missing sources
- **World Language Mapping System** - This data is privately licensed and can be found here: https://www.worldgeodatasets.com/language/. We used editions 16 and 17, and the files from these datasets were found in the following subfolders under the `data/` subfolder:
  * `lang`
  * `lang_bnd`
  * `lang_dbf`
  * `lang_pt`
- **IUCN threatened species status** - We used `.shp` files for amphibian and mammal species, which can be found here: https://www.iucnredlist.org/resources/spatial-data-download. `.shp` files were placed in the following subfolders under the `data/` subfolder:
  * `IUCN/amphibians`
  * `IUCN/mammals`
- **Human Footprint data** - We used data which can be found here: https://datadryad.org/stash/dataset/doi:10.5061/dryad.052q5. Data from this Dryad repository were unzipped into a `HumanFootprint` subfolder under the `data/` subfolder.

