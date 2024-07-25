

<!-- README.md is generated from README.qmd. Please edit that file -->

# Pandemic Pass? Treaty Derogations and Human Rights Practices During COVID-19

<!-- badges: start -->

[![OSF
DOI](https://img.shields.io/badge/OSF-10.17605%2FOSF.IO%2FAQVNK-blue)](https://doi.org/10.17605/OSF.IO/AQVNK)
[![Code
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12817616.svg)](https://doi.org/10.5281/zenodo.12817616)
<!-- badges: end -->

[Suparna Chaudhry](https://www.suparnachaudhry.com/) • Lewis and Clark
College  
[Audrey Comstock](https://audreylcomstock.weebly.com/) • Arizona State
University  
[Andrew Heiss](https://www.andrewheiss.com/) • Andrew Young School of
Policy Studies • Georgia State University

------------------------------------------------------------------------

## Abstract

This research note asks whether states issuing pandemic-era human rights
treaty derogations implemented emergency provisions as intended or used
them to abuse human rights during a time of crisis. In an effort to
combat the COVID-19 pandemic, many countries declared states of
emergency and derogated (temporarily suspended) from their international
human rights treaty obligations. Using data from the Varieties of
Democracy PanDem dataset and the Oxford COVID-19 Government Response
Tracker, we find that states that derogated from their international
human rights obligations imposed emergency measures that were temporary
and did not violate non-derogable rights. On the other hand, states that
did not derogate were more likely impose discriminatory measures, enact
emergency measures without time limits and violate non-derogable rights.
Our results support the role that flexibility mechanisms such as
derogations play in international law and show that states are being
sincere about their intentions and not, generally, using these
mechanisms to cover abusive behavior.

------------------------------------------------------------------------

## How to download and replicate

You can either [download the compendium as a ZIP
file](./archive/main.zip) or use GitHub to clone or fork the compendium
repository (see the green “Clone or download” button at the top of the
GitHub page).

We use the [{renv}
package](https://rstudio.github.io/renv/articles/renv.html) to create a
stable version-specific library of packages, and we use the [{targets}
package](https://docs.ropensci.org/targets/) to manage all file
dependencies and run the analysis. ([See this for a short helpful
walkthrough of
{targets}.](https://books.ropensci.org/targets/walkthrough.html)).

To replicate the findings and re-run the analysis, you can use a Docker
container at
[`mountainous-mackerel-docker`](https://github.com/andrewheiss/mountainous-mackerel-docker),
which will create a complete computing envirionment for running
everything. Full details and instructions [are available
there](https://github.com/andrewheiss/mountainous-mackerel-docker).

Alternatively, everything can be run on your local computer too. Do the
following:

1.  Download and install these fonts (if you’re using Windows, make sure
    you right click on the font files and choose “Install for all users”
    when installing these fonts):
    - [Noto Sans](https://fonts.google.com/specimen/Noto+Sans)
    - [Linux Libertine
      O](https://www.cufonfonts.com/font/linux-libertine-o) (also
      [here](https://sourceforge.net/projects/linuxlibertine/))
    - [Libertinus Math](https://github.com/alerque/libertinus)
2.  [Install R](https://cloud.r-project.org/) (and preferably
    [RStudio](https://www.rstudio.com/products/rstudio/download/#download)).
    - If you’re using macOS, [install XQuartz
      too](https://www.xquartz.org/), so that you have access to the
      Cairo graphics library
    - If you’re using Windows, [install RTools
      too](https://cran.r-project.org/bin/windows/Rtools/) and add it to
      your PATH so that you can install packages from source if needed
3.  Open `mountainous-mackerel.Rproj` to open an [RStudio
    Project](https://r4ds.had.co.nz/workflow-projects.html).
4.  In the terminal, run `quarto install tinytex` to ensure that you
    have a working LaTeX installation.
5.  If it’s not installed already, R *should* try to install the {renv}
    package when you open the RStudio Project for the first time. If you
    don’t see a message about package installation, install it yourself
    by running `install.packages("renv")` in the R console.
6.  Run `renv::restore()` in the R console to install all the required
    packages for this project.
7.  Run `targets::tar_make()` in the R console to automatically download
    all data files, process the data, run the analysis, and compile the
    paper and appendix.

Running `targets::tar_make()` will create several helpful outputs:

1.  All project data in `data/`
2.  An analysis notebook website in `_site/`
3.  PDF, HTML, and Word versions of the manuscript in
    `manuscript/output/`

## 🏔️🐟: Note on “mountainous mackerel” project name

Because project titles change all the time with revisions, rewriting,
and peer review, we used [{codename}](http://svmiller.com/codename/) to
generate an [Ubuntu-style](https://wiki.ubuntu.com/DevelopmentCodeNames)
internal-to-us project name that won’t change.

``` r
library(codename)
codename_message()
#> code name generated by {codename} v.0.4.0

codename(seed = "covid and ngos", type = "ubuntu")
#> [1] "mountainous mackerel"
```

## Licenses

**Text and figures:** All prose and images are licensed under Creative
Commons ([CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)).

**Code:** All code is licensed under the [MIT License](LICENSE.md).

## Contributions and Code of Conduct

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
