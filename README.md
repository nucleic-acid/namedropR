
<!-- README.md is generated from README.Rmd. Please edit that file -->

# namedropR

<!-- badges: start -->
<!-- badges: end -->

`namedropR` provides ‘visual citations’ containing the metadata of a
scientific paper and a ‘QR’ code.

## Installation

You can install the development version of namedropR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nucleic-acid/namedropR")
```

## Visual Citations

A ‘visual citation’ is a banner containing an article’s title, authors,
journal and year of a publication (s. example below).

<img src="man/figures/collaboration_2019_ApJL.png" align="right" alt="Sharingan" width="60%" style="box-shadow: 8px 8px 6px grey;"/>

One might want to include this in a presentation

-   to back up one’s claims,
-   to drop an important name,
-   to boast about a publication in a ‘high impact journal’.

*Some might even use this to genuinely **point the audience to good
resources** for further reading.*

On conferences, such banners are frequently displayed way too short for
the audience to actually comprehend them (and often in a bad
resolution). Creating visual citations requires manually taking a
screenshot and placing it on the slide.

`namedropR` helps to generate visual citations conveniently (see below),
in high resolution and with a QR code. This allows the audience to
follow the reference *while you are talking about it*, instead of
looking it up in a reference list at the end of your talk.

## Basic Usage

This package accepts bibliographic information as ‘BibTeX’ and
‘BibLaTeX’ references and includes a QR code pointing to the
[‘DOI’](https://www.doi.org). If the ‘DOI’ is not available in the
bibliography entry, but a ‘URL’ field instead, this is used. If neither
is given, the QR code points to a search call at
<https://scholar.google.com> with the available data as search terms.

``` r
bib <- RefManageR::ReadBib(bib_path)

# create a visual citation as PNG with 'modern' design
drop_name(bib, cite_key = "SomeAuthor2010", export_as = "png", style = "modern")

# create a visual citation as HTML with 'classic design'
drop_name(bib, cite_key = "SomeAuthor2010", export_as = "html", style = "classic")

# drop_name() by default returns the file path, 
# where the visual citation was stored as character string. 
# Within an Rmarkdown/HTML presentation you can 
# include the banner conveniently like so:

# PNG
knitr::include_graphics(drop_name(bib, cite_key = "SomeAuthor2010", export_as = "png", style = "clean"))

# HTML
htmltools::includeHTML(drop_name(bib, cite_key = "SomeAuthor2010", export_as = "html", style = "clean", use_xaringan = TRUE))
```

Styling is possible via predefined designs or via custom ‘CSS’ to match
the design of the HTML presentation like e.g. ‘xaringan’ or ‘revealJS’
(see the vignette for more options).

## Notes

This package is not intended as replacement for proper reference manager
packages, but a tool to enrich scientific presentation slides. Hence the
functionality is quite limited to this specific use case.
