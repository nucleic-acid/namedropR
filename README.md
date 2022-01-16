
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

<img src="man/figures/collaboration_2019_ApJL.png" align="right" alt="A visual citation pointing to a scientific paper. Scan with a QR code scanner to follow the URL." width="500" style="box-shadow: 8px 8px 6px grey;"/>

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

<img src="man/figures/hawking_1973.png" align="left" alt="A compact visual citation pointing to a scientific paper. Scan with a QR code scanner to follow the URL." width="130" style="box-shadow: 5px 5px 6px grey;margin-right: 15px"/>

`namedropR` helps to generate visual citations conveniently (see below),
in high resolution and with a QR code. This allows the audience to
follow the reference *while you are talking about it*, instead of
looking it up in a reference list at the end of your talk.  
There is also a less intrusive, compact style, as seen on the left.

## Basic Usage

This package accepts bibliographic information as ‘BibTeX’ and
‘BibLaTeX’ references and includes a QR code pointing to the
[‘DOI’](https://www.doi.org). If the ‘DOI’ is not available in the
bibliography entry, but a ‘URL’ field instead, this is used. If neither
is given, the QR code points to a search call at
<https://scholar.google.com> with the available data as search terms.

``` r
bib_path <- "path/to/bibliography_file.bib"

# create a visual citation as PNG with 'modern' design
drop_name(bib_path, cite_key = "SomeAuthor2010", export_as = "png", style = "modern")

# create a visual citation as HTML with 'compact' design
drop_name(bib_path, cite_key = "SomeAuthor2010", export_as = "html", style = "compact")

# drop_name() by default returns the file path, 
# where the visual citation was stored as character string. 
# Within an Rmarkdown/HTML presentation you can create and 
# include the banner conveniently like so:

# PNG
knitr::include_graphics(drop_name(bib_path, cite_key = "SomeAuthor2010", export_as = "png", style = "clean"))

# HTML
htmltools::includeHTML(drop_name(bib_path, cite_key = "SomeAuthor2010", export_as = "html", style = "clean", use_xaringan = TRUE))

# To bulk-create VCs to include in another kind of document / presentation slides you can pass a vector of citation keys or pass no keys at all to render some or all bibliography entries respectively:

drop_name(bib_path, cite_key = c("SomeAuthor2010", "SomeOther2011", "YetAnother2012"), export_as = "png", style = "clean")

drop_name(bib_path, export_as = "png", style = "clean")
```

Styling is possible via predefined designs or via custom ‘CSS’ to match
the design of the HTML presentation like e.g. ‘xaringan’ or ‘revealJS’
(see the vignette for more options).

## Notes

-   This package is not intended as replacement for proper reference
    manager packages, but a tool to enrich scientific presentation
    slides. Hence the functionality is quite limited to this specific
    use case.
-   previous versions (before v2.0) allowed passing
    `RefManageR::BibEntry()` objects. This option was removed to
    eliminate dependency on the orphaned `{bibtex}` package. This might
    break code for users of initial releases of the package, but was
    needed to allow for CRAN submission.
