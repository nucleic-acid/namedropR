`namedropR` provides 'visual citations' containing the metadata of a scientific paper and a 'QR' code.

A 'visual citation' is a banner containing title, authors, journal and year of a publication:

![](man/figures/collaboration_2019_ApJL.png)

One might want to include this in a presentation 

- to back up one's claims, 
- to drop an important name, 
- to boast about a publication in a 'high impact journal'

*Some might even use this to genuinely **point the audience to good resources** for further reading.*

As seen in the wild, such banners are often pixelated screenshots thrown on a presentation slide and displayed way too short for the audience to actually comprehend them. `namedropR` on the other hand facilitates to show a reference *while you are talking about it* and allows the audience to follow the reference in the same time, instead of looking it up in a reference list at the end of a talk.

This package creates such banners based on 'BibTeX' and 'BibLaTeX' references
and includes a QR code pointing to the ['DOI'](https://doi.org). 
Styling is possible via predefined designs or via custom 'CSS'. By that adaption to the design of HTML presentations like e.g. 'xaringan' or 'revealJS' is possible.

To install the latest version from `GitHub`:

```
install.packages("remotes")
remotes::install_github("nucleic-acid/namedropR")
```
## Notes
This package is not intended as replacement for proper reference manager packages, 
but a tool to enrich scientific presentation slides.
Hence the functionality is quite limited to this specific use case.

Styling via custom CSS is in early stages and has much room for improvement in the current stage.
