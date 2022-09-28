# namedropR (development version)

# namedropR 2.4.1

* Solves issue of failing tests in CRAN checks.

# namedropR 2.4.0

* Adds drop_name_crossref() function (thanks to Lukas Wallrich)
* Adds the ability to turn a QR code into a hyperlink (thanks to Matt Warkentin)
* Rebuilds documentation using latest roxygen2 version to adhere to HTML5 standards

# namedropR 2.3.3

* Fixes critical error previously missed by unit tests when working with file paths. This broke the basic functionality on some systems when working outside of projects.
* Removes dependency on the {here} package

# namedropR 2.3.2

* Fixes failing test due to tempdir access problems on win devel.
* Changes git install snippet in README.md to use {remotes} instead of {devtools}

# namedropR 2.3.1

* Adds documentation / vignette for new styling options
* changes lazy data loading to false (no relevant datasets are included)
* tested with R 4.2

namedropR 2.3.0

* Adds option to specify QR code color.
* Adds several options to adjust stylistic elements like font, color, size and weight for title, journal and authors.
* Adjusts predefined styles to allow for custom style editing.

# namedropR 2.2.3

* Fixes inconsistent documentation and presentation in the Readme.
* Fixes style options that resulted in sub-optimal readability of the rendered citations.
* Adds sample render images for the predefined styles in the readme.
* Slight adjustment to the DESCRIPTION title.

# namedropR 2.2.2

* Adds a {pkgdown} website to the repository to make the documentation more accessible.
* Adds a separate vignette for use in Rmarkdown environments.

# namedropR 2.2.1

* Allows users to also specify the width of the visual citation, as larger QR codes compressed the remaining text area.
* Adds two new styles to chose from.

# namedropR 2.2.0

* Styles are now stored in a separate csv. This allows for easier addition of styles.
* Allows user specification of QR code size.

# namedropR 2.1.1

* Removes examples from not-exported functions to fulfill CRAN reviewer comment
* Adds and improves test coverage for additional input situations.
* Minor style adjustments for compact and clean style.

# namedropR 2.1.0

* Implements 'compact' output style. See update vignette for usage.

# namedropR 2.0.2

* Slight adjustment to the 'modern' style (journal titles not capitalized anymore)
* By default curly braces are removed from titles and journal names. This can be prevented by setting clean_strings to FALSE

# namedropR 2.0.1

* Minor fixes to prepare pull request to main
* Style fixes of code in preparation for release

# namedropR 2.0.0

* Major code changes to eliminate dependency on the orphaned {bibtex} package to allow for CRAN submission.
* On the down side the package cannot work with BibEntry objects anymore, so the update might break existing user-code. Due to this the version number was increased as a 'major release'. 
* As new function, bulk handling is now available.
* The update furthermore improves overall stability and test coverage.

# namedropR 1.0.2

* Minor adjustments to unit tests to avoid build errors

# namedropR 1.0.1

* Minor adjustments to unit tests to avoid build errors

# namedropR 1.0.0

* Feature complete first version for CRAN submission.
* Improves unit testing.

# namedropR 0.10.1

* Corrects a typo in the example in the README.

# namedropR 0.10.0

* Added a vignette and proper README

# namedropR 0.9.4.9000

* Added a `NEWS.md` file to track changes to the package.
