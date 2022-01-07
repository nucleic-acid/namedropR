# AESTHETIC OUTPUT CHECKS for manual testing

bib_path <- system.file("testdata", "sample.bib", package="namedropR")
test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))

## check both HTML formats
drop_name(test_bibfile, export_as = "html", cite_key = "collaboration_2019_ApJL", output_dir = tempdir())
drop_name(test_bibfile, export_as = "html_full", cite_key = "HAWKING_noDOI", output_dir = tempdir())

## check all preset styles
drop_name(test_bibfile, export_as = "png", style = "modern", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "modern"))
drop_name(test_bibfile, export_as = "png", style = "classic", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "classic"))
drop_name(test_bibfile, export_as = "png", style = "clean", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "clean"))
drop_name(test_bibfile, export_as = "png", style = "none", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "none"))
drop_name(test_bibfile, export_as = "png", style = "invalid", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "invalid_style"))

