# AESTHETIC OUTPUT CHECKS for manual testing

bib_path <- system.file("testdata", "sample.bib", package = "namedropR")

## check both HTML formats
drop_name(bib_path, export_as = "html", cite_key = "collaboration_2019_ApJL", output_dir = tempdir())
drop_name(bib_path, export_as = "html_full", cite_key = "HAWKING_noDOI", output_dir = tempdir())

## check all preset styles
drop_name(bib_path, export_as = "png", style = "modern", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "modern"))
drop_name(bib_path, export_as = "png", style = "classic", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "classic"))
drop_name(bib_path, export_as = "png", style = "clean", cite_key = "collaboration_2019_ApJL", output_dir = here::here(tempdir(), "clean"))
drop_name(bib_path, export_as = "png", style = "none", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "none"))
drop_name(bib_path, export_as = "png", style = "invalid", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "invalid_style"))

# check compact style
drop_name(bib_path, export_as = "png", style = "compact", cite_key = "collaboration_2019_ApJL", output_dir = here::here(tempdir(), "compact_style"))
drop_name(bib_path, export_as = "html", style = "compact", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "compact_style"))
drop_name(bib_path, export_as = "html_full", style = "compact", cite_key = "HAWKING_noDOI", output_dir = here::here(tempdir(), "compact_style"))
