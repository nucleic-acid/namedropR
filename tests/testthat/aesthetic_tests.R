# AESTHETIC OUTPUT CHECKS for manual testing

bib_path <- system.file("testdata", "sample.bib", package = "namedropR")

## check both HTML formats
drop_name(bib_path, export_as = "html", cite_key = "collaboration_2019_ApJL", output_dir = tempdir())
drop_name(bib_path, export_as = "html_full", cite_key = "HAWKING_noDOI", output_dir = tempdir())

## check all preset styles
drop_name(bib_path, export_as = "png", style = "modern", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "modern"))
drop_name(bib_path, export_as = "png", style = "classic", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "classic"))
drop_name(bib_path, export_as = "png", style = "clean", cite_key = "collaboration_2019_ApJL", vc_width = 750, output_dir = here::here(tempdir(), "clean"))
drop_name(bib_path, export_as = "png", style = "fancy", cite_key = "collaboration_2019_ApJL", vc_width = 750, output_dir = here::here(tempdir(), "fancy"))
drop_name(bib_path, export_as = "png", style = "newspaper", cite_key = "Eschrich1983", vc_width = 800, output_dir = here::here(tempdir(), "newspaper"))
drop_name(bib_path, export_as = "png", style = "newspaper", cite_key = "collaboration_2019_ApJL", vc_width = 700, output_dir = here::here(tempdir(), "newspaper"))
drop_name(bib_path, export_as = "png", style = "none", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "none"))
drop_name(bib_path, export_as = "png", style = "invalid", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "invalid_style"))

# check compact style
drop_name(bib_path, export_as = "png", style = "compact", cite_key = "collaboration_2019_ApJL", output_dir = here::here(tempdir(), "compact_style"))
drop_name(bib_path, export_as = "html", style = "compact", cite_key = "Eschrich1983", output_dir = here::here(tempdir(), "compact_style"))
drop_name(bib_path, export_as = "html_full", style = "compact", cite_key = "HAWKING_noDOI", output_dir = here::here(tempdir(), "compact_style"))

# check extreme content lengths
all_styles <- readr::read_csv(system.file("styles", "styles.csv", package = "namedropR"), show_col_types = FALSE)
extreme_bib <- system.file("testdata", "extremes.bib", package = "namedropR")

for (style in all_styles$style_name) {
  print(style)
  drop_name(extreme_bib, export_as = "png", style = style, output_dir = here::here(tempdir(), style))
}

drop_name(extreme_bib, cite_key = "long_author", export_as = "png", style = "fancy", vc_width = 750, output_dir = here::here(tempdir(), "wide"))

