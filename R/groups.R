xml_find_multiple_ids <- function(xml, ids) {
  ids_txt <- paste0(glue::glue("@id = \'"), ids, "\'", collapse = " or ")
  xpath <- glue::glue(".//svg:g[{ids_txt}]")

  xml_find_all(xml, xpath)
}

#' List the groups in an svg
#'
#' @param svg The svg to use
#' @param patterns One or more regular expression patterns, used for consecutive filtering
#'
#' @export
svg_groups_list <- function(
  svg,
  patterns = c("^(?!g[0-9]*).*$", "^(?!layer[0-9]*).*$")
) {
  group_ids <- svg %>% xml_find_all(".//svg:g") %>% xml_attr("id") %>% as.character()

  for(pattern in patterns) {
    group_ids <- group_ids %>% str_subset(pattern)
  }

  group_ids
}

#' Change the opacity of svg groups
#'
#' @param svg The svg
#' @param groups Tibble containing an id column (the id of the group) and an opacity column (double between 0 and 1)
#' @param output The output name, will be appended with .svg and/or .png
#' @param verbose Verbosity
#' @param export Inkscape argument for exporting
#' @param trim Whether to trim the png
#' @param folder The folder in which the output will be saved
#'
#' @return The location of the png image
#'
#' @export
svg_groups_opacify <- function(
  svg,
  groups,
  output,
  verbose = FALSE,
  export = "--export-area-page",
  trim = FALSE,
  folder = tempdir()
) {
  if (!(all(c("opacity", "id") %in% colnames(groups)))) {
    stop("Need opacity")
  }

  if (is.character(svg)) {
    svg <- read_xml(svg)
  }

  walk(groups %>% split(groups$opacity), function(x) {
    nodes <- xml_find_multiple_ids(
      svg,
      x$id
    )
    xml_attr(nodes, "style") <- glue::glue("opacity:{x$opacity[[1]]};")
  })

  svg_location <- glue::glue("{folder}/{output}.svg")
  png_location <- glue::glue("{folder}/{output}.png")

  write(as.character(svg), file=svg_location)
  system(glue::glue("inkscape {svg_location} --export-area-page --export-png={png_location} --export-dpi=300"), ignore.stdout = !verbose)

  file.remove(svg_location)

  if(trim) {
    magick::image_trim(magick::image_read(png_location))
  }

  png_location
}

#' Create smaller images from multiple groups in an svg
#'
#' @inheritParams svg_groups_opacify
#' @param group_ids The identifiers of the groups which will all be transparent expect one
#'
#' @export
svg_groups_split <- function(svg, group_ids, folder) {
  groups <- tibble(
    id = group_ids,
    opacity = 1
  )

  map(group_ids, function(group_id) {
    groups <- groups %>%
      mutate(
        opacity = ifelse(group_id == !!group_id, 1, 0)
      )

    opacify_svg_groups(
      svg,
      groups,
      group_id,
      trim = TRUE,
      export = "--export-area-drawing",
      folder = folder
    )
  })
}
