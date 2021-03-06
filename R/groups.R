xml_find_multiple_ids <- function(xml, ids, node = "svg:g", attribute = "id") {
  ids_txt <- paste0(glue("@{attribute} = \'"), ids, "\'", collapse = " or ")
  xpath <- glue(".//{node}[{ids_txt}]")

  xml_find_all(xml, xpath)
}

get_svg <- function(svg) {
  if (is.character(svg)) {
    svg <- read_xml(svg)
  }
  svg
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
  svg <- get_svg(svg)

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
#' @param folder The folder in which the output will be saved
#' @param verbose Verbosity
#' @param export Inkscape argument for exporting
#' @param trim Whether to trim the png
#' @param node What svg to look for, defaults to a group (which can include layers)
#' @param attribute What attribute to look for, defaults to id
#' @param output_format Whether to output the svg, png or pdf
#'
#' @return The location of the png image
#'
#' @export
svg_groups_opacify <- function(
  svg,
  groups,
  output,
  folder = ".",
  verbose = FALSE,
  export = "--export-area-page",
  trim = FALSE,
  node = "svg:g",
  attribute = "id",
  output_format = "png"
) {
  if (!(all(c("opacity", "id") %in% colnames(groups)))) {
    stop("Need opacity")
  }

  if (length(output) == 0) stop("Output needs to be png and/or svg")

  svg <- get_svg(svg)

  walk(groups %>% split(groups$opacity), function(x) {
    nodes <- xml_find_multiple_ids(
      svg,
      x$id,
      node = node,
      attribute = attribute
    )

    if (length(nodes) != nrow(x)) stop("Found ", length(nodes), " matching nodes, but needed ", nrow(x), ": ", x$id)

    xml_attr(nodes, "style") <- glue("opacity:{x$opacity[[1]]};")
  })

  svg_location <- glue("{folder}/{output}.svg")
  xml2::write_xml(svg, svg_location, options = NULL)

  if ("svg" %in% output_format && trim) {
    command <- glue("inkscape --verb=FitCanvasToDrawing --verb=FileSave --verb=FileQuit {svg_location}")
    system(command, ignore.stdout = !verbose)
  }

  if ("png" %in% output_format) {
    png_location <- glue("{folder}/{output}.png")
    system(glue("inkscape {svg_location} --export-area-page --export-png={png_location} --export-dpi=300"), ignore.stdout = !verbose)

    if(trim) {
      magick::image_read(png_location) %>% magick::image_trim() %>% magick::image_write(png_location)
    }
  }

  if (!"svg" %in% output_format) {
    file.remove(svg_location)
  } else if ("png" %in% output) {
    svg_location
  }

  if ("png" %in% output_format) {
    png_location
  }
}

#' Create smaller images from multiple groups in an svg
#'
#' @inheritParams svg_groups_opacify
#' @param group_ids The identifiers of the groups which will all be transparent expect one
#'
#' @export
svg_groups_split <- function(svg, group_ids = svg_groups_list(svg), folder = ".", output_format = "png") {
  groups <- tibble(
    id = group_ids,
    opacity = 1
  )

  map(group_ids, function(group_id) {
    groups <- groups %>%
      mutate(opacity = ifelse(id == !!group_id, 1, 0))

    svg_groups_opacify(
      svg,
      groups,
      group_id,
      trim = TRUE,
      export = "--export-area-drawing",
      folder = folder,
      output_format = output_format
    )
  })
}
