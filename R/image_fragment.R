#' Add a Graded Fragmentation Effect to a Magick Image
#'
#' This function creates a set number of copies of a magick image, with an increasing proportion of random non-background "fragments" of the image obscured by coloured squares.
#' @param image a magick image object
#' @param pixel_size a double representing the height and width of each image fragment
#' @param levels the number of fragmentation levels to generate, inclusive of the fully revealed image
#' @param pixel_colour a valid magick colour string such as "navyblue" or "#000000" that determines the fill colour of the obscured image fragments
#' @param background_detect a logical value. When TRUE, image_fragment will discount white background segments when randomly assigning parts of the image to obscure.
#' @keywords fragmentation
#' @export
#' @examples
#' library(magick)
#' stimulus <- image_read_svg("http://jeroen.github.io/images/tiger.svg", width = 400)
#'
#' fragments <- image_fragment(stimulus, pixel_size = 20, levels = 10)

image_fragment <- function(image, pixel_size, levels, pixel_colour = "white", background_detect = TRUE) {

  # Check if input image is of class magick image
  stopifnot("Input image is not a magick image" = class(image) == "magick-image")

  bg_h <- magick::image_info(image)$height
  bg_w <- magick::image_info(image)$width

  # Create square based on width and colour arguments
  square <- magick::image_blank(pixel_size, pixel_size, color = pixel_colour)

  # Add check to make sure that the image dimensions are evenly divisible by pixel size
  stopifnot("Pixel size not evenly divisible by image dimensions" = bg_h %% pixel_size == 0, bg_w %% pixel_size == 0)

  # Get whole grid of widths and heights
  gridcombos <- tibble::tibble(
    x = unlist(lapply(seq(0, bg_w-pixel_size, by = pixel_size), \(x) rep(x, bg_h / pixel_size))),
    y = rep(seq(0, bg_h - pixel_size, by = pixel_size), length(unique(x)))
  )

  # Crop image into 20x20 squares and check for empty parts
  image_sections <- mapply(\(x, y) magick::image_crop(image, geometry = paste0(pixel_size, "x", pixel_size, "+", x, "+", y)),
                           gridcombos$x, gridcombos$y)

  if (background_detect) {
    filter_rows <- sapply(image_sections, \(x) any(as.vector(magick::image_data(x)) != "ff"))
  } else {
    filter_rows <- rep(T, nrow(image_sections))
  }

  # Filter out empty squares, shuffle order of remaining ones
  img_squares <- dplyr::slice(gridcombos[filter_rows,], sample(1:(dplyr::n())))

  # Work out number of squares to present at each frag level
  prop_vis <- rev(0.75^(levels - 1:levels))
  n_vis <- ceiling(nrow(img_squares) * prop_vis)
  n_vis_diffs <- n_vis[1:(levels-1)] - n_vis[2:levels]

  if(any(n_vis_diffs == 0)) warning("Multiple image copies are equally fragmented. Try reducing the number of fragmentation levels")

  # Denote which frag level each circle will be visible in
  vis_squares <- img_squares
  vis_squares$vis_level <- c(rep(0, nrow(img_squares)-sum(n_vis_diffs)),
                             unlist(mapply(\(x, y) rep(x, y), 1:length(n_vis_diffs), n_vis_diffs)))

  # Filter image sections with vis level 0 (initially visible at highest fragmentation level)
  # and then split by group into separate list items
  fragments <-  subset(vis_squares, vis_level != 0) |>
    dplyr::group_split(vis_level)

  tmp_image <- image
  fragmented_images <- list(image)

  # For each fragmentation level and each image fragment, draw in the fragments
  for (i in 1:length(fragments)) {
    for (j in 1:nrow(fragments[[i]])) {
      tmp_image <- magick::image_composite(tmp_image,
                                           square,
                                           offset = paste0("+", fragments[[i]]$x[j], "+", fragments[[i]]$y[j]))
    }

    fragmented_images[[i+1]] <- tmp_image
  }

  return(rev(fragmented_images))
}
