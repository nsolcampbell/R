#install.packages('RCurl')
library(RCurl)
install.packages('jpeg')
library(jpeg)
install.packages('png')
library(png)

street_fighter <- function() {
        url <- list(
                bg = "http://front-back.com/wp-content/uploads/2013/01/sf2-bg.jpg",
                ken = "http://front-back.com/wp-content/uploads/2013/01/ken.png"
                )

        bg <- readJPEG(getURLContent(url$bg))
        ken <- readPNG(getURLContent(url$ken))

        block_w <- 70
        block_h <- 80
        draw_w <- 140
        draw_h <- 160
        bg_w <- ncol(bg)
        bg_h <- nrow(bg)
        bg_margin <- (bg_w - bg_h) / 2

        fighter_x <- 200
        fighter_y <- 30
        step_size <- 35

        init_bg <- function() {
                plot(c(1, bg_w), c(-bg_margin, bg_h + bg_margin), type = "n", axes = FALSE, xlab = "", ylab = "")
                rasterImage(bg, 1, 1, bg_w, bg_h)
        }
        recover_bg <- function(x, y, w, h) {
                rasterImage(bg[bg_h - (h:1 + y) + 1, 1:w + x, ], x + 1, y + 1, x + w, y + h)
        }
        draw_block <- function(sx, sy, sw, sh, x, y, w, h) {
                rasterImage(ken[1:sh + sy, 1:sw + sx, ], x + 1, y + 1, x + w, y + h)
        }

        init_bg()
        repeat {
                for (step in 0:15) {
                        if (step %% 4 == 0) {
                                recover_bg(fighter_x, fighter_y, draw_w, draw_h)
                                draw_block(block_w * ((floor(step / 4) + 2) %% 4), 0, block_w, block_h, fighter_x, fighter_y, draw_w, draw_h)
                        }

                        draw_block(block_w * (step %% 2), 320, block_w, block_h, fighter_x + draw_w + step * step_size, fighter_y, draw_w, draw_h)
                        Sys.sleep(0.15)
                        recover_bg(fighter_x + draw_w + step * step_size, fighter_y, draw_w, draw_h)
                }
        }
}
street_fighter()
