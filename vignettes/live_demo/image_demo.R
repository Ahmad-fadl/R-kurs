library(imager)

plot_rgb_matrix <- function (data, w ,h) {
  n <- ncol(data)
  x <- integer(n)
  y <- integer(n)
  col <- c()
  for (i in 0:(h-1)) {
    for (j in 0:(w-1)) {
      idx <- i * w + j + 1
      y[idx] <- i
      x[idx] <- j
      cur <- data[, idx]
      col <- c(col, rgb(cur[1], cur[2], cur[3], 1))
    }
  }
  plot(x,y,pch=15,col=col, ann=FALSE, xaxt='n', yaxt='n')
}

plot_clustered_rgb_vectors <- function(data, w, h, cl=1, bgCol=rgb(1,1,1,1)) {
  cluster <- attr(data, "cluster")
  cl_indices <- seq(ncol(data))[cluster == cl]
  cl_indices
  n <- ncol(data)
  x <- integer(n)
  y <- integer(n)
  col <- c()
  for (i in 0:(h-1)) {
    for (j in 0:(w-1)) {
      idx <- i * w + j + 1
      y[idx] <- i
      x[idx] <- j
      if (idx %in% cl_indices) {
        cur <- data[, idx]
        col <- c(col, rgb(cur[1], cur[2], cur[3], 1))
      } else {
        col <- c(col, bgCol)
      }

    }
  }
  plot(x,y,pch=15,col=col, ann=FALSE, xaxt='n', yaxt='n')
}

convert_imager_to_matrix <- function (image) {
  w <- imager::width(image)
  h <- imager::height(image)
  v <- c()
  for (i in seq(h)) {
    for (j in seq(w)) {
      v <- c(v, imager::color.at(image, i, j));
    }
  }

  result <- matrix(v, nrow=3);

  return(result);
}

original <- imager::load.image(paste(getwd(),"/vignettes/live_demo/sample_image.jpg", sep = ""))
cropped <- imager::crop.borders(original, 60, 60)

w <- imager::width(cropped)
h <- imager::height(cropped)

m <- convert_imager_to_matrix(cropped)

par(mfrow=c(3,2), mar=c(3,3,2,1))
plot_rgb_matrix(m,w,h)

cl_m <- clustering::kmean(m, k=4)

plot_clustered_rgb_vectors(cl_m, w, h, 1)
plot_clustered_rgb_vectors(cl_m, w, h, 2)
plot_clustered_rgb_vectors(cl_m, w, h, 3)
plot_clustered_rgb_vectors(cl_m, w, h, 4)

