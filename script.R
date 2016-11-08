load('c:/R/data.Rd')
im <- matrix(data=rev(im.train[3,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-d.train$nose_tip_x[3],         96-d.train$nose_tip_y[3],         col="red")
points(96-d.train$left_eye_center_x[3],  96-d.train$left_eye_center_y[3],  col="blue")
points(96-d.train$right_eye_center_x[3], 96-d.train$right_eye_center_y[3], col="green")

for(i in 1:nrow(d.train)) {points(96-d.train$nose_tip_x[i], 96-d.train$nose_tip_y[i], col="red")}

coord      <- "left_eye_center"
patch_size <- 10

coord_x <- paste(coord, "x", sep="_")
coord_y <- paste(coord, "y", sep="_")
patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
    im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
    x   <- d.train[i, coord_x]
    y   <- d.train[i, coord_y]
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
        as.vector(im[x1:x2, y1:y2])
    }
    else
    {
        NULL
    }
}
mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)