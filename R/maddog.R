#######################################################################################

set.list        <- c("north", "south", "east", "west")
names(set.list) <- set.list
seq.list        <- c("rainbow")
names(seq.list) <- seq.list

set.num         <- rep(6, length(set.list))
names(set.num)  <- set.list
seq.num         <- rep(10, length(seq.list))
names(seq.num)  <- seq.list

list.name       <- c(set.list, seq.list)
col.max         <- c(set.num, seq.num)
list.cat        <- rep(c("div", "seq"),
                      c(length(set.list), length(seq.list)))

maddog.info     <- data.frame(col.max   = col.max,
                              category  = list.cat,
                              row.names = list.name)

#######################################################################################

# Function: returns a maddog color palette (e.g., scale_fill_manual(values = maddog(n, "name")))
maddog <- function(name, n) {
  if(!(name %in% list.name)) {
    stop(paste(name, "is not a valid palette name for maddog\n"))}

  if(n < 3) {
    warning(paste("Minimal value for n in palette",name,"is 3. Returning palette with 3 different levels\n"))
    return(maddog(name, 3))}

  if(n > col.max[which(name == list.name)]) {
    warning(paste("Maximum value for n in palette",name,"is",col.max[which(name == list.name)]),". Returning palette with maximum levels\n")
    return(name, maddog(col.max[which(name == list.name)]))}
  switch(name,
        north = switch(n-2,
                       rgb(c(205, 65,  98),
                           c(154, 149, 125),
                           c(66,  156, 66),
                           maxColorValue = 255),
                       rgb(c(205, 65,  171, 98),
                           c(154, 149, 91,  125),
                           c(66,  156, 70,  66),
                           maxColorValue = 255),
                       rgb(c(205, 65,  171, 147, 98),
                           c(154, 149, 91,  176, 125),
                           c(66,  156, 70,  92,  66),
                           maxColorValue = 255),
                       rgb(c(205, 65,  171, 147, 98,  102),
                           c(154, 149, 91,  176, 125, 40),
                           c(66,  156, 70,  92,  66,  93),
                           maxColorValue = 255)),
        south = switch(n-2,
                       rgb(c(167, 205, 72),
                           c(49,  154, 29),
                           c(45,  66,  28),
                           maxColorValue = 255),
                       rgb(c(167, 205, 171, 72),
                           c(49,  154, 91,  29),
                           c(45,  66,  70,  28),
                           maxColorValue = 255),
                       rgb(c(167, 205, 171, 221, 72),
                           c(49,  154, 91,  215, 29),
                           c(45,  66,  70,  151, 28),
                           maxColorValue = 255),
                       rgb(c(167, 205, 171, 221, 72, 200),
                           c(49,  154, 91,  215, 29, 174),
                           c(45,  66,  70,  151, 28, 115),
                           maxColorValue = 255)),
        east = switch(n-2,
                       rgb(c(202, 65,  70),
                           c(68,  149, 99),
                           c(51,  156, 83),
                           maxColorValue = 255),
                       rgb(c(202, 65,  221, 70),
                           c(68,  149, 215, 99),
                           c(51,  156, 151, 83),
                           maxColorValue = 255),
                       rgb(c(202, 65,  221, 171, 70),
                           c(68,  149, 215, 91,  99),
                           c(51,  156, 151, 70,  83),
                           maxColorValue = 255),
                       rgb(c(202, 65,  221, 171, 70, 102),
                           c(68,  149, 215, 91,  99, 40),
                           c(51,  156, 151, 70,  83, 93),
                           maxColorValue = 255)),
        west = switch(n-2,
                       rgb(c(205, 65,  125),
                           c(154, 149, 36),
                           c(66,  156, 33),
                           maxColorValue = 255),
                       rgb(c(205, 65,   171, 125),
                           c(154, 149, 91,  36),
                           c(66,  156, 70,  33),
                           maxColorValue = 255),
                       rgb(c(205, 65,   171, 111, 125),
                           c(154, 149, 91,  33,  36),
                           c(66,  156, 70,  96,  33),
                           maxColorValue = 255),
                       rgb(c(205, 65,   171, 111, 125, 70),
                           c(154, 149, 91,  33,  36,  99),
                           c(66,  156, 70,  96,  33,  83),
                           maxColorValue = 255)),
        rainbow = switch(n-2,
                        rgb(c(65,  214, 183),
                            c(149, 152, 86),
                            c(156, 41,  63),
                            maxColorValue = 255),
                        rgb(c(65,  112, 214, 183),
                            c(149, 171, 152, 86),
                            c(156, 98,  41,  63),
                            maxColorValue = 255),
                        rgb(c(102, 65,  112, 214, 183),
                            c(40,  149, 171, 152, 86),
                            c(93,  185, 98,  41,  63),
                            maxColorValue = 255),
                        rgb(c(102, 65,  112, 214, 201, 183),
                            c(40,  149, 171, 152, 118, 86),
                            c(93,  156, 98,  41,  63,  63),
                            maxColorValue = 255),
                        rgb(c(102, 65,  112, 147, 214, 201, 183),
                            c(40,  149, 171, 176, 152, 118, 86),
                            c(93,  156, 98,  92,  41,  63,  63),
                            maxColorValue = 255),
                        rgb(c(102, 70, 65,  112, 147, 214, 201, 183),
                            c(40,  99, 149, 171, 176, 152, 118, 86),
                            c(93,  83, 156, 98,  92,  41,  63,  63),
                            maxColorValue = 255),
                        rgb(c(102, 70,  65,  112, 147, 214, 201, 183, 125),
                            c(40,  99, 149,  171, 176, 152, 118, 86,  36),
                            c(93,  83,  156, 98,  92,  41,  63,  63,  33),
                            maxColorValue = 255),
                        rgb(c(102, 70,  65, 112, 147, 200, 214, 201, 183, 125),
                            c(40,  99, 149, 171, 176, 171, 152, 118, 86,  36),
                            c(93,  83, 156, 98,  92,  115, 41,  63,  63,  33),
                            maxColorValue = 255)))}

#######################################################################################

# Make flavors
redgrapewine    <- rgb(72,  29,  28,  maxColorValue = 255)
dragonfruit     <- rgb(125, 36,  33,  maxColorValue = 255)
redsangria      <- rgb(167, 49,  45,  maxColorValue = 255)
cranberry       <- rgb(202, 68,  51,  maxColorValue = 255)
tangerinedream  <- rgb(175, 69,  48,  maxColorValue = 255)
rubygrapefruit  <- rgb(171, 91,  70,  maxColorValue = 255)
peachesandcream <- rgb(201, 118, 63,  maxColorValue = 255)
orangejubilee   <- rgb(205, 154, 66,  maxColorValue = 255)
keylimepie      <- rgb(200, 174, 115, maxColorValue = 255)
lightningcreek  <- rgb(221, 215, 151, maxColorValue = 255)
sourapple       <- rgb(147, 176, 92,  maxColorValue = 255)
spikedmelon     <- rgb(112, 171, 98,  maxColorValue = 255)
kiwilemon       <- rgb(98,  125, 66,  maxColorValue = 255)
blueraspberry   <- rgb(65,  149, 156, maxColorValue = 255)
hawaiianblue    <- rgb(70,  99, 83,  maxColorValue = 255)
purplerain      <- rgb(102, 40,  93,  maxColorValue = 255)

#purplerain      <- rgb(102, 40,  93,  maxColorValue = 255)
#hawaiianblue    <- rgb(70,  99, 83,  maxColorValue = 255)
#blueraspberry   <- rgb(65,  149, 156, maxColorValue = 255)
#spikedmelon     <- rgb(112, 171, 98,  maxColorValue = 255)
#sourapple       <- rgb(147, 176, 92,  maxColorValue = 255)
#keylimepie      <- rgb(200, 174, 115, maxColorValue = 255)
#orangejubilee   <- rgb(205, 154, 66,  maxColorValue = 255)
#peachesandcream <- rgb(201, 118, 63,  maxColorValue = 255)
#redsangria      <- rgb(167, 49,  45,  maxColorValue = 255)
#dragonfruit     <- rgb(125, 36,  33,  maxColorValue = 255)

#orangejubilee   <- rgb(205, 154, 66,  maxColorValue = 255)
#blueraspberry   <- rgb(65,  149, 156, maxColorValue = 255)
#rubygrapefruit  <- rgb(171, 91,  70,  maxColorValue = 255)
#sourapple       <- rgb(147, 176, 92,  maxColorValue = 255)
#kiwilemon       <- rgb(98,  125, 66,  maxColorValue = 255)
#purplerain      <- rgb(102, 40,  93,  maxColorValue = 255)

#redsangria      <- rgb(167, 49,  45,  maxColorValue = 255)
#orangejubilee   <- rgb(205, 154, 66,  maxColorValue = 255)
#rubygrapefruit  <- rgb(171, 91,  70,  maxColorValue = 255)
#lightningcreek  <- rgb(221, 215, 151, maxColorValue = 255)
#redgrapewine    <- rgb(72,  29,  28,  maxColorValue = 255)
#keylimepie      <- rgb(200, 174, 115, maxColorValue = 255)

#cranberry       <- rgb(202, 68,  51,  maxColorValue = 255)
#blueraspberry   <- rgb(65,  149, 156, maxColorValue = 255)
#lightningcreek  <- rgb(221, 215, 151, maxColorValue = 255)
#rubygrapefruit  <- rgb(171, 91,  70,  maxColorValue = 255)
#hawaiianblue    <- rgb(70,  99, 83,  maxColorValue = 255)
#purplerain      <- rgb(102, 40,  93,  maxColorValue = 255)

#orangejubilee   <- rgb(205, 154, 66,  maxColorValue = 255)
#blueraspberry   <- rgb(65,  149, 156, maxColorValue = 255)
#rubygrapefruit  <- rgb(171, 91,  70,  maxColorValue = 255)
#purplerain      <- rgb(102, 40,  93,  maxColorValue = 255)
#dragonfruit     <- rgb(125, 36,  33,  maxColorValue = 255)
#hawaiianblue    <- rgb(70,  99, 83,  maxColorValue = 255)

# Save flavors
#usethis::use_data(redgrapewine, overwrite = TRUE)
#usethis::use_data(dragonfruit, overwrite = TRUE)
#usethis::use_data(redsangria, overwrite = TRUE)
#usethis::use_data(cranberry, overwrite = TRUE)
#usethis::use_data(tangerinedream, overwrite = TRUE)
#usethis::use_data(rubygrapefruit, overwrite = TRUE)
#usethis::use_data(peachesandcream, overwrite = TRUE)
#usethis::use_data(orangejubilee, overwrite = TRUE)
#usethis::use_data(keylimepie, overwrite = TRUE)
#usethis::use_data(lightningcreek, overwrite = TRUE)
#usethis::use_data(sourapple, overwrite = TRUE)
#usethis::use_data(spikedmelon, overwrite = TRUE)
#usethis::use_data(kiwilemon, overwrite = TRUE)
#usethis::use_data(blueraspberry, overwrite = TRUE)
#usethis::use_data(hawaiianblue, overwrite = TRUE)
#usethis::use_data(purplerain, overwrite = TRUE)

#######################################################################################

# Function: displays specific maddog color palette
#maddog_palette <- function(n, name) {
#  if(!(name %in% list.name)) {
#    stop(paste(name,"is not a valid palette name for maddog\n"))}
#
#  if(n < 3) {
#    warning(paste("Minimal value for n in palette",name,"is 3. Returning palette with 3 different levels\n"))
#    return(display.maddog(3,name))}
#
#  if(n > col.max[which(name == list.name)]) {
#    warning(paste("Maximum value for n in palette",name,"is",col.max[which(name == list.name)]),". Returning palette with maximum levels\n")
#    return(maddog(col.max[which(name == list.name)], name))}
#
#  if(length(which(name == set.list))>0) palattr <- "(divergent)"
#  if(length(which(name == seq.list))>0) palattr <- "(sequential)"
#  image(x = 1:n, y = 1, z = as.matrix(1:n), col = maddog(n, name),
#        xlab = paste(name, palattr), ylab = "", xaxt = "n", yaxt = "n", bty = "n")}

# Function: displays all maddog flavors
maddog_flavors <- function(n = NULL, type = "all", select = NULL, exact.n = TRUE) {
  flav.list = c("redgrapewine", "dragonfruit", "redsangria",
                "cranberry", "tangerinedream", "rubygrapefruit",
                "peachesandcream", "orangejubilee", "keylimepie",
                "lightningcreek", "sourapple", "spikedmelon",
                "kiwilemon", "blueraspberry", "hawaiianblue",
                "purplerain")
  return(flav.list)}

#######################################################################################

# Function: displays all maddog color palettes
maddog_palettes <- function(n = NULL, type = "all", select = NULL, exact.n = TRUE) {
  gap.list          <- ""
  total.list        <- c(set.list, gap.list, seq.list)
  names(total.list) <- c(names(set.list), "gap1", names(seq.list))
  gap.num           <- max(c(set.num, seq.num))
  tot.num           <- c(set.num, gap.num, seq.num)
  names(tot.num)    <- names(total.list)

  if (!(type %in% c("div", "seq", "all"))) {
    stop(paste(type, "is not a valid name for a color list\n"))}
  color.list <- switch(type, div = set.list, seq = seq.list, all = total.list)
  max.num    <- switch(type, div = set.num,  seq = seq.num,  all = tot.num)

  if(!is.null(select)){color.list <- color.list[select]
  max.num <- max.num[select]

  if(any(is.na(color.list)))
    stop(paste("Invalid value(s) of select: ",
               paste(select[is.na(color.list)],
                     collapse=" ")))}

  palattr <- switch(type, div = "divergent", seq = "sequential", all = "divergent+sequential")

  if(is.null(n))n <- max.num

  if(length(n) == 1)n <- rep(n, length(color.list))

  if(exact.n){
    keep      <- n <= max.num
    color.list <- color.list[keep]
    n         <- n[keep]
    max.num    <- max.num[keep]}

  if (any(n < 3) | exact.n & any(n > max.num)|
      length(n) != length(color.list)){
    warning("Illegal vector of color numbers")
    print(paste(n, collapse=" "))}

  n[n < 3]      <- 3
  n[n > max.num] <- max.num[n > max.num]

  nr <- length(color.list)
  nc <- max(n)

  ylim   <- c(0, nr)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, nc), ylim = ylim,
       type = "n", axes = FALSE, bty = "n",
       xlab = "", ylab = "")

  for(i in 1:nr)
  {nj <- n[i]
  if (color.list[i] == "") next
  shade <- maddog(color.list[i], nj)
  rect(xleft = 0:(nj - 1), ybottom = i-1, xright = 1:nj, ytop = i - 0.2, col = shade,
       border = "light grey")}
  text(rep(-0.1, nr), (1:nr) - 0.6, labels = color.list, xpd = TRUE, adj = 1, cex = 0.75)}

#######################################################################################

