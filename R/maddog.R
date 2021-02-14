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
                       rgb(c(214, 0,   139),
                           c(152, 151, 177),
                           c(41,  158, 78),
                           maxColorValue = 255),
                       rgb(c(214, 183, 0,   139),
                           c(152, 86,  151, 177),
                           c(41,  63,  158, 78),
                           maxColorValue = 255),
                       rgb(c(214, 183, 61,  0,   139),
                           c(152, 86,  100, 151, 177),
                           c(41,  63,  81,  158, 78),
                           maxColorValue = 255),
                       rgb(c(214, 183, 61,  0,   139, 111),
                           c(152, 86,  100, 151, 177, 96),
                           c(41,  63,  81,  158, 78,  96),
                           maxColorValue = 255)),
         south = switch(n-2,
                        rgb(c(181, 0,  78),
                            c(35, 151, 26),
                            c(37, 158, 26),
                            maxColorValue = 255),
                        rgb(c(222, 181, 0,   78),
                            c(216, 35,  151, 26),
                            c(142, 37,  158, 26),
                            maxColorValue = 255),
                        rgb(c(222, 181, 204, 0,   78),
                            c(216, 35,  174, 151, 26),
                            c(142, 37,  106, 158, 26),
                            maxColorValue = 255),
                        rgb(c(222, 181, 204, 136, 0,   78),
                            c(216, 35,  174, 25,  151, 26),
                            c(142, 37,  106, 27,  158, 26),
                            maxColorValue = 255)),
         east = switch(n-2,
                       rgb(c(219, 0,   214),
                           c(53,  151, 152),
                           c(39,  158, 41),
                           maxColorValue = 255),
                       rgb(c(219, 0,   214, 90),
                           c(53,  151, 152, 127),
                           c(39,  158, 41,  57),
                           maxColorValue = 255),
                       rgb(c(219, 0,   139, 214, 90),
                           c(53,  151, 177, 152, 127),
                           c(39,  158, 78,  41,  57),
                           maxColorValue = 255),
                       rgb(c(219, 0,   139, 214, 90,  111),
                           c(53,  151, 177, 152, 127, 33),
                           c(39,  158, 78,  41,  57,  96),
                           maxColorValue = 255)),
         west = switch(n-2,
                       rgb(c(214, 0,   111),
                           c(113, 151, 33),
                           c(47,  158, 96),
                           maxColorValue = 255),
                       rgb(c(214, 0,   90, 111),
                           c(113, 151, 127, 33),
                           c(47,  158, 57,  96),
                           maxColorValue = 255),
                       rgb(c(214, 0,   90,  183, 111),
                           c(113, 151, 127, 86,  33),
                           c(47,  158, 57,  63,  96),
                           maxColorValue = 255),
                       rgb(c(214, 0,   214, 90,  183, 111),
                           c(113, 151, 152, 127, 86,  33),
                           c(47,  158, 41,  57,  63,  96),
                           maxColorValue = 255)),
         rainbow = switch(n-2,
                        rgb(c(0,   214, 181),
                            c(151, 152, 35),
                            c(158, 42,  37),
                            maxColorValue = 255),
                        rgb(c(0,   92,  214, 181),
                            c(151, 174, 152, 35),
                            c(158, 88,  41,  37),
                            maxColorValue = 255),
                        rgb(c(0,   92,  214, 214, 181),
                            c(151, 174, 152, 113, 35),
                            c(158, 88,  41,  47,  37),
                            maxColorValue = 255),
                        rgb(c(111, 0,   92,  214, 214, 181),
                            c(33,  151, 174, 152, 113, 35),
                            c(96,  158, 88,  41,  47,  37),
                            maxColorValue = 255),
                        rgb(c(111, 0,   92,  139, 214, 214, 181),
                            c(33,  151, 174, 177, 152, 113, 35),
                            c(96,  158, 88,  78,  41,  47,  37),
                            maxColorValue = 255),
                        rgb(c(111, 61,  0,   92,  139, 214, 214, 181),
                            c(33,  100, 151, 174, 177, 152, 113, 35),
                            c(96,  81,  158, 88,  78,  41,  47,  37),
                            maxColorValue = 255),
                        rgb(c(111, 61,  0,   92,  139, 214, 214, 181, 136),
                            c(33,  100, 151, 174, 177, 152, 113, 35,  25),
                            c(96,  81,  158, 88,  78,  41,  47,  37,  27),
                            maxColorValue = 255),
                        rgb(c(111, 61,  0,   92,  139, 204, 214, 214, 181, 136),
                            c(33,  100, 151, 174, 177, 174, 152, 113, 35,  25),
                            c(96,  81,  158, 88,  78,  106, 41,  47,  37,  27),
                            maxColorValue = 255)))}

#######################################################################################

# Make flavors
redgrapewine    <- rgb(78,  26,  26,  maxColorValue = 255)
dragonfruit     <- rgb(136, 25,  27,  maxColorValue = 255)
redsangria      <- rgb(181, 35,  37,  maxColorValue = 255)
cranberry       <- rgb(219, 53,  39,  maxColorValue = 255)
tangerinedream  <- rgb(189, 59,  39,  maxColorValue = 255)
rubygrapefruit  <- rgb(181, 35,  37,  maxColorValue = 255)
peachesandcream <- rgb(214, 113, 47,  maxColorValue = 255)
orangejubilee   <- rgb(214, 152, 41,  maxColorValue = 255)
keylimepie      <- rgb(204, 174, 106, maxColorValue = 255)
lightningcreek  <- rgb(222, 216, 142, maxColorValue = 255)
sourapple       <- rgb(139, 177, 78,  maxColorValue = 255)
spikedmelon     <- rgb(92,  174, 88,  maxColorValue = 255)
kiwilemon       <- rgb(90,  127, 57,  maxColorValue = 255)
blueraspberry   <- rgb(0,   151, 158, maxColorValue = 255)
hawaiianblue    <- rgb(61,  100, 81,  maxColorValue = 255)
purplerain      <- rgb(111, 33,  96,  maxColorValue = 255)

# Save flavors
# usethis::use_data(redgrapewine)
# usethis::use_data(dragonfruit)
# usethis::use_data(redsangria)
# usethis::use_data(cranberry)
# usethis::use_data(tangerinedream)
# usethis::use_data(rubygrapefruit)
# usethis::use_data(peachesandcream)
# usethis::use_data(orangejubilee)
# usethis::use_data(keylimepie)
# usethis::use_data(lightningcreek)
# usethis::use_data(sourapple)
# usethis::use_data(spikedmelon)
# usethis::use_data(kiwilemon)
# usethis::use_data(blueraspberry)
# usethis::use_data(hawaiianblue)
# usethis::use_data(purplerain)

#######################################################################################

# Function: displays particular maddog color palette
#maddog.palette <- function(n, name) {
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
                "peachesandcrea", "orangejubilee", "keylimepie",
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

