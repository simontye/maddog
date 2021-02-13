#######################################################################################

divlist        <- c("set1", "set2", "set3", "set4")
names(divlist) <- divlist
seqlist        <- c("rainbow")
names(seqlist) <- seqlist

divnum         <- rep(6, length(divlist))
names(divnum)  <- divlist
seqnum         <- rep(10, length(seqlist))
names(seqnum)  <- seqlist

namelist       <- c(divlist, seqlist)
maxcolors      <- c(divnum, seqnum)
catlist        <- rep(c("div", "seq"),
                      c(length(divlist), length(seqlist)))

maddog.info    <- data.frame(maxcolors = maxcolors,
                             category  = catlist,
                             row.names = namelist)

#######################################################################################

# Function: returns a maddog color palette (e.g., scale_fill_manual(values = maddog(n, "name")))
maddog <- function(n, name) {
  if(!(name %in% namelist)) {
    stop(paste(name, "is not a valid palette name for maddog\n"))}

  if(n < 3) {
    warning(paste("Minimal value for n in palette",name,"is 3. Returning palette with 3 different levels\n"))
    return(maddog(3, name))}

  if(n > maxcolors[which(name == namelist)]) {
    warning(paste("Maximum value for n in palette",name,"is",maxcolors[which(name == namelist)]),". Returning palette with maximum levels\n")
    return(maddog(maxcolors[which(name == namelist)], name))}
  switch(name,
         set1 = switch(n-2,
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
         set2 = switch(n-2,
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
         set3 = switch(n-2,
                       rgb(c(214,0,111),
                           c(113,151,33),
                           c(47,158,96),maxColorValue=255),
                       rgb(c(214,0,90,111),
                           c(113,151,127,33),
                           c(47,158,57,96),maxColorValue=255),
                       rgb(c(214,0,90,183,111),
                           c(113,151,127,86,33),
                           c(47,158,57,63,96),maxColorValue=255),
                       rgb(c(214,0,214,90,183,111),
                           c(113,151,152,127,86,33),
                           c(47,158,41,57,63,96),maxColorValue=255)),
         set4 = switch(n-2,
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

# Function: displays particular maddog color palette
display.maddog <- function(n, name) {
  if(!(name %in% namelist)) {
    stop(paste(name,"is not a valid palette name for maddog\n"))}

  if(n < 3) {
    warning(paste("Minimal value for n in palette",name,"is 3. Returning palette with 3 different levels\n"))
    return(display.maddog(3,name))}

  if(n > maxcolors[which(name == namelist)]) {
    warning(paste("Maximum value for n in palette",name,"is",maxcolors[which(name == namelist)]),". Returning palette with maximum levels\n")
    return(maddog(maxcolors[which(name == namelist)], name))}

  if(length(which(name == divlist))>0) palattr <- "(divergent)"
  if(length(which(name == seqlist))>0) palattr <- "(sequential)"
  image(x = 1:n, y = 1, z = as.matrix(1:n), col = maddog(n, name),
        xlab = paste(name, palattr), ylab = "", xaxt = "n", yaxt = "n", bty = "n")}

#######################################################################################

# Function: displays all maddog color palettes
display.maddog.all <- function (n = NULL, type = "all", select = NULL, exact.n = TRUE) {
  gaplist          <- ""
  totallist        <- c(divlist, gaplist, seqlist)
  names(totallist) <- c(names(divlist), "gap1", names(seqlist))
  gapnum           <- max(c(divnum, seqnum))
  totnum           <- c(divnum, gapnum, seqnum)
  names(totnum)    <- names(totallist)

  if (!(type %in% c("div", "seq", "all"))) {
    stop(paste(type, "is not a valid name for a color list\n"))}
  colorlist <- switch(type, div = divlist, seq = seqlist, all = totallist)
  maxnum    <- switch(type, div = divnum, seq = seqnum, all = totnum)

  if(!is.null(select)){colorlist <- colorlist[select]
  maxnum <- maxnum[select]

  if(any(is.na(colorlist)))
    stop(paste("Invalid value(s) of select: ",
               paste(select[is.na(colorlist)],
                     collapse=" ")))}

  palattr <- switch(type, div = "divergent", seq = "sequential", all = "divergent+sequential")

  if(is.null(n))n <- maxnum

  if(length(n) == 1)n <- rep(n, length(colorlist))

  if(exact.n){
    keep      <- n <= maxnum
    colorlist <- colorlist[keep]
    n         <- n[keep]
    maxnum    <- maxnum[keep]}

  if (any(n < 3) | exact.n & any(n > maxnum)|
      length(n) != length(colorlist)){
    warning("Illegal vector of color numbers")
    print(paste(n, collapse=" "))}

  n[n < 3]      <- 3
  n[n > maxnum] <- maxnum[n > maxnum]

  nr <- length(colorlist)
  nc <- max(n)

  ylim   <- c(0, nr)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, nc), ylim = ylim,
       type = "n", axes = FALSE, bty = "n",
       xlab = "", ylab = "")

  for(i in 1:nr)
  {nj <- n[i]
  if (colorlist[i] == "") next
  shadi <- maddog(nj, colorlist[i])
  rect(xleft=0:(nj-1), ybottom = i-1, xright = 1:nj, ytop = i-0.2, col = shadi,
       border = "light grey")}
  text(rep(-0.1, nr), (1:nr)-0.6, labels = colorlist, xpd = TRUE, adj = 1)}
#######################################################################################

