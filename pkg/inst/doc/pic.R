# Generate all pictures for the vignettes.
# mvdl 08.07.2011

diamondFile <- "diamond.pdf"
twodiamondFile <- "twodiamond.pdf"

## plot parameters
linewidth = 2
polygoncolor = "#E3E3E3"
textcex = 2

plotPoly <- function(x,y){
    polygon(x,y,lwd=linewidth,col=polygoncolor)
}

###############################################################################
# DIAMOND AREA
###############################################################################

# Allowed area is the diamond
# {(x,y) in R : y <= x+1, y>=-x+3, y>=x-1, y<=-x+5 } 
x <- array(c(
    1,4,
    0,3,
    0,3,
    1,4), dim=c(2,4)
)
y <- array(0,dim=c(2,4))
y[,1] <- x[,1] - 1  # e1
y[,2] <- -x[,2] + 3 # e2
y[,3] <- x[,3] + 1  # e3
y[,4] <- -x[,4] +5  # e4


pdf(diamondFile)
    plot(x[,1],y[,1],
        xlim=c(-1,4.5),
        ylim=c(-1,4.5),
        'l',
        lwd=linewidth,
        xlab="x",
        ylab="y",
        cex.lab=2,
        cex.axis=2,
    )
    
    for ( i in 2:4 ){
        lines(x[,i],y[,i],
            lwd=linewidth
        )
    }
    text(2.7,1.3,expression(e[1]),cex=textcex)
    text(1.3,1.3,expression(e[2]),cex=textcex)
    text(1.3,2.7,expression(e[3]),cex=textcex)
    text(2.7,2.7,expression(e[4]),cex=textcex)
        
    # fill up diamond    
    xPoints <- c(2,3,2,1) 
    yPoints <- c(1,2,3,2)
    plotPoly(xPoints,yPoints)
    
    # record 1
    points(2,-1,pch=19,cex=textcex)
    # record 2
    points(0,0,pch=19,cex=textcex)
    # record 3
    points(-1,2,pch=19,cex=textcex)
    
    arrows(
        x0=c( 2.0,0.2,0.2,-0.8),
        y0=c(-0.8,0.2,0.2, 2.0),
        x1=c( 2.0,0.85,1.6, 0.6),
        y1=c( 0.6,1.65,0.85, 2.0),
        lwd=linewidth,lty=c(1,2,2,1))

    # solution sets
    lines(
        x = c(2,2),
        y = c(1,3))
    lines(
        x = c(1,3),
        y = c(2,2))

    # labels
    text(1.3,-1,"(2,-1)",cex=textcex)
    text(0,-0.4,"(0,0)",cex=textcex)
    text(-1,2.7,"(-1,2)",srt=90,cex=textcex)
dev.off()

###############################################################################
# nonconvex example with two diamonds
###############################################################################


xleft <- c(-2,-3,-2,-1)
yleft <- c( 1, 2, 3, 2)

xright <- c(2,1,2,3)
yright <- c(2,3,4,3)

pdf(twodiamondFile)
    plot(0,0,xlim=c(-3,3),ylim=c(0,4),
        col="white",
        xlab="x",ylab="y",
        cex.lab=textcex,
        cex.axis=textcex)
    #grid()
    plotPoly(xleft,yleft)
    plotPoly(xright,yright)
    
    # record 1
    points(1.5,1.5,pch=19,cex=textcex)
    # record 2
    points(0,2.5,pch=19,cex=textcex)
    # record 2
    points(2,0,pch=19,cex=textcex)
    
    arrows(
        x0 = c( 1.3, 1.5,-0.2, 0.2, 2.0),
        y0 = c( 1.5, 1.7, 2.5, 2.5, 0.2),
        x1 = c(-1.3, 1.5,-1.3, 1.3, 2.0),
        y1 = c( 1.5, 2.3, 2.5, 2.5, 1.8),
        lwd=linewidth
    )
    
    # solution sets
    lines(
        x=c(-1.5,-2.5),
        y=c(2.5,2.5))
    lines(
        x=c(1.5,1.5),
        y=c(2.5,3.5))
    lines(
        x=c(1.5,2.5),
        y=c(2.5,2.5))
    lines(
        x=c(-1.5,-2.5),
        y=c(1.5,1.5))
    lines(
        x=c(2,2),
        y=c(2,4))
    
    # labels
    text(0.9,1.2,"(3/2,3/2)", cex=textcex)
    text(0.0,2.2,"(0,5/2)", cex=textcex)
    text(1.4,0.0,"(2,0)", cex=textcex)
dev.off()










