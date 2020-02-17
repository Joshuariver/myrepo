# Complex coloring and contour levels
# https://www.r-bloggers.com/complex-coloring-and-contour-levels/

rm(list=ls())

fz <- function(z) z^2-0.6/z
fz6 <-function(z) fz(fz(fz(fz(fz(fz(z))))))
f6 <- function(x,y) fz6(x+1i*y)
x <- seq(-0.85,1.35,len=1001)
y <- seq(-1.4,1.4,len=1001)
m <- outer(x,y,f6) # this is a matrix of complex numbers
contour(x,y,Re(m),levels=c(0),drawlabels=F,axes=F) 
# Re(m) takes the real parts

cl <- contourLines(x,y,Re(m),levels=0)
pal <- topo.colors(48)
contour(x,y,Re(m),levels=c(0),drawlabels=F,axes=F)
for(i in 1:length(cl)) polygon(cl[[i]]$x,cl[[i]]$y,col=pal[i %% 12+1])

fz <- function(z) z^3+(-0.2+0.11*1i)/z^3
fz3 <-function(z) fz(fz(fz(z)))
f3 <- function(x,y) fz3(x+1i*y)
x <- seq(-1.08,1.08,len=1001)
y <- seq(-1.3,1.3,len=1001)
m <- outer(x,y,f3) # this is a matrix of complex numbers
contour(x,y,Re(m),levels=c(-2,0,2),drawlabels=F,axes=F) 
# Re(m) takes the real parts

