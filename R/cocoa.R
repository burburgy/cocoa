# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




### https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/CCC
#require(epitools)

cocoa <- function(x, y, method="spearman"){
  require( DescTools)


  cor.mat.one <- cor( t(x), method=method)
  cor.mat.two <- cor( t(y), method=method)
  cor.mat.one[ is.na(cor.mat.one)] <- 0
  cor.mat.two[ is.na(cor.mat.two)] <- 0

  n.genus <- nrow(cor.mat.one)
  rs <- matrix(NA, nrow=n.genus, ncol=3)

  for(i in 1:n.genus){
    # i <- 2
    rs.cor.test <- DescTools::CCC( cor.mat.one[i,-i], cor.mat.two[i,-i] )
    rs[i, 1:3] <- unlist(rs.cor.test$rho.c)
  }
  #colnames(rs) <- names(unlist(rs.cor.test))
  colnames(rs) <- names(rs.cor.test$rho.c)
  rownames(rs) <- rownames(cor.mat.one)

  head(rs)
  rs <- data.frame(rs, stringsAsFactors = F)
  head(rs)
  which( is.na(rs$est) )
  rs[ which( is.na(rs$est) ), ] <- 0

  rs
}


### x: COCOA result
plot.cocoa <- function(x){

  rs <- x
  col.set <- c("red", "green")
  rs <- rs[ order(rs$est, decreasing = T), ]
  rs$col <- as.numeric(rs$est<0)
  head(rs)

  par( mar=c(11, 7, 3, 3))
  barplot( rs$est, col=col.set[as.numeric(rs$est>0)+1], names.arg = rownames(rs),
           las=2, ylim=c(-0.2, 0.55),
           cex.names = 0.9, angle=45)
  range( rs$est) ## -0.244 to 0.507
  sum( rs$est > 0)
  sum( rs$est < 0) ## 15
}

