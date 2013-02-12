library(shiny)
library(ggplot2)
require(reshape)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#hops.comp <- read.table("hops-comp.txt",header=T)
hops.comp.wide <- read.table("hops-comp-wide.csv",header=T)
hops.comp.wide$Hops.Variety <- as.character(hops.comp.wide$Hops.Variety)
hops.comp.wide$Hops.Variety <- ifelse(hops.comp.wide$Hops.Variety=="Hallertauer Mittelfr\xfch","Hallertauer Mittelfruh",hops.comp.wide$Hops.Variety)
hops.comp.wide$Hops.Variety <- ifelse(hops.comp.wide$Hops.Variety=="H\xfcller Bitterer","Huller Bitterer",hops.comp.wide$Hops.Variety)

shinyServer(function(input, output) {
  
  output$hopList <- reactiveUI(function() {
    selectInput("hopList", "Hop Variety:", unique(hops.comp.wide$Hops.Variety))
  })
  
  output$acidsPlot <- reactivePlot(function() {
    if (length(input$acids)==0) return()
    if (length(input$usage)>0)
      for (use in levels(hops.comp.wide$Usage))
        if (!(use %in% input$usage))
          hops.comp.wide <- subset(hops.comp.wide, Usage!=use, drop=T)
    hoporder <- levels(reorder(hops.comp.wide$Hops.Variety,hops.comp.wide[,input$sortAcids]))
    hops.comp.wide <- subset(hops.comp.wide,
                             Alpha.Acid >= input$alphaRange[1] &
                               Alpha.Acid <= input$alphaRange[2] &
                               Beta.Acid >= input$betaRange[1] &
                               Beta.Acid <= input$betaRange[2] &
                               Co.Humulone >= input$cohumRange[1] &
                               Co.Humulone <= input$cohumRange[2], drop=T)
    hops.comp.wide <- melt(hops.comp.wide[,c(1,2,3,4,5)], measure.vars=c("Alpha.Acid","Beta.Acid","Co.Humulone"))
    for (acid in levels(hops.comp.wide$variable))
      if (!(acid %in% input$acids))
        hops.comp.wide <- subset(hops.comp.wide, variable!=acid, drop=T)
    hops.comp.wide$Hops.Variety <- factor(hops.comp.wide$Hops.Variety, hoporder)
    p <- ggplot(hops.comp.wide) + 
      geom_bar(aes(y=value,x=Hops.Variety,fill=variable),stat="identity",position=position_dodge()) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom", legend.direction="horizontal", axis.title.x = element_blank()) + 
      ylab("Percent Composition") + scale_fill_discrete("Acid Type")
    if (length(input$usage)>0)
      p <- p + facet_grid(.~Usage,scale="free",space="free")
    print(p)
  })
  
  output$oilsPlot <- reactivePlot(function() {
    if (length(input$oils)==0) return()
    if (length(input$usage)>0)
      for (use in levels(hops.comp.wide$Usage))
        if (!(use %in% input$usage))
          hops.comp.wide <- subset(hops.comp.wide, Usage!=use, drop=T)
    hoporder <- levels(reorder(hops.comp.wide$Hops.Variety,hops.comp.wide[,input$sortOils]))
    hops.comp.wide <- subset(hops.comp.wide,
                             Myrcene.Oil >= input$myrceneRange[1] &
                               Myrcene.Oil <= input$myrceneRange[2] &
                               Humulene.Oil >= input$humuleneRange[1] &
                               Humulene.Oil <= input$humuleneRange[2] &
                               Caryophyllene.Oil >= input$caryophylleneRange[1] &
                               Caryophyllene.Oil <= input$caryophylleneRange[2] &
                               Farnesene.Oil >= input$farneseneRange[1] &
                               Farnesene.Oil <= input$farneseneRange[2], drop=T)
    hops.comp.wide <- melt(hops.comp.wide[,c(1,2,6,7,8,9)], measure.vars=c("Myrcene.Oil","Humulene.Oil","Caryophyllene.Oil","Farnesene.Oil"))
    for (oil in levels(hops.comp.wide$variable))
      if (!(oil %in% input$oils))
        hops.comp.wide <- subset(hops.comp.wide, variable!=oil)
    hops.comp.wide$Hops.Variety <- factor(hops.comp.wide$Hops.Variety, hoporder)
    p = ggplot(hops.comp.wide) + 
      geom_bar(aes(y=value,x=Hops.Variety,fill=variable),stat="identity",position=position_dodge()) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom", legend.direction="horizontal", axis.title.x = element_blank()) + 
      ylab("mls/100 grams") + scale_fill_discrete("Oil Type")
    if (length(input$usage)>0)
      p <- p + facet_grid(.~Usage,scale="free",space="free")
    print(p)
  })
  
  output$hopPlot <- reactivePlot(function() {
    hops.comp.wide <- subset(hops.comp.wide, Hops.Variety==input$hopList)
    hops.comp.wide <- melt(hops.comp.wide[,c(1,3:9)])
    hops.comp.wide$kind <- ifelse(grepl("Oil$", hops.comp.wide$variable),"Oil","Acid")
    p1 <- ggplot(subset(hops.comp.wide,kind=="Acid")) + 
      geom_bar(aes(y=value,x=variable,fill=variable)) + 
      ylab("Percent Composition") + xlab("Acid Type") +
      theme(legend.position="none")
    p2 <- ggplot(subset(hops.comp.wide,kind=="Oil")) + 
      geom_bar(aes(y=value,x=variable,fill=variable)) + 
      ylab("mls/100 grams") + xlab("Oil Type") +
      theme(legend.position="none")
    p <- multiplot(p1,p2,cols=2)
    print(p)
  })
})