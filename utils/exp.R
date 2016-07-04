##* ****************************************************************
##  Programmer[s]: Leandro Fernandes
##  Company/Institution: 
##  email: leandroohf@gmail.com
##  Date: June 18, 2016
##  
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************

library(ggplot2)

PlotTermFreqDashBoard <- function(term.freq,freq.thr = 500){

    wf <- data.frame(word=names(term.freq), freq=term.freq, row.names = NULL)   
    p <- ggplot(subset(wf, freq> freq.thr), aes(word, freq))    
    p <- p + geom_bar(stat="identity")   
    p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
    print(p)

}

PlotScatterPlotDensity <- function(df){

    ##placeholder plot - prints nothing at all
    empty <- ggplot() + geom_point(aes(1,1), colour="white") +
        theme(
            plot.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
        )

                                        #scatterplot of x and y variables
    scatter <- ggplot(df,aes(nword, post.sent)) + 
        geom_point(aes(color=requester_received_pizza)) + 
        scale_color_manual(values = c("orange", "purple")) + 
        theme(legend.position=c(1,1),legend.justification=c(1,1)) 

                                        #marginal density of x - plot on top
    plot_top <- ggplot(df, aes(nword, fill=requester_received_pizza)) + 
        geom_density(alpha=.5) + 
        scale_fill_manual(values = c("orange", "purple")) + 
        theme(legend.position = "none")

                                        #marginal density of y - plot on the right
    plot_right <- ggplot(df, aes(post.sent, fill=requester_received_pizza)) + 
        geom_density(alpha=.5) + 
        coord_flip() + 
        scale_fill_manual(values = c("orange", "purple")) + 
        theme(legend.position = "none") 

                                        #arrange the plots together, with appropriate height and width for each row and column
    grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

}
