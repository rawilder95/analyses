library(Rmisc)

bbar <- function(plotdat) {
    n<-names(plotdat)
    len<-length(n)
    y<-n[len]
    x<-n[1]
    plotSummary <- summarySE(plotdat,measurevar=y,groupvars=n[1:len-1],na.rm=TRUE)
    if (len > 2) { 
        group<-n[2] 
        plot <- ggplot(plotSummary,aes(y=get(y),x=get(x),fill=get(group)),environment=environment()) + geom_bar(stat="identity",position="dodge") + scale_fill_discrete(name=group)
    }
    else {
        plot <- ggplot(plotSummary,aes(y=get(y),x=get(x)),environment=environment()) + geom_bar(stat="identity")
    }
    plot <- plot + xlab(x) + ylab(y)
    plot <- plot + geom_errorbar(width=.1, aes(ymin=get(y)-se, ymax=get(y)+se),position=position_dodge(.9))
    plot
}
