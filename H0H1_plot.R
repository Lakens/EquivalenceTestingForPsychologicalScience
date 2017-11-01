library(ggplot2)
library(gridExtra)

plotheight <- 0.9
lowerbound <- -0.2
upperbound <- 0.2
df <- data.frame()

baseplot <-   ggplot(df) +
  scale_y_continuous(limits = c(0,plotheight+0.02), breaks=NULL) + # no y-axis will be displayed
  scale_x_continuous(limits = c(-0.85,0.85), breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8), 
                     labels=c("", "", "", "", "0", "", "", "", ""), name = "effect size") +
  theme_classic() +
  theme(plot.title = element_text(size = rel(1.3), face = "bold"), #font size & appearance for plot titles
        axis.title.y = element_blank(), #remove title of y-axis
        axis.title.x = element_text(size=rel(1.1), lineheight = 0.5), #font size for x-axis label
        plot.margin=unit(c(0.5,0.8,0.5,0.8),"cm")) #add padding around each plot to make them look nicer when combined; margin order: top, right, bottom, left

#equivalence test plot
eqplot <- baseplot +  
  ggtitle("A: Equivalence test") +
  annotate("segment", x = lowerbound, xend = lowerbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for lower bound
  annotate("segment", x = upperbound, xend = upperbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for upper bound
  annotate("rect", xmin = -Inf, xmax = lowerbound, ymin = -Inf, ymax = plotheight, fill = "red", alpha = .2, color = NA) + #shading for lower area
  annotate("rect", xmin = upperbound, xmax = Inf, ymin = -Inf, ymax = plotheight, fill = "red", alpha = .2, color = NA) + #shading for upper area
  annotate("text", size = rel(5), x=-0.6, y=plotheight/2, parse=TRUE, label="H[0]") + #label for lower area (H0)
  annotate("text", size = rel(5), x=0.6, y=plotheight/2, parse=TRUE, label="H[0]") + #label for upper area (H0)
  annotate("text", size = rel(5), x=-0, y=plotheight/2, parse=TRUE, label="H[1]", hjust = 0.3) #label for equivalence area

#NHST plot
NHSTplot <- baseplot +  
  ggtitle("B: Classic NHST (two-sided)") +
  annotate("segment", x = 0, xend = 0, y = plotheight-plotheight/2.4, yend = -Inf) + #vertical line at x=0 (H0)
  annotate("text", size = rel(5), x=0, y = plotheight, parse=TRUE, label="H[0]", hjust = 0.3) + #label for point null (H0)
  annotate("segment", x = 0, xend = 0, y = plotheight-plotheight/8, yend=plotheight-plotheight/2.9, 
           arrow = arrow(type = "closed", length=unit(2, "mm"))) + #arrow pointing from H0 label to H0 line
  annotate("text", size = rel(5), x=-0.45, y=plotheight/3, parse=TRUE, label="H[1]") + #label for lower area (H1)
  annotate("text", size = rel(5), x=0.45, y=plotheight/3, parse=TRUE, label="H[1]") #label for upper area (H1)

#minimal effects test plot
METplot <-   baseplot +  
  ggtitle("C: Minimal effects test") +
  annotate("segment", x = lowerbound, xend = lowerbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for lower bound
  annotate("segment", x = upperbound, xend = upperbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for upper bound
  annotate("rect", xmin = -0.2, xmax = 0.2, ymin = -Inf, ymax = 0.9, fill = "red", alpha = .2, color = NA) + #shading for H0 area
  annotate("text", size = rel(5), x=-0.6, y=plotheight/2, parse=TRUE, label="H[1]") + #label for lower area (H1)
  annotate("text", size = rel(5), x=0.6, y=plotheight/2, parse=TRUE, label="H[1]") + #label for upper area (H1)
  annotate("text", size = rel(5), x=-0, y=plotheight/2, parse=TRUE, label="H[0]", hjust = 0.3) #label for minimal effects area (H1)

#inferiority plot
infplot <-   baseplot +  
  ggtitle("D: Inferiority test") +
  annotate("segment", x = upperbound, xend = upperbound, y = plotheight, yend = -Inf, linetype = "dashed") + #dashed line for inferiority bound
  annotate("rect", xmin = upperbound, xmax = Inf, ymin = -Inf, ymax = 0.9, fill = "red", alpha = .2, color = NA) + #shading for H0 area
  annotate("text", size = rel(5), x=0.6, y=plotheight/2, parse=TRUE, label="H[0]") + #label for upper area (H0)
  annotate("text", size = rel(5), x=-0.35, y=plotheight/2, parse=TRUE, label="H[1]") #label for lower area (H1)

grid.arrange(eqplot, NHSTplot, METplot, infplot, ncol = 1) #combine plots in one column (all stacked)

