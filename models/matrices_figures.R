source('ibr.R')
source('viz_img.R')
source("matrices.R")

######################################################################
## CREATE FIGURES

pdf("../writeup/figures/hatglasses.pdf", width=5.75, height=2)
ImageViz(stiller.scales)
dev.off()


pdf("../writeup/figures/levels-levels-stim.pdf", width=5.75, height=2)
ImageViz(complex)
dev.off()

pdf("../writeup/figures/levels-twins-stim.pdf", width=5.75, height=2)
ImageViz(twins)
dev.off()

pdf("../writeup/figures/levels-oddman-stim.pdf", width=5.75, height=2)
ImageViz(oddman)
dev.off()

disps <- list(size2.2, size3.2, size4.2, 
              size2.3, size3.3, size4.3, 
              size2.4, size3.4, size4.4)
widths <- c(4, 5.75, 7.5, 4, 5.75, 7.5, 4, 5.75, 7.5 )
names <- c("size2x2", "size3x2", "size4x2",
           "size2x3", "size3x3", "size4x3",
           "size2x4", "size3x4", "size4x4")

for (d in 1:length(disps)) {
  pdf(paste("../plots/", names[d], ".pdf",sep=""), width=widths[d], height=2)
  ImageViz(disps[[d]])
  dev.off()
}
