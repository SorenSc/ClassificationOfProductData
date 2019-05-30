require(tikzDevice)       # Export plot as tex-file
library(scatterplot3d)    # three-dimensional scatterplots

brakepads <- read.csv(file="C:\\Users\\Sören\\Documents\\ClassificationOfProductData\\test.csv", header=TRUE, sep=",")
head(brakepads)

# Design of plot
colors = c("grey", "black")
xlab = "Breite in mm"
ylab = "LÃ¤nge in mm"
zlab = "Dicke in mm"

# Assign color to data
brakepads["colors"] = sapply(brakepads$classOfPart, as.character)
brakepads$colors[brakepads$colors=="Vorderachse"]=colors[1]
brakepads$colors[brakepads$colors=="Hinterachse"]=colors[2]

# Assign color to data
brakepads["pch"] = sapply(brakepads$classOfPart, as.numeric)
brakepads$pch[brakepads$pch==1]=1
brakepads$pch[brakepads$pch==2]=4

head(brakepads)

pairs(brakepads[,2:4], col=brakepads$colors,pch=brakepads$pch)



tikz('C:\\Users\\Public\\Documents\\scatterplotBrakePads2.tex', width = 5.75, height = 4)
pairs(brakepads[,2:4], col=brakepads$colors,pch=brakepads$pch)
dev.off()



head(brakepads)

brakepads$index[brakepads$length>=1000]

plotBrakePadData(brakepads)
