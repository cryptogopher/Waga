# requires: 'segmented', 'googlesheets'
if ( !('segmented' %in% installed.packages()[, "Package"])) {
  stop('The package segmented was not installed')
} else {
  require(segmented)
}
if ( !('googlesheets' %in% installed.packages()[, "Package"])) {
  stop('The package googlesheets was not installed')
} else {
  require(googlesheets)
}

setwd("/mnt/doc/projekty/R")

# wariant 1: import z CSV
#dane = read.csv("Waga - 2016.csv")
#plot(dane$Day, dane$Fat..kg.)
#dframe = data.frame(x=dane$Day, y=dane$Fat..kg.)
# wariant 2: import z Google sheets
gap = gs_title("Waga")
days = gs_read(gap, ws="2016", range="D1:D500")
fat = gs_read(gap, ws="2016", range="X1:X500")
mscl = gs_read(gap, ws="2016", range="Y1:Y500")
dfFat = data.frame(x = days$"Day"[1:length(fat$"Fat\n[kg]")], y = fat$"Fat\n[kg]")
dfMscl = data.frame(x = days$"Day"[1:length(mscl$"Muscle\n[kg]")], y = mscl$"Muscle\n[kg]")

linmodFat = lm(y~x, data=dfFat)
linmodMscl = lm(y~x, data=dfMscl)
# wariant 1: automatyczny dobór PSI
#segcon = seg.control(it.max=30, stop.if.error=FALSE, K=length(dane$Day)/14, n.boot=0)
#segmod2 = segmented.lm(linmod, seg.Z=~x, psi=list(x=NA), control=segcon)
# wariant 2: jako PSI podaję daty ważniejszych zmian w diecie
#segmod2 = segmented.lm(linmod, seg.Z=~x, psi=list(x=c(66,148,262,297)))
segmod2Fat = segmented.lm(linmodFat, seg.Z=~x, psi=list(x=c(60,120,184,240,302,370)))
# najlepsze trafienie: 66 104 251 275 297
# slope: -.0695 -.0087 -.0312 -.0892 .0230 -.0789
segmod2Mscl = segmented.lm(linmodMscl, seg.Z=~x, psi=list(x=c(200)))

old.par <- par(mfrow=c(1, 2))

plot(segmod2Fat, xlab="Day", ylab="Fat [kg]")
lines(segmod2Fat)
lines(dfFat)

plot(segmod2Mscl, xlab="Day", ylab="Muscle [kg]")
lines(segmod2Mscl)
lines(dfMscl)

par(old.par)

# slopes = slope(segmod2)$x[,1]
# breaks = summary(segmod2)$psi[,2]
# breaks = append(breaks, 0, after=0)
# breaks = append(breaks, dim(dframe)[1]-1)
# for (i in 1:length(slopes)) {
#   height = max(c(dframe$y[floor(breaks[i])+1], dframe$y[floor(breaks[i+1])+1]))
# #  text((breaks[i+1]+breaks[i])/2, height+1, sprintf("%.4f", slopes[i]))
# }

summary(segmod2Fat)
slope(segmod2Fat)
summary(segmod2Mscl)
slope(segmod2Mscl)

#dev.print(png, "fat.png", width=1024)
