validPreds = knnImg.test(trainImgPix, clsVecTr, validImgPix, k = 1, returnSimRow = T, mc.cores = 8)

tmp = (validPreds[,"predCls"] == as.character(validClsVec))
mean(tmp)

tmp = (validPreds[,"predCls"] == as.character(validClsVec))
mean(tmp)

####### wyrysowanie złych
X11()

wrong = which(!tmp)
#View(validPreds[wrong[3],])

K = 5
#par(mfrow=c(length(wrong),K+1))
par(mfrow=c(1,K+1))

for (wId in wrong) {
  predRow = validPreds[wId,]
  teImg = rownames(predRow)
  draw(teImg, dir="valid", title="valid img")
  sims = predRow[,-c(1,2)]
  sims = sims[,order(sims, decreasing = T)]
  for (i in 1:K) {
    trImg = colnames(sims)[i]
    draw(trImg, dir="train", title=paste(trImg, "\nsim =", as.character(round(sims[[i]],3)))  )
  }
}
      
