validPreds = knnImg.test(trainImgPix, clsVecTr, validImgPix, 
                         k = 1, cmpFunc = simPixByPixBlackOnly, returnSimRow = T, mc.cores = 8)
tmp = (validPreds[,"predCls"] == as.character(validClsVec))
mean(tmp)


# nie odpalac combined na calosci
#validPreds = knnImg.test(trainImgPix, clsVecTr, validImgPix, 
#                         k = 1, cmpFunc = simCombined1, returnSimRow = T, mc.cores = 8)
#tmp = (validPreds[,"predCls"] == as.character(validClsVec))
#mean(tmp)

# powtorne odpalenie dla zaklasyfikowanych jako:
repeatChar = c("b","h","c","e","i","l","n","o","t","!")

toRepeatIdx = which(validPreds[,"predCls"] %in% repeatChar)
length(toRepeatIdx)
validPreds2 = knnImg.test(trainImgPix, clsVecTr, validImgPix[toRepeatIdx], 
                         k = 1, cmpFunc = simCombined1, returnSimRow = T, mc.cores = 8)
# jak bardzo sie zmienil werdykt
#mean(validPreds[toRepeatIdx,"predCls"] == validPreds2[,"predCls"])
validPreds[toRepeatIdx,] = validPreds2
tmp = (validPreds[,"predCls"] == as.character(validClsVec))
mean(tmp) #spadlo
# KIEPSKO DZIALA - REZYGNUJE


# powiększam zbior treningowy
# dla k = 1 nierówność klas nie powinna miec znaczenia




####### wyrysowanie złych
X11()

wrong = which(!tmp)
validPreds[wrong,"predCls"]
#View(validPreds[wrong[3],])

K = 5
#par(mfrow=c(length(wrong),K+1))
par(mfrow=c(1,K+1))

for (wId in wrong) {
  predRow = validPreds[wId,]
  teImg = rownames(predRow)
  draw(teImg, dir="valid", title=paste(teImg, "\nvalid img"))
  sims = predRow[,-c(1,2)]
  sims = sims[,order(sims, decreasing = T)]
  for (i in 1:K) {
    trImg = colnames(sims)[i]
    draw(trImg, dir="train", title=paste(trImg, "\nsim =", as.character(round(sims[[i]],3)))  )
  }
}
      
