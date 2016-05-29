simPixByPix = function(img1, img2) {
  al = align(img1$img, img2$img)
  mean(al$a == al$b)
}

knnImg.kernel.simVal = function(orderId, simVal) { simVal }
knnImg.kernel.simValSq = function(orderId, simVal) { simVal^2 }
knnImg.kernel.binary = function(orderId, simVal) { 1 }
knnImg.kernel.order = function(orderId, simVal) { orderId }


# trainImgs lista, $img obrazek bitowy 
knnImg.testOne = function(trainImgsList, clsVec, testImg, 
                       k = 3, 
                       cmpFunc = simPixByPix, 
                       # jaki wpływ będzię mięc obiekt treningowy na głosowanie klasy
                       # orderId to id po posortowaniu wartosci podobienstw z obiektem testowanym (1 do k)
                       # simVal to wartosc podobienstwa pomiedzy danym obiektem treningowym a testowanym
                       kernelFunc = knnImg.kernel.simVal,
                       returnSimRow = F,
                       returnVotes = F
                      ) {
  res = NULL
  k = min(k, length(clsVec))
  if ( length(trainImgsList) != length(clsVec) ) {
    stop ("Training list has to be the same length as clsVec")
  }
  sims = lapply(trainImgsList, cmpFunc, testImg)
  sims = unlist(sims)
  names(sims) = clsVec
  
  if (returnSimRow) {
    trainNames = names(trainImgsList)
    if (is.null(trainNames)) trainNames = 1:length(trainImgsList)
    simsDf = data.frame(cls = as.character(clsVec), stringsAsFactors = F)
    simsDf = cbind(simsDf, trainId = trainNames, sim = sims)
    #rownames(simsDf) = NULL
    res$simRow = simsDf
  }
  
  simsSort = sort(sims, decreasing = T)
  
  voteCls = unique(names(sims))
  votes = integer(length(voteCls))
  names(votes) = voteCls
  
  for (i in 1:k) {
    cls = names(simsSort[i])
    votes[cls] = votes[cls] + kernelFunc(i, simsSort[i]) 
  }
  
  if (returnVotes) {
    res$votes = votes
  }
  
  maxClsIdx = which.max(votes)
  res$predScore = votes[[maxClsIdx]]
  res$predCls = names(votes[maxClsIdx])
  res
} 

listToDF = function(list) do.call(rbind, lapply(list, data.frame, stringsAsFactors=FALSE))

library(parallel)
knnImg.test = function(trainImgsList, clsVec, testImgsList, mc.cores=1, ...) {
  if (mc.cores == 1) {
    trainPreds = lapply(testImgsList, knnImg.testOne, clsVec=clsVec, trainImgsList = trainImgsList, ...)
  } else {
    trainPreds = mclapply(testImgsList, knnImg.testOne, clsVec=clsVec, trainImgsList = trainImgsList, ..., mc.cores=mc.cores)
  }
  
  tmp = do.call(rbind, lapply(trainPreds, function(row) {
      c1 = data.frame(predScore=row$predScore, predCls = row$predCls, stringsAsFactors = F)
      if (!is.null(row$simRow)) {
        c2 = row$simRow[,"sim"]
        c2 = t(c2)
        colnames(c2) = as.character(row$simRow[,"trainId"])
      }
      cbind(c1,c2)
  })) 
  #tmp = listToDF(trainPreds)
  tmp
}

