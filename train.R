library(dplyr)
trainImg = list.files(path="train/", pattern = ".*\\.png")

trainDf = splitImgNames(trainImg)
table(trainDf[,"char"])

#######################################################################

trainGr = trainDf %>% group_by(char) %>% summarise(imgs=list(as.character(validImg)))

# obliczam treshold podobienstwa dla kazdej grupy (minimalna wartosc podobienstwa pomiedzy obiektami wewnatrz grupy)
tresholds = apply(trainGr, 1, function(row,dir="train") {
    imgsN = unlist(row[["imgs"]])
    imgs = loadImgs(imgsN,dir)
    
    combs = combn(1:length(imgs),2,simplify = F)
    sim = lapply(combs, function(pair) {
      i=pair[1]; j=pair[2]
      al = align(imgs[[i]]$img, imgs[[j]]$img)
      mean(al$a == al$b)
    })
    
    treshold = min(unlist(sim))
    treshold
})

trainGr2 = cbind(trainGr, tresholds)

trainImgPix = loadImgs( as.character(trainDf[,"validImg"]), dir="train" )
trainSimMatrix = simImgMatrx(trainImgPix)
colnames(trainSimMatrix) = rownames(trainSimMatrix) = as.character(trainDf[,"char"])
trainSimMatrixR = round(trainSimMatrix, 2)

clsVecTr = trainDf[,"char"]
length(trainImgPix) == length(clsVecTr)

################## knnImg #############################################
# sprawdzamy skutecznosc na zbiorze treningowym - nie powinien sie mylic
trainPreds = knnImg.test(trainImgPix, clsVecTr, trainImgPix, k = 1)
tmp = (trainPreds[,"predCls"] == as.character(clsVecTr))
mean(tmp)
clsVecTr[!tmp]

# na walidacyjnym
validAllImg = list.files(path="valid/", pattern = ".*\\.png")
validImg = setdiff(validAllImg, trainImg)
validDf = splitImgNames(validImg)
table(validDf[,"char"])
validClsVec = validDf[,"char"]
validImgPix = loadImgs( validDf[,"validImg"], dir="valid" )
validPreds = knnImg.test(trainImgPix, clsVecTr, validImgPix, k = 1, mc.cores = 8)
tmp = (validPreds[,"predCls"] == as.character(validClsVec))
mean(tmp)
validClsVec[!tmp]
validPreds[!tmp,]

# done
doneImgDf = splitImgNames(validAllImg)

# na testowym
allImg = list.files(path="png/", pattern = ".*\\.png")
testImg = setdiff(allImg, doneImgDf[,"orgImg"])
length(testImg) + nrow(doneImgDf) == 6168
testImgPix = loadImgs( testImg, dir="png" )
testPreds = knnImg.test(trainImgPix, clsVecTr, testImgPix, k = 1, mc.cores = 8)
testResDf = data.frame(cbind(orgImg=testImg,char=testPreds[,"predCls"]), stringsAsFactors = F)
nrow(testResDf)

# collect results
doneResDf = doneImgDf[,c(3,1)]

result = rbind(testResDf, doneResDf)
nrow(result) == 6168
rownames(result) = result["orgImg"]

# sprawdzenie ilu klas brakuje, dorobimy w jakichs fuck elementach
setdiff(clsAll, result[,"char"])

# zastapienie fuck elementow
fuckImg = list.files(path="fuck/", pattern = ".*\\.png")
fuckDf = splitImgNames(fuckImg)
fuckResDf = fuckDf[,c(3,1)]
result[fuckResDf[,"orgImg"],"char"] = fuckResDf[,"char"]
nrow(result)

length(setdiff(result[,"char"], clsAll)) == 0
length(setdiff(clsAll, result[,"char"])) == 0

# kodujemy na liczby rzeczywiste
charToInt = 1:length(clsAll)
names(charToInt) = clsAll


resultCoded = result
resultCoded[, "char"] = charToInt[result[,"char"]]
write.table(resultCoded, file = "res1.csv", quote = F, sep = ",", row.names = F, col.names = F)

#######################################################################
# klastrowanie

# klastrowanie single linkage działa idealnie:
library(flashClust)
?flashClust

N = nrow(resultCoded)
mtr = sapply(resultCoded[,"char"], function(c) as.numeric(c != resultCoded[,"char"]) )
mtr = matrix(mtr,N,N)
colnames(mtr) = rownames(mtr) = result[,"char"]

res = hclust(as.dist(mtr), method = "single")
str(res)
mean(res$labels == result[,"char"])

# popatrzec ktore sie źle klastrują i cos z tym zrobic
# recznie poprawic ~250 obiektow do wyłapania
# zapytac = / - i fuck

#######################################################################

# porównanie mojego knn z tesseract / gocr na zbiorze walidacyjnym

# mój: 0.9589744

# tes
ocrTesDone = mclapply(validImg, function(img) 
  #  for (img in allImg) 
  {
    src = paste0("valid/", img)
    tesOut = ocrTesseract("valid", img, NA, lang="pol", psm=10)
    tesOut = tesOut[1]
    print(paste(img,":",tesOut[1]))
    tesOut
  }
  , mc.cores=8
)
# czas > 2min
ocrTesDone = unlist(ocrTesDone)
length(validDf[,"char"]) == length(ocrTesDone)
ocrTesDone[is.na(ocrTesDone)] = "?"
mean(ocrTesDone == validDf[,"char"]) #0.5699634

# gocr
system.time({
  gocrDone = mclapply(validImg, function(img) 
    {
      src = paste0("valid/", img)
      out = gocR(src, a=95)
      if (is.na(out) || out == "_" || length(out) != 1) {
        out = "?"
      }
      out
    }
    , mc.cores=8
  )
})
gocrDone = unlist(gocrDone)
length(validDf[,"char"]) == length(gocrDone)
mean(gocrDone == validDf[,"char"]) #0.51



#######################################################################


a1png = readPNG("train/a_171.png")
a2png = readPNG("train/a_263.png")
a1im = as.cimg(a1png[,,1])
a2im = as.cimg(a2png[,,1])
a1fft = FFT(a1im)
a2fft = FFT(a2im)

a1fft$real == a2fft$real

im = as.cimg(png[,,1])
tmp = FFT(im)

tmp = crop.borders(im,ny=5)
plot(tmp$imag)

im <- as.cimg(function(x,y) {print(class(x)); x}, 50, 50)

at(im,10,1)
at(im,10:12,1)
at(im,10:12,1:3)
color.at(boats,x=10,y=10)


  
allImg
