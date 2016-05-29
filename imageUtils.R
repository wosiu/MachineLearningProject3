library(png)
library(imager)

draw = function(img, dir=NA, title=NA) {
  if (!is.na(dir)) img = paste0(dir,"/",img)
  png = readPNG(img)
  plot(0:1,0:1,type="n",ann=T,xlab=NA,ylab=NA, axes=FALSE,main=title)
  rasterImage(png,0,0,1,1)
}

convert2D = function(png) png[,,1]

crop = function(png) {
  if(length(dim(png)) != 2) {
    stop("2 dims excpected")
  }
  pngN = !png
  row = which(apply(pngN, 1, any))
  ymin = head(row, 1)
  ymax = tail(row, 1)
  col = which(apply(pngN, 2, any))
  xmin = head(col, 1)
  xmax = tail(col, 1)
  list(img=png[ymin:ymax,xmin:xmax], ymin=ymin)
}

im1 = crop(png)
im2 = crop(png)
# tworzy 2 obrazki o tych samych rozmiarach wpisujac pierwotne obrazki w gorny lewy naroznik
align = function(im1, im2) {
  hmax = max(nrow(im1), nrow(im2))
  wmax = max(ncol(im1), ncol(im2))
  base = matrix(rep(1,hmax*wmax), ncol=wmax)
  res1 = base  
  res1[1:nrow(im1),1:ncol(im1)] = im1
  res2 = base
  res2[1:nrow(im2),1:ncol(im2)] = im2
  list(a=res1, b=res2)
}

# load, convert to 2D, crop
library(png)
loadImgs = function(imgVec, dir="train") {
  paths = paste0(dir, "/", imgVec)
  imgs = lapply(paths, readPNG)
  imgs = lapply(imgs, convert2D)
  imgs = lapply(imgs, crop)
  names(imgs) = imgVec
  imgs
}

simImgMatrx = function(imgs) {
  combs = combn(1:length(imgs),2,simplify = F)
  simMatrix = matrix(NA, ncol=length(imgs), nrow=length(imgs))
  #sim = lapply(combs, function(pair) {
  #  i=pair[1]; j=pair[2]
  #  al = align(imgs[[i]]$img, imgs[[j]]$img)
  #  s = mean(al$a == al$b)
  #  s
  #})
  for (pair in combs) {
    i=pair[1]; j=pair[2]
    simMatrix[i,j] = simPixByPix(imgs[[i]], imgs[[j]]) 
  }
  simMatrix
}

# nazwy są postaci cls_nazwapliku.rozserzenie
splitImgNames = function(imgNames) {
  df = lapply(imgNames, function(img) { 
    spl = strsplit(img, "_")[[1]]
    orgImg = gsub("[kd]", "", spl[2])
    data.frame(char=spl[1], validImg=img, orgImg=orgImg, stringsAsFactors = F)
  })
  df = do.call(rbind, df)
  #df = df %>% arrange(char)
  df
}
