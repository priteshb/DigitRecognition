#libraries used:
library(devtools)
library(ggbiplot)
library(ggplot2)
library(HSAUR2)
library(outliers)
library(jpeg)
library(matlib)

pc = prcomp(classDigits[, 2:785], center = TRUE, scale = FALSE) #PCA of classDigits
digitMatrix = matrix(pc$center,28,28,byrow = TRUE) #matrix for mean digit
writeJPEG(digitMatrix, target = "meanDigit.jpg") #save mean digit jpg

#Problem 3(c)

#k = 5
image15 = pc$center
image100 = pc$center

for (i in 1:5) { 
  image15 = image15 + ((classDigits[15,2:785] - pc$center)*pc$rotation[i])*pc$rotation[i]
  image100 = image100 + ((classDigits[100,2:785] - pc$center)*pc$rotation[i])*pc$rotation[i]
}
digitImage15 = matrix(as.numeric(image15)/255,28,28,byrow = TRUE) #create matrix for image#15
digitImage100 = matrix(as.numeric(image100)/255,28,28,byrow = TRUE) #create matrix for image#100
writeJPEG(digitImage15, target = "image15-5.jpg") #save image for image#15
writeJPEG(digitImage100, target = "image100-5.jpg") #save image for image#100

#k = 20
image15 = pc$center
image100 = pc$center

for (i in 1:20) { 
  image15 = image15 + ((classDigits[15,2:785] - pc$center)*pc$rotation[i])*pc$rotation[i]
  image100 = image100 + ((classDigits[100,2:785] - pc$center)*pc$rotation[i])*pc$rotation[i]
}
digitImage15 = matrix(as.numeric(image15)/255,28,28,byrow = TRUE) #create matrix for image#15
digitImage100 = matrix(as.numeric(image100)/255,28,28,byrow = TRUE) #create matrix for image#100
writeJPEG(digitImage15, target = "image15-20.jpg") #save image for image#15
writeJPEG(digitImage100, target = "image100-20.jpg") #save image for image#100

#k = 100
image15 = pc$center
image100 = pc$center

for (i in 1:100) { 
  image15 = image15 + ((classDigits[15,2:785] - pc$center)*pc$rotation[i])*pc$rotation[i]
  image100 = image100 + ((classDigits[100,2:785] - pc$center)*pc$rotation[i])*pc$rotation[i]
}
digitImage15 = matrix(as.numeric(image15)/255,28,28,byrow = TRUE) #create matrix for image#15
digitImage100 = matrix(as.numeric(image100)/255,28,28,byrow = TRUE) #create matrix for image#100
writeJPEG(digitImage15, target = "image15-100.jpg") #save image for image#15
writeJPEG(digitImage100, target = "image100-100.jpg") #save image for image#100

#Problem 3(d)
summary_ = summary(pc) #summary of the PCA

#plot PCA variance with PC index
plot(summary_$importance[3,],col="red", xlab = "PCA Number",ylab = "Cumulative variance proportion") #Variance plot

#using k = 200
class7test = read.csv("Class7Test.csv", header = TRUE) #read test data

D = array(0,dim = c(7,1)) #variable to store the mahalanobis distance
k = 200 #using value of k for which at least 90% variance is captured on the training set
covMat2 = cov(pc$x[,1:k]) #Covariance matrix of the PCA

for(i in 1:7) { #calculate Mahalanobis distance for each test data
  x = (as.numeric(class7test[i,3:786])%*%pc$rotation)[,1:k]
  x.bar = (pc$center%*%pc$rotation)[,1:k]
  D[i] = mahalanobis(x,x.bar,covMat2)
}

#Problem 3(e)
D.train = array(0,dim = c(30000,2)) #variable to store the mahalanobis distance
M.train = array(0,dim = c(10,2)) #Median Mahalanobis distance for each training digit
D = array(0,dim = c(7,1)) #variable to store the mahalanobis distance
ki = array(0,dim = c(10,1)) #Minimum k for each digit
t = array(Inf, dim = c(10,1)) #temporary variable for each digit

for(k in 2:25) {
  covMat2 = cov(pc$x[,1:k]) #Covariance matrix of the PCA
  for(i in 1:30000) { #calculate Mahalanobis distance for each test data
    x = (as.numeric(classDigits[i,2:785])%*%pc$rotation)[,1:k]
    x.bar = (pc$center%*%pc$rotation)[,1:k]
    D.train[i,2] = mahalanobis(x,x.bar,covMat2) #mahalanobis distance calculation
    D.train[i,1] = classDigits[i,1]
  }
  for(i in 1:10) { #Clculate median Mahalanobis distance for each digit in training dataset
    M.train[i,1] = i-1
    M.train[i,2] = median(D.train[classDigits[,1] == i-1,2])
  }
  #calculate Mahalanobis distance for each test data for each k value and 
  #compare with training median mahalanobis distances
  for(i in 1:7) { 
    x = (as.numeric(class7test[i,3:786])%*%pc$rotation)[,1:k]
    x.bar = (pc$center%*%pc$rotation)[,1:k]
    D[i] = mahalanobis(x,x.bar,covMat2)#mahalanobis distance calculation
    if(abs(D[i] - M.train[class7test[i+1,2],2]) < t[i]) { #find ideal k for each test digit
      t[i] = abs(abs(D[i] - M.train[class7test[i+1,2],2]))
      k[i] = k
    }
  }
}
