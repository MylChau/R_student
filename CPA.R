
#definition of the dataset

maths=c(6,8,6,14.5,14,11,5.5,13,9)		#definition of the variable maths
phys=c(6,8,7,14.5,14,10,7,12.5,9.5)
french=c(5,8,11,15.5,12,5.5,14,8.5,12.5)
english=c(5.5,8,9.5,15,12.5,7,11.5,9.5,12)
X=matrix(c(maths,phys,french,english),ncol=4)	#definition of the matrix of data
X
colnames(X)=c('maths','phys','french','english')	#to add names to the columns
X
rownames(X)=c('jean','alain','annie','monique','didier','andre','pierre','brigitte','evelyne') #to add names to thr individuals
X

apply(X,2,mean)		#to compute the mean of each variables

apply(X,2,sd)		#to compute the standard deviation of each variable
sqrt(apply(X,2,var))	#other way
sqrt(8/9)*sqrt(apply(X,2,var))	#corrected standard deviation (with the variance in 1/n instead of 1/n)
apply(X,2,min)
apply(X,2,max)
max(X)
cor(X)		#to compute the correlation matrix
cov(X)		#to compute the usual covariance matrix
apply(X,2,var)
8/9*cov(X)	#the corrected covariance matrix
varn<-function(x)	#function to compute the variance in 1/n
{
  n=length(x)
  varn=(n-1)/n*var(x)
}
P=princomp(X)		#to perform PCA
P
names(P)		#to see that P is a list that contains a lot of information
P$center		#vector of the mean 
P$scale			#vector to say if the data are normalized or not. If only 1, not normalized, otherwise the vector contains the corrected standard deviation
P1=princomp(X,cor=TRUE) #to perform normalized PCA
P1$center
P1$scale
X
m=apply(X,2,mean)  
X-m			#not the good way to center the data
X			#compare the elements in position (1,1) and (1,2) in X and X-m
X-m
M=matrix(rep(m,9),ncol=4,byrow=TRUE)  #to create a matrix where the vector m is repeated 9 times
M
Xc=X-M				#centered data
Xc
t(matrix(m)%*%matrix(rep(1,9),nrow=1)) #another way to do it
apply(Xc,2,mean)		#to verify that the data are centered so that the empirical mean of the variables of Xc is 0
Mc=apply(Xc,2,mean)
(Mc==0)				#be careful, to test equality to 0 is not lyke this
(abs(Mc)<=10^(-15))		#better
D=diag(1/(sqrt(8/9)*apply(X,2,sd)))	#definition of the matrix with 1/s on the diagonal
D
Xn=Xc%*%D				#computation of the normalized data
Xn
apply(Xn,2,sd)*sqrt(8/9)
s=sqrt(8/9)*apply(X,2,sd)
s
S=matrix(rep(s,9),ncol=4,byrow=TRUE)
S
Xc/S					#other way to compute tyhe normalized data
Xn

V=1/9*t(Xc)%*%Xc			#matrix of covariance of X
V
8/9*cov(X)				#V is equal to this
8/9*cov(Xc)				#or this
E1=eigen(V)				#computation of the eigen elements of V
E1					#list with 2 elements
e1va=E1$values
P$sdev
e1va
(P$sdev)^2				#thus P$sdev contains the vector with the square root of the eignevalues of V 
P$loadings
E1$vectors				#P$loadings contains the eigenvectors of V
summary(P)				#the first line of the table is the square root of the eignevalues which is alos the standard deviation of the new created variables
#the second line contains the proportion of variance it means the eigenvalue divided by the sum of the eigenvalues
#the third line contains the cumulative vector of the previous one
#it is helpful to determine the number of newcomponents to keep because we want to keep 80% or 90% of the information
plot(P)			#another way to see how many components to keep
X
Xc%*%E1$vectors		#projection of the individuals on the first component (first new variable)
P$scores		#same thing
plot(P$scores[,1],P$scores[,2]) 	#projection of the individuals on the first factorial plan
plot(P$scores[,1],P$scores[,2],xlab='comp1',ylab='comp2',type='n')
abline(h=0,v=0)
text(P$scores[,1],P$scores[,2],labels=1:9)		#better since we have the number of the individuals


cor(Xc,P$scores[,1])		#projection of the variables on the fist new one
cor(Xc[,1],P$scores)
cor(Xc,P$scores[,1])
cor(Xc,P$scores[,2])

axe1=cor(Xc,P$scores[,1])
axe2=cor(Xc,P$scores[,2])
plot(axe1,axe2,xlim=c(-1,1),ylim=c(-1,1))
ablin(v=0,h=0)
abline(v=0,h=0)
t=seq(0,2*pi,0.01)
x=cos(t)
y=sin(t)
lines(x,y,col='red')   #drawing of the projection of the variables on the first factorial plan with the circle of correlation.
# a variable is well represented if close to this circle
