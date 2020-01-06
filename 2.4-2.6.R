#masking - multiplying binary times image we want to mask
library(oro.nifti)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_1/113")
T1 <- readNIfTI("113-01-MPRAGE.nii.gz", reorient = FALSE)
orthographic(T1)
#mask image is just part of the brain in 0s and 1s that we might want from the brain
mask <- readNIfTI("113-01-MPRAGE_mask.nii.gz")
orthographic(mask)
#multiplying mask by image gives everything we don't want in 0s and everything we do want in 1s
masked.T1 <- T1 * mask
orthographic(masked.T1)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_2/113")
T1.follow <- readNIfTI("113-02-MPRAGE.nii.gz", reorient = FALSE)
#gives a nifti object that shows difference in intensities between two images
subtract.T1 <- T1.follow - T1
min(subtract.T1)
max(subtract.T1)
orthographic(subtract.T1)
#similar all standard mathematical operations work to return nifti object
add.T1 <- T1.follow + T1
multiply.T1 <- T1.follow * T1 
#make sure that dimensions match
#masking is just one operation where one image is binary

#transformations and smoothing
im_hist <- hist(T1, plot=FALSE)
#number of lines in margin
par(mar=c(5,4,4,4)+0.3)
coll = rgb(0,0,1,1/2)
plot(im_hist$mids, im_hist$counts, log = "y", type = "h", lwd = 10, lend = 2, col = coll, xlab = "Intensity Values", ylab = "Count (Log Scale)")
#first define the transfer function and then apply it to the image
#linear tranfer function
#keeps current frame for new plot
par(new=TRUE)
curve(x*1, axes = FALSE, xlab="", ylab = "", col=2, lwd=3)
axis(side=4, at=pretty(range(im_hist$mids))/max(T1), labels = pretty(range(im_hist$mids)))
mtext("Original Intensity", side = 4, line = 2)
#define a linear spline function
lin.sp<-function(x,knots,slope)
{knots<-c(min(x),knots,max(x))
slopeS<-slope[1]
for(j in 2:length(slope)){slopeS<-c(slopeS,slope[j]-sum(slopeS))}
rvals<-numeric(length(x))
for(i in 2:length(knots))
{rvals<-ifelse(x>=knots[i-1], slopeS[i-1]*(x-knots[i-1])+rvals,rvals)}
return(rvals)}
#Define a spline with two knots and three slopes
knot.vals<-c(.3,.6)
slp.vals<-c(1,.5,.25)
par(new = TRUE)
curve(lin.sp(x,knot.vals,slp.vals),axes=FALSE,xlab="",ylab="",col=2,lwd=3)
axis(side=4,at = pretty(range(im_hist$mids))/max(T1),labels=pretty(range(im_hist$mids)))
mtext("Transformed Intensity", side=4, line=2)
#apply transformation functions to pictures
trans_T1<-lin.sp(T1, knot.vals*max(T1), slp.vals)
image(T1,z=11,plot.type='single', main="Original Image")
image(trans_T1,z=11,plot.type='single',main="Transformed Image")
#smoothing
install.packages("AnalyzeFMRI")
library(AnalyzeFMRI)
#reduces amount of noise in images; ksize is kernel size
smooth.T1 <- GaussSmoothArray(T1,voxdim=c(1,1,1),ksize=1,sigma=diag(3,3),mask=NULL,var.norm=FALSE)
orthographic(smooth.T1)

#basic MRI Contrasts
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/BRAINIX/NIfTI")
sequence <- "FLAIR"
volume.f <- readNIfTI("FLAIR.nii.gz", reorient = FALSE)
volume.f <- cal_img(volume.f)
image(volume.f, z=12, plot.type="single")
volume.t1 <- readNIfTI("T1.nii.gz", reorient = FALSE)
volume.t1 <- cal_img(volume.t1)
image(volume.t1, z=12, plot.type = "single")
volume.t2 <- readNIfTI("T2.nii.gz", reorient = FALSE)
volume.t2 <- cal_img(volume.t2)
image(volume.t2, z=12, plot.type = "single")
