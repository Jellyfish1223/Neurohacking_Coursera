library(devtools)
library(oro.dicom)
#work with one slice
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/BRAINIX/DICOM/FLAIR")
#one slice of a brain image (out of 22)
slice=readDICOM("IM-0001-0011.dcm")
dim(slice$img[[1]])
d=dim(t(slice$img[[1]]))
#plotting this one slice of brain
image(1:d[1],1:d[2],t(slice$img[[1]]),col=gray(0:64/64))
#getting the numbers for a specific spot in the brain
slice$img[[1]] [101:105,121:125]
#creating a density histogram
hist(slice$img[[1]][,],breaks=50, xlab="FLAIR",prob=T, col = rgb(0,0,1,1/4),main="")
hdr=slice$hdr[[1]]
#value of header of name PixelSpacing ---- each pixel in this slice has
#dimension 0.798 by 0.798 so tells us the resolution
hdr[hdr$name=="PixelSpacing","value"]
hdr[hdr$name=="FlipAngle", ]

#work with all slices
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/BRAINIX/DICOM")
all_slices_T1 = readDICOM("T1/")
dim(all_slices_T1$img[[11]])
hdr=all_slices_T1$hdr[[11]]
hdr[hdr$name == "PixelSpacing", "value"]
#much smaller pixel size so higher resolution

#DICOM and NIfTI
nii_T1 = dicom2nifti(all_slices_T1)
d = dim(nii_T1)
image(1:d[1],1:d[2], nii_T1[,,11], col = gray(0:64/64), xlab = "", ylab = "")

#NIfTI
library(oro.nifti)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/BRAINIX/NIfTI")
#filename that we want
fname="Output_3D_File"
#write nii_T1 into a NIfTI file with our filename, if we don't want compressed,
#we can say gzipped = false
writeNIfTI(nim = nii_T1, filename = fname)
#output the list of files that have our filename in the current directory
list.files(getwd(),pattern = "Output_3D_File")
#output the list of files that have T in the current directory
list.files(getwd(), pattern = "T")
#read in a nifti file
nii_T2 = readNIfTI("T2.nii.gz", reorient = FALSE)
dim(nii_T2)

#basic visualization
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/BRAINIX/NIfTI")
fname = "Output_3D_File"
print({nii_T1 = readNIfTI(fname = fname)})
#dimension is 512*512*22 which means x and y 512 and going up 22 slice of brain
#print image - in x --> 1 to the xdim and in y --> 1 to the ydim; use 11th slice
#prints it automatically with heat palette - lower intensities red
image(1:d[1],1:d[2], nii_T1[,,11], xlab = "", ylab = "")
#can also pass in nifti object to image function, use z to specify slice --
#white is high intensity, black is low
image(nii_T1, z=11, plot.type = "single")
#without plot.type equals single, by default all slices are plotted
image(nii_T1)
#orthographic shows all three planes of brain -- axial, sagittal, and coronal 
#axial - slicing parallel to neck, sagittal - slicing down noseline, coronal -
#slicing parallel to spine
#200 in x and 220 in y in 11th slice (z axis)
orthographic(nii_T1, xyz = c(200,220, 11))

#basic visualization pt.2 - graphs
#create two plots with two columns, one row
par(mfrow=c(1,2))
#set margins to 0
o <- par(mar=c(4,4,0,0))
#histogram of nifti object - large spike at 0 because lots of area for background
hist(nii_T1, breaks = 75, prob = T, xlab = "T1 Intensities", col = rgb(0,0,1, 1/2), main = "")
#histogram of intensities greater than 20
hist(nii_T1[nii_T1>20], breaks = 75, prob = T, xlab = "T1 Intensities >20", col = rgb(0,0,1, 1/2), main = "")
#backmapping 
#which places have intensities between 300 and 400
is_btw_300_400 <- ((nii_T1>300) & (nii_T1<400))
nii_T1_mask <- nii_T1
#saying that values not between 300 and 400 are not applicable 
nii_T1_mask[!is_btw_300_400] = NA
overlay(nii_T1, nii_T1_mask, z=11, plot.type = "single")
overlay(nii_T1, nii_T1_mask)
orthographic(nii_T1, nii_T1_mask, xyz = c(200,220, 11))
