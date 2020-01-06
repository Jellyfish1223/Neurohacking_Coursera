install.packages("devtools")
devtools::install_github("stnava/cmaker")
devtools::install_github("stnava/ITKR")
devtools::install_github("stnava/ANTsR")

library(ANTsR)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_1/113")
aimg = antsImageRead("113-01-MPRAGE.nii.gz", dimension = 3)
#tells pixel type (how the image is stored: int vs fraction), dimension, and pointer (where data is stored)
class(aimg)
mean(aimg)
mean(aimg[aimg!=0])
#extracts image from antsImage
class(as.array(aimg))
#ants objects better than nifti objects because ants class faster at performing functions
devtools::install_github("muschellij2/extrantsr")
library(extrantsr)
#changes ants to nifti
class(nim <- ants2oro(aimg))

#preprocessing in ANTsR
#pass in nifti object, returns bias corrected image in n3 and n4
#n4 resolves some problems of n3 (new version)
#using extrantsr::bias_correct wraps n3BiasFieldCorrection and n4BiasFieldCorrection from antsr
n3img = bias_correct(nim, correction = "N3", retimg = TRUE)
n4img = bias_correct(nim, correction = "N4", retimg = TRUE)
#image registration (matching up the images to the same space)
#from extrantsr, use ants_regwrite which transforms filename image to space of template filename
registered_n4 = ants_regwrite(filename = n4img, template.file = template, remove.warp = TRUE, typeofTransform = "Rigid")
typeofTransform = "Rigid", "Affine", "SyN"
#syn is nonlinear