#FSLR co-registration
#registration within subjects requires fewer degrees of freedom because images are more alike
#example analyses that don't require a template - longitudinal analyses within subjects, etc.

library(oro.nifti)
library(fslr)
library(extrantsr)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_1/113")
T1 = readNIfTI("113-01-MPRAGE.nii.gz", reorient = FALSE)
#use fslr function flirt to register T2w (weighted)(infile) to the T1 (reffile)
T2w = readNIfTI("113-01-T2w.nii.gz")
flirt_reg_t2_img = flirt(infile = T2w, reffile = T1, dof = 6, verbose = FALSE)
#double ortho plots two images side by side
double_ortho(T1, flirt_reg_t2_img)

#ANTsR co-registration
#register T2 to T1 file using ants_regwrite
reg_t2_img = ants_regwrite(filename = T2w, template.file = T1, typeofTransform = "Rigid", verbose = FALSE)
#register FLAIR to T1
flair_file = readNIfTI("113-01-FLAIR.nii.gz")
reg_flair_img = ants_regwrite(filename = flair_file, template.file = T1, typeofTransform = "Rigid", verbose = FALSE)
#overlays registered image to the baseline
library(scales)
ortho2(T1, reg_t2_img, col.y = alpha(hotmetal(), 0.25))
#can do the same for flair

#wrapper functions
#within visit registration
#preprocess_mri_within allows both inhomogeneity correction and registers within a visit
files = c("113-01-MPRAGE.nii.gz", "113-01-T2w.nii.gz", "113-01-FLAIR.nii.gz")
files = file.path("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21", files)
outfiles = c("113-01-MPRAGE_processed.nii.gz", "113-01-T2w_processed.nii.gz", "113-01-FLAIR_processed.nii.gz")
outfiles = file.path("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21", outfiles)
#registers to the first image in files
preprocess_mri_within(files = files, retimg = FALSE, outfiles = outfiles, correction = "N4", skull_strip = FALSE)
#applying a brain mask to all registered images
#images from visit 1 are all in the same space as T1
#if we skull strip the T1 image then the mask can be applied to all the other images to extract brain tissue
brain = fslbet_robust(img = outfiles[1], correct = FALSE, verbose = FALSE) #T1 weighted image that was inhomegeneity corrected
mask = brain > 0
masked_imgs = lapply(outfiles, fslmask, mask = mask, verbose = FALSE)
#masked_imgs is list of nifti objects
orthographic(masked_imgs[[2]])
#within visit 2 n4 correction and co-registration
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_2/113")
files2 = c("113-02-MPRAGE.nii.gz", "113-02-T2w.nii.gz", "113-02-FLAIR.nii.gz")
files2 = file.path("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21", files2)
outfiles2 = c("113-02-MPRAGE_processed.nii.gz", "113-02-T2w_processed.nii.gz", "113-02-FLAIR_processed.nii.gz")
outfiles2 = file.path("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21", outfiles2)
preprocess_mri_within(files = files2, retimg = FALSE, outfiles = outfiles2, correction = "N4", skull_strip = FALSE)
brain2 = fslbet_robust(img = outfiles2[1], correct = FALSE, verbose = FALSE)
mask2 = brain2 > 0
masked_imgs2 = lapply(outfiles2, fslmask, mask = mask2, verbose = FALSE)
orthographic(masked_imgs2[[2]])

#across visit co registration of T1 images
#in previous code, moved first visit flair and T2 to space of first visit T1
#also moved second visit flair and T2 to space of second visit T1 image
#in this one, move second visit T1 into space of first visit T1, then use that for T2 and flair
#everything will be in the first visit T1 when done
visit2_files = file.path("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21", c("113-02-MPRAGE_processed.nii.gz", "113-02-T2w_processed.nii.gz", "113-02-FLAIR_processed.nii.gz"))
outfiles2 = sub(".nii.gz", "_reg.nii.gz", visit2_files)
ants_regwrite(filename = masked_imgs2[[1]], retimg = FALSE, outfile = outfiles2[1], template.file = masked_imgs[[1]], other.files = masked_imgs2[2:3], other.outfiles = outfiles2[2:3], typeofTransform = "Rigid", verbose = FALSE)
#within subjects so rigid is appropriate
ss_t1 = masked_imgs[[1]]
visit_2_t1 = readNIfTI(outfiles2[1], reorient = FALSE)
double_ortho(ss_t1, visit_2_t1)
ortho2(ss_t1, visit_2_t1, col.y = alpha(hotmetal(), 0.25))
#leaving skull on
visit2_files_skull = file.path("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21", c("113-02-MPRAGE_processed.nii.gz", "113-02-T2w_processed.nii.gz", "113-02-FLAIR_processed.nii.gz"))
outfiles2_skull = sub(".nii.gz", "_reg.nii.gz", visit2_files_skull)
ants_regwrite(filename = visit2_files_skull[[1]], retimg = FALSE, outfile = outfiles2_skull[1], template.file = outfiles[[1]], other.files = visit2_files_skull[2:3], other.outfiles = outfiles2_skull[2:3], typeofTransform = "Rigid", verbose = FALSE)
t1_skull = readNIfTI(outfiles[1], reorient = FALSE)
v2_t1_skull = readNIfTI(outfiles2_skull[1], reorient = FALSE)
double_ortho(t1_skull, v2_t1_skull)
ortho2(t1_skull, v2_t1_skull, col.y = alpha(hotmetal(), 0.25))

#roi = region of interest
#registration to a template