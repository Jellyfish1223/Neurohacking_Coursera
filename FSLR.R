Sys.getenv("FSLDIR")
library(fslr)
have.fsl()
#if = false then options(fsl.path = "usr/local/fsl")
library(oro.nifti)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_1/113")
nim = readNIfTI("113-01-MPRAGE.nii.gz", reorient = FALSE)
#t1_fname = "Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/kirby21/visit_1/113/113-01-MPRAGE.nii.gz"
#nim = readNIfTI(t1_fname, reorient = FALSE)
mean(nim)
fslstats(nim, opts = "-m")

#bias field correction (inhomogeneity correction)
fast_img = fsl_biascorrect(nim, retimg = TRUE)
#find difference between images
sub.bias = niftiarr(nim, nim-fast_img)
#create histograms
slices = c(2, 6, 10, 14, 18)
vals = lapply(slices, function(x) {cbind(img = c(nim[,,x]), fast = c(fast_img[,,x]), slice = x)})
vals = do.call("rbind", vals)
vals = data.frame(vals)
vals = vals[vals$img >0 &vals$fast >0,]
colnames(vals)[1:2] = c("Original Value", "Bias-Corrected Value")
v = melt(vals, id.vars = "slice")
g = ggplot(aes(x=value, colour = factor(slice)), data = v) + geom_line(stat = "density") + facet_wrap(~variable)
g = g + scale_colour_discrete(name = "Slice #")

#brain extraction
bet_fast = fslbet(infile = nim, retimg = TRUE)
bet_fast_mask <- niftiarr(bet_fast, 1)
is_in_mask = bet_fast >0
#all parts that are not in new image are NA
bet_fast_mask[!is_in_mask] <- NA
orthographic(bet_fast)
orthographic(fast_img, bet_fast_mask)
#produce better segmentation with new center of gravity (cog)
cog = cog(bet_fast, ceil = TRUE)
cog = paste("-c", paste(cog, collapse = ""))
bet_fast2 = fslbet(infile = fast_img, retimg = TRUE, opts = "-c 88 140 129")

#linear registration with flirt 
#rigid (df6) or affine(df12)
setwd("~/Desktop/Coursera_R/Neurohacking/Neurohacking_data-0.0/Template")
template <- readNIfTI("MNI152_T1_1mm_brain.nii.gz", reorient = FALSE)
registered_fast = flirt(infile=bet_fast2, reffile =template, dof = 6,retimg = TRUE) 
orthographic(template)
orthographic(registered_fast)
reg_fast_affine = flirt(infile=bet_fast2, reffile =template, dof = 12,retimg = TRUE)
orthographic(reg_fast_affine)

#non-linear registration with fnirt
#must do affine registration first, r combines into one function
fnirt_fast = fnirt_with_affine(infile=bet_fast2, reffile = template, outfile = "FNIRT_to_Template", retimg=TRUE) 
orthographic(fnirt_fast)
