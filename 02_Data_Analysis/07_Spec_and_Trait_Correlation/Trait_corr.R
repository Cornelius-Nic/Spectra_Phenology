################################################################################
# Packages
################################################################################
library("corrplot")
library("spectrolab")
library("viridis")
library("RColorBrewer")

################################################################################
# Import spectral data
################################################################################
measured_traits  = readRDS("Data/Processed/fresh_spec_traits.RDS")

########################
# Process spectra
########################
# Select leaf developmental stages: early (young), peak (mature), and late (old/senescent)
x_e   = measured_traits[measured_traits$Week == 1, ]
x_p   = measured_traits[measured_traits$Week == 11, ]
x_l   = measured_traits[measured_traits$Week == 23, ]

# Select traits
traits = as.character(c("LMA", "EWT", "N", "C", "QY"))

# Keep only trait columns
x_e = x_e[ , traits]
x_p = x_p[ , traits]
x_l = x_l[ , traits]

# Remove rows with missing values
x_e = na.omit(x_e)
x_p = na.omit(x_p)
x_l = na.omit(x_l)

# Convert to numeric matrices
mat_x_e = as.matrix(x_e)
mat_x_p = as.matrix(x_p)
mat_x_l = as.matrix(x_l)

#########################
# Correlations
#########################
cor_x_e = cor(mat_x_e)
cor_x_p = cor(mat_x_p)
cor_x_l = cor(mat_x_l)


corr_test_e = cor.mtest(mat_x_e)
corr_test_p = cor.mtest(mat_x_p)
corr_test_l = cor.mtest(mat_x_l)

# 0 = corr NOT sig. 1 corr IS sig
corr_sig_e = corr_test_e$p < 0.05
corr_sig_p = corr_test_p$p < 0.05
corr_sig_l = corr_test_l$p < 0.05

##############################
# Compare Correlations
##############################
#Are elements in corr A outside of the bounds of corr B
corr_p_within_cor_e = cor_x_p < corr_test_e$lowCI | cor_x_p > corr_test_e$uppCI
corr_l_within_cor_p = cor_x_l < corr_test_p$lowCI | cor_x_l > corr_test_p$uppCI

# Squared diff
cor_x_e_minus_p = (cor_x_e - cor_x_p)^2
cor_x_p_minus_l = (cor_x_p - cor_x_l)^2

##############################
# Plot function for traits
##############################

f = function(R, upp_tri = FALSE, cols = NULL, breaks = NULL, main = NULL,
             show_values = TRUE, legend = TRUE, digits = 2){
  
  if(is.null(cols)){
    cc = RColorBrewer::brewer.pal(5, "RdBu")
    cc[3] = "white"
    cols = colorRampPalette(cc)(100)
  }
  
  if(is.null(breaks)){
    breaks = seq(-1, 1, length.out = length(cols) + 1)
  }
  
  plotR <- R
  
  if(upp_tri)
    plotR[lower.tri(plotR)] <- NA
  
  image(
    x = seq(0.5, ncol(plotR) + 0.5, by = 1),
    y = seq(0.5, nrow(plotR) + 0.5, by = 1),
    z = t(plotR[nrow(plotR):1, ]),
    col = cols,
    breaks = breaks,
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1,
    xlim = c(0.5, ncol(plotR) + 0.5),
    ylim = c(0.5, nrow(plotR) + 0.5),
    useRaster = TRUE
  )
  
  at = seq_len(ncol(plotR))
  labs = colnames(plotR)
  
  axis(3, at = at, labels = labs, las = 2, tick = FALSE)
  axis(2, at = at, labels = rev(labs), las = 2, tick = FALSE)
  
#  box()
  
  ## Add correlation values
  if(show_values){
    
    nr <- nrow(plotR)
    
    for(i in 1:nr){
      for(j in 1:ncol(plotR)){
        
        if(!is.na(plotR[i,j])){
          
          text(
            j,
            nr - i + 1,
            labels = sprintf(paste0("%.", digits, "f"), plotR[i,j]),
            cex = 0.8
          )
          
        }
        
      }
    }
  }
  
  ## Add color legend
  if(legend){
    
    fields::image.plot(
      legend.only = TRUE,
      zlim = c(min(breaks), max(breaks)),
      col = cols,
      breaks = breaks,
      # legend.width = 1.2,
      # legend.shrink = 0.8,
      axis.args = list(at = c(-1,-0.5,0,0.5,1))
    )
    
  }
  
}


# plot panel
par(mfrow = c(2,3))

f(cor_x_e * corr_sig_e, upp_tri = T) # , main = "Young"
f(cor_x_p * corr_sig_p, upp_tri = T) # , main = "Mature"
f(cor_x_l * corr_sig_l, upp_tri = T) # , main = "Old/Senescent"


col = c("white", colorRampPalette(brewer.pal(5, 'Purples'))(30))
brk = seq(0, 1, length.out = length(col) + 1)


f( sqrt(cor_x_e_minus_p) * corr_p_within_cor_e, upp_tri = T, cols = col, breaks = brk)
f( sqrt(cor_x_p_minus_l) * corr_l_within_cor_p, upp_tri = T, cols = col, breaks = brk )

# Save plots
pdf("Figs/Correlation_plots/Trait_Correlation_plot.pdf", width = 14, height = 8)
png("Figs/Correlation_plots/Trait_Correlation_plot.png", width = 4200, height = 2400, res = 600)


# col     = colorRampPalette(c("red", "white", "blue"))(100)
# col_lim = c(-1, 1)
#
# par(mfrow = c(1, 3))
# corrplot(corr = cor_x_p, method = "shade", "full", col = col,
#          col.lim = col_lim, cl.pos = "n", tl.pos= "n")
# corrplot(corr = cor_x_l, method = "shade", "full", col = col,
#          col.lim = col_lim,  cl.pos = "n", tl.pos= "n")
#
