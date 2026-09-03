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
fresh_spec  = readRDS("Data/Processed/spectra_fresh.RDS")

######################
# Process spectra
######################
# Remove Narcissus pseudonarcissus
fresh_spec = lapply(fresh_spec, function(x){
  x[x$Species !="Narcissus pseudonarcissus", ] 
})

# Select leaf developmental stages: early (young), peak (mature), and late (old/senescent)
x_e   = as_spectra(fresh_spec$Week_01, name_idx = 5, meta_idxs = c(1,2,3,4))
x_p   = as_spectra(fresh_spec$Week_11, name_idx = 5, meta_idxs = c(1,2,3,4))
x_l   = as_spectra(fresh_spec$Week_23, name_idx = 5, meta_idxs = c(1,2,3,4))

# Spectral Normalization
# x_e = normalize(x_e)
# x_p = normalize(x_p)
# x_l = normalize(x_l)


common = intersect(names(x_e), names(x_p))
common = intersect(common, names(x_l))
band_keep = seq(400, 2400, 5)

x_e = x_e[common , band_keep]
x_p = x_p[common , band_keep]
x_l = x_l[common , band_keep]


mat_x_e = as.matrix(x_e)
mat_x_p = as.matrix(x_p)
mat_x_l = as.matrix(x_l)

###########################
# Correlation matrices
###########################
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

#Are elements in corr A outside of the bounds of corr B
corr_p_within_cor_e = cor_x_p < corr_test_e$lowCI | cor_x_p > corr_test_e$uppCI
corr_l_within_cor_p = cor_x_l < corr_test_p$lowCI | cor_x_l > corr_test_p$uppCI

# Squared diff
cor_x_e_minus_p = (cor_x_e - cor_x_p)^2
cor_x_p_minus_l = (cor_x_p - cor_x_l)^2

################################
# Plot function for spectra
################################
f = function(R, upp_tri = FALSE, n_axis = 6, cols = NULL, breaks = NULL){
  
  if(is.null(cols)){
    cc   = RColorBrewer::brewer.pal(5, 'RdBu')
    cc[3] = "white"
    cols = colorRampPalette(cc)(100)
  }
  
  if(is.null(breaks)){
    breaks = seq(-1, 1, length.out = length(cols) + 1)
  }
  
  cols[which(breaks == 0)] = "white"
  
  idx    = findInterval(R, breaks, all.inside = TRUE)
  cell_cols = matrix(cols[idx], nrow(R))
  
  
  if(upp_tri){ R = R * upper.tri(R) }else{1}
  image(1:nrow(R),
        1:ncol(R),
        t(R[nrow(R):1, ]) ,
        col = cols,
        breaks = breaks,
        axes = FALSE, ylab = "bands", xlab = "bands"
  )
  
  at = seq(1, ncol(R), length.out = n_axis)
  l  = round(as.numeric(colnames(R)[at]), digits = -1)
  
  axis(3, at = at, labels = l, las = 2, tick = F)
  axis(2, at = rev(at), labels = l, las = 2, tick = F)
}


par(mfrow = c(2, 3))


f(cor_x_e * corr_sig_e, upp_tri = T) # , main = "Young"
f(cor_x_p * corr_sig_p, upp_tri = T) # , main = "Mature"
f(cor_x_l * corr_sig_l, upp_tri = T) # , main = "Old/Senescent"


col = c("white", colorRampPalette(brewer.pal(5, 'Purples'))(30))
brk = seq(0, 1, length.out = length(col) + 1)

f( sqrt(cor_x_e_minus_p) * corr_p_within_cor_e, upp_tri = T, cols = col, breaks = brk)
f( sqrt(cor_x_p_minus_l) * corr_l_within_cor_p, upp_tri = T, cols = col, breaks = brk )



# col     = colorRampPalette(c("red", "white", "blue"))(100)
# col_lim = c(-1, 1)
#
# par(mfrow = c(1, 3))
# corrplot(corr = cor_x_p, method = "shade", "full", col = col,
#          col.lim = col_lim, cl.pos = "n", tl.pos= "n")
# corrplot(corr = cor_x_l, method = "shade", "full", col = col,
#          col.lim = col_lim,  cl.pos = "n", tl.pos= "n")
#




f = function(R, upp_tri = FALSE, n_axis = 6, cols = NULL, breaks = NULL, main = NULL,
             legend = TRUE){
  
  if(is.null(cols)){
    cc   = RColorBrewer::brewer.pal(5, 'RdBu')
    cc[3] = "white"
    cols = colorRampPalette(cc)(100)
  }
  
  if(is.null(breaks)){
    breaks = seq(-1, 1, length.out = length(cols) + 1)
  }
  
  cols[which(breaks == 0)] = "white"
  
  idx    = findInterval(R, breaks, all.inside = TRUE)
  cell_cols = matrix(cols[idx], nrow(R))
  
  
  if(upp_tri){ R = R * upper.tri(R) }else{1}
  image(1:nrow(R),
        1:ncol(R),
        t(R[nrow(R):1, ]) ,
        col = cols,
        breaks = breaks,
        axes = FALSE, ylab = "bands", xlab = "bands", main = main
  )
  
  at = seq(1, ncol(R), length.out = n_axis)
  l  = round(as.numeric(colnames(R)[at]), digits = -1)
  
  axis(3, at = at, labels = l, las = 2, tick = F)
  axis(2, at = rev(at), labels = l, las = 2, tick = F)
  
  # ## Add correlation values
  # if(show_values){
  #   
  #   nr <- nrow(plotR)
  #   
  #   for(i in 1:nr){
  #     for(j in 1:ncol(plotR)){
  #       
  #       if(!is.na(plotR[i,j])){
  #         
  #         text(
  #           j,
  #           nr - i + 1,
  #           labels = sprintf(paste0("%.", digits, "f"), plotR[i,j]),
  #           cex = 0.8
  #         )
  #         
  #       }
  #       
  #     }
  #   }
  # }
  
  ## Add color legend
  if(legend){
    
    fields::imagePlot(
      legend.only = TRUE,
      zlim = c(min(breaks), max(breaks)),
      col = cols,
      breaks = breaks,
      horizontal = FALSE,
      legend.mar = 5.1,
      legend.width = 1.2,
      legend.shrink = 0.8,
      axis.args = list(at = c(-1,-0.5,0,0.5,1))
    )
    
  }
}




#################
f = function(R, upp_tri = FALSE, n_axis = 6, cols = NULL, breaks = NULL,
             main = NULL, legend = TRUE){
  
  if(is.null(cols)){
    cc = RColorBrewer::brewer.pal(5, "RdBu")
    cc[3] = "white"
    cols = colorRampPalette(cc)(100)
  }
  
  if(is.null(breaks)){
    breaks = seq(-1, 1, length.out = length(cols) + 1)
  }
  
  cols[which(breaks == 0)] = "white"
  
  idx = findInterval(R, breaks, all.inside = TRUE)
  cell_cols = matrix(cols[idx], nrow(R))
  
  if(upp_tri) R = R * upper.tri(R)
  
  ## Reserve room for legend
  # if(legend){
  #   layout(matrix(c(1,2), nrow = 1), widths = c(5, 0.6))
  # }
  
  ## Correlation plot
  par(mar = c(4.5, 4.5, 3, 1))
  
  image(
    1:nrow(R),
    1:ncol(R),
    t(R[nrow(R):1, ]),
    col = cols,
    breaks = breaks,
    axes = FALSE,
    xlab = "bands",
    ylab = "bands",
    main = main,
    asp = 1
  )
  
  at = seq(1, ncol(R), length.out = n_axis)
  l  = round(as.numeric(colnames(R)[at]), digits = -1)
  
  axis(3, at = at, labels = l, las = 2, tick = FALSE)
  axis(2, at = rev(at), labels = l, las = 2, tick = FALSE)
 # box()
  
  ## Color legend in its own panel
  if(legend){
    
    par(mar = c(4.5, 1, 3, 4))
    
    fields::imagePlot(
      legend.only = TRUE,
      zlim = c(-1, 1),
      col = cols,
      breaks = breaks,
      horizontal = FALSE,
      legend.width = 1.3,
      legend.shrink = 0.95,
      axis.args = list(
        at = c(-1,-0.5,0,0.5,1),
        las = 1
      )
    )
    
#    layout(1)
  }
}



par(mfrow = c(2, 3),
    mar = c(4.5, 5, 2, 4), # 4,4,2,2
    oma = c(0, 0, 0, 2))


f(cor_x_e * corr_sig_e, upp_tri = T) # , main = "Young"
f(cor_x_p * corr_sig_p, upp_tri = T) # , main = "Mature"
f(cor_x_l * corr_sig_l, upp_tri = T) # , main = "Old/Senescent"


col = c("white", colorRampPalette(brewer.pal(5, 'Purples'))(30))
brk = seq(0, 1, length.out = length(col) + 1)

f( sqrt(cor_x_e_minus_p) * corr_p_within_cor_e, upp_tri = T, cols = col, breaks = brk)
f( sqrt(cor_x_p_minus_l) * corr_l_within_cor_p, upp_tri = T, cols = col, breaks = brk )
