simulate_boolean <- function(x, y, n = NULL, nd = NULL, R, res) {
  # x, y = width and height of the result (in meters, not pixels)
  # n = number of points, only parameter of the underlying poisson process
  # R = radius of the discs, corresponds to the parameter R of the Boolean model 
  # nd =  point density per hectare, alternative parameterization of n to ensure corresponding x-axis among different area sizes
  # res = size of a pixel in meter, i.e. which distance in the real world (in meters) corresponds to the edge length of a pixel (e.g. 1, 2.5, ...)
  
  
  if(is.null(n) & is.null(nd)) {
    stop("n and nd can't be NULL at the same time! Specify at least on of them!")
  }
  
  if((! is.null(n)) & (! is.null(nd))) {
    stop("n or nd needs to be NULL! They can't be specified at the same time!")
  }
  
  if(is.null(n)) {
    nd <- nd/10000 # convert hectare to m^2
    n <- round(nd * x * y)
  }
  
  X <- round(x/res)
  Y <- round(y/res)
  spatstat.options(npixel=c(nx = X, ny = Y))
  pois <- runifpoint(n, win=owin(c(0,x),c(0,y)))
  bool <- discs(pois, radii = R, npoly=16, mask = TRUE)
  
  return(bool)
}

plot_CI <- function(mean_var, q1_var, q2_var, n_var, R_var, value_2015, value_2006, n_lim = c(50, 500),  Ylab = "", Ylim = NULL, header = NULL, XLAB = "", set_par = TRUE, v_lines = FALSE, plot_value = TRUE, plot_legend = TRUE, plot_dashed = FALSE, PDF_file = NULL, pdf_size = c(6,6), MAR = NULL) {
  
  n_lim <- c(min(n_lim - 20), max(n_lim + 20))
  
  if(is.null(Ylim)){
    Ylim <- c(min(q1_var, value_2015, value_2006), max(q2_var, value_2015, value_2006))
  }
  
  if(set_par == TRUE) {
    set_par(mfrow = c(1, length(unique(R_var))))
  }
  
  jj <- 0
  
  for (j in sort(unique(R_var))){
    
    jj <- jj +1
    
    mean_temp <- mean_var[R_var == j]
    q1_temp <- q1_var[R_var == j]
    q2_temp <- q2_var[R_var == j]
    density_temp <- n_var[R_var == j]
    
    # XLAB <- "Intensity per hectare"
    
    if(! is.null(PDF_file)){
      
      PDFs <- rep(PDF_file, length(unique(R_var)))
      
      pdf_file <-  PDFs[jj] %>% paste(., "R", sep = "_") %>% paste(., j, sep = "") %>% paste(., "pdf", sep = ".")
      
      
      pdf(file = pdf_file, width = pdf_size[1], height = pdf_size[2])
      
      
      if(! is.null(MAR)){
        par(mar = MAR)
      }
      
    }
    
    
    
    plot(1, 1, 
         xlim = n_lim, 
         type = "n", 
         xlab = XLAB, 
         ylab = Ylab,
         ylim = Ylim,
         las = 1,
         cex.axis= 1.4)
    
    if (v_lines == TRUE) {
      abline(v = seq(0,1200,100), col ="grey80")
    }
    
    for (i in 1:length(unique(density_temp))){
      segments(x0 = density_temp[i],     x1 = density_temp[i],     y0 = q1_temp[i], y1 = q2_temp[i])
      segments(x0 = density_temp[i] - 5, x1 = density_temp[i] + 5,   y0 = q1_temp[i],   y1 = q1_temp[i])
      segments(x0 = density_temp[i] - 5, x1 = density_temp[i] + 5,   y0 = q2_temp[i],   y1 = q2_temp[i])
    }
    
    points(density_temp, mean_temp, pch = 19, cex = 1)
    
    
    if (plot_value == TRUE) {
      abline(h = value_2015, col = "red")
      abline(h = value_2006, col = "blue")
    }
    
    if (plot_dashed == TRUE) {
      
      dashed_value <- mean(value_2006, value_2015)
      
      abline(h = dashed_value, col = "blue")
      abline(h = dashed_value, col = "red", lty = "ff")
    }
    
    
    if (plot_legend == TRUE) {
      legend("topright", legend =  c(paste(paste("Boolean model (R =", j), ")"), "2015", "2006"), col = c("black", "red", "blue"), lty = 1, pch = c(19, NA, NA), ncol = 3)
    }
    
    if(! is.null(PDF_file)){
      dev.off()
    }
    
  }
  
  if(! is.null(header)){
    title(header, line = -2, outer = TRUE, cex.main = 2)
  }
  
  
  
}


density_envelope <- function(metric, R, point_density_index, path = "", name = "boolean_simu", iterations = 100, log = FALSE, verbose = TRUE, bandwidth, res = 2.5, area = c(161, 235), xlim, Kernel = "gaussian", output) {

  return_short <- list()
  return_long <- list()
  
  area_size <- area * res
  
  if(verbose == TRUE){
    paste("Area size:", area_size) %>% print()
    paste("Area:", area) %>% print()
  }
  
  for(j in point_density_index[1]:point_density_index[2]){
    
    # if(verbose == TRUE){
    #   print(j)
    # }
    
    mypath <- path %>% paste(name, sep = "") %>% paste(R, sep = "_") %>% paste(j, sep = "_")  %>% paste(".Rdata", sep = "")
    load(mypath)
    
    index <- (length(Results) + 1 - iterations):length(Results)
    
    Dim <- dim(Results[[index[1]]]) 
    
    x_area <- c(floor((Dim[1] - area[2])/2), floor((Dim[1] - area[2])/2) + area[2] - 1)
    y_area <- c(floor((Dim[2] - area[1])/2), floor((Dim[2] - area[1])/2) + area[1] - 1) 
    
    if(verbose == TRUE){
      
      if(j == point_density_index[1]){
        paste("Chosen pixels (x-Axis):", x_area) %>% print()
        paste("Chosen pixels (y-Axis):", y_area) %>% print()
        
        par(mfrow = c(1,1))
        Results[[index[1]]] %>% raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% plot()
        
      }
      
      print(j)
    }
    
    
    evaluations <- Results[[index[1]]] %>% 
      raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
      metric() %>% 
      filter(class == 0) %>%
      .$value %>% 
      round(5) %>%
      na.omit() %>%
      logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) %>% .$x %>% length()
    
    dens_mat <- matrix(NA, nrow = iterations, ncol = evaluations)
    
    colnames(dens_mat) <- Results[[index[1]]] %>% 
      raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
      metric() %>% 
      filter(class == 0) %>%
      .$value %>% 
      round(5) %>%
      na.omit() %>%
      logdensity(.,kernel = Kernel ,  bw = bandwidth, from = xlim[1], to = xlim[2]) %>% .$x %>% as.character()
    
    
    for (i in index){
      
      resu <- Results[[i]]
      
      resu <- resu[x_area[1]:x_area[2], y_area[1]:y_area[2]]
      
      if(log == TRUE){

        md <- resu %>% 
        raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
        metric() %>% 
        filter(class == 0) %>%
        .$value %>% 
        round(5) %>% 
        na.omit() %>%
        logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2])
      }
      
      if(log == FALSE){
        md <- resu %>% 
          raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
          metric() %>% 
          filter(class == 0) %>%
          .$value %>% 
          round(5) %>%
          na.omit() %>%
          density(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2])
      }
      
      if(any(as.character(md$x) != colnames(dens_mat))) {
        stop(paste("Error at i = ", i))
      }
      
      dens_mat[i - (max(index) - iterations), ] <- md$y
      
    }
    
    plot_mat <- matrix(NA, nrow = evaluations, ncol = 4)
    colnames(plot_mat) <- c("mean", "q1", "q2", "x")
    
    for (i in 1:evaluations){
      plot_mat[i,1] <- mean(dens_mat[,i], na.rm = TRUE)
      plot_mat[i,2] <- quantile(dens_mat[,i], 0.025, na.rm = TRUE)
      plot_mat[i,3] <- quantile(dens_mat[,i], 0.975, na.rm = TRUE)
    }
    
    plot_mat[,4] <- as.numeric(colnames(dens_mat))
    
    iter_list_short <- list()
    iter_list_short[[1]] <- R
    iter_list_short[[2]] <- j
    iter_list_short[[3]] <- plot_mat
    names(iter_list_short) <- c("R", "point_density_index", "plot_mat")
    
    return_short[[j]] <- iter_list_short
    
    iter_list_long <- list()
    iter_list_long[[1]] <- R
    iter_list_long[[2]] <- j
    iter_list_long[[3]] <- dens_mat
    names(iter_list_long) <- c("R", "point_density_index", "dens_mat")
    
    return_long[[j]] <- iter_list_long
    
  } 
  
  if(output == "short"){
    return(return_short)
  }
  
  if(output == "long"){
    return(return_long)
  }
  
  if(output == "both"){
    return_both <- list(return_short, return_long)
    names(return_both) <- c("short", "long")
    
    return(return_both)
  }
  
  warning("Output argument was not correctly specified! Returning both output formats.")
  return_both <- list(return_short, return_long)
  names(return_both) <- c("short", "long")
  
  return(return_both)
  
}

plot_envelope <- function(envelope_data, metric, R, intensity_index, log = FALSE, bandwidth, xlim, ylim = NULL ,xlab = "", data_2006, data_2015, mfrow  = NULL, Kernel = "gaussian", plot_legend = TRUE, PDF_file = NULL, pdf_size = c(6,6), MAR = NULL, xlim_plot = xlim) {
  
    if(! is.null(mfrow)) {
      par(mfrow = mfrow)
    }  
  
  jj <-0 
  
  for(j in intensity_index){
    
    jj <- jj + 1
    
    plot_mat <- envelope_data[[j]]$plot_mat 
    
    X <- plot_mat[,4]
    
    if(log == TRUE){
      den06 <- data_2006 %>% metric() %>% 
        filter(class == 0) %>%
        .$value %>% 
        round(5) %>%
        logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) 
      
      den15 <- data_2015 %>% metric() %>% 
        filter(class == 0) %>%
        .$value %>% 
        round(5) %>%
        logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) 
    }
    
    if(log == FALSE){
      den06 <- data_2006 %>% metric() %>% 
        filter(class == 0) %>%
        .$value %>% 
        round(5) %>%
        density(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2])
      
      den15 <- data_2015 %>% metric() %>% 
        filter(class == 0) %>%
        .$value %>% 
        round(5) %>%
        density(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) 
    }
    
    max_y <- max(c(den06$y, den15$y,  plot_mat[,3]))    
    
    if(is.null(ylim)) {
      YLIM = c(0, max_y)
    } else {
      YLIM = ylim
    }
    
    if(! is.null(PDF_file)){
      
      PDFs <- rep(PDF_file, length(unique(intensity_index)))
      
      pdf_file <-  PDFs[jj] %>% paste(., j, sep = "_") %>% paste(., "pdf", sep = ".")
      
      
      pdf(file = pdf_file, width = pdf_size[1], height = pdf_size[2])
      
      
      if(! is.null(MAR)){
        par(mar = MAR)
      }
      
    }
    
    plot(x = X, y = plot_mat[,3], type = "n", xlab = xlab, ylab = "", xlim = xlim_plot , ylim = YLIM,
         las = 1,
         cex.axis= 1.4)
    
    polygon(c(X,rev(X)),c(plot_mat[,2],rev(plot_mat[,3])),col = "grey80", border = FALSE)
    lines(x = X, y = plot_mat[,1], type = "l")
    
    den06 %>% lines(., col = "blue")
    den15 %>% lines(., col = "red")
    
    if(plot_legend == TRUE) {
      leg <- "Boolean model (R = " %>% paste(R, sep = "") %>% paste(", d = ", sep = "") %>% paste(j*50, sep = "") %>% paste(")", sep = "")
      legend("topright", legend =  c(leg, "2015", "2006"), col = c("black", "red", "blue"), lty = 1) #, fill = c("grey80", NA, NA), border = NA)
    } 
    
    if(! is.null(PDF_file)){
      dev.off()
    }
    
  }
}



GET <- function(dens_mat, obs, GET_type = "erl",plot = FALSE){
  
  #r <- as.numeric(colnames(dens_mat))
  sim <- t(dens_mat)
  sim <- sim[2:nrow(sim),]
  r <- as.numeric(row.names(sim))
  cs <- create_curve_set(list(r = r, obs = obs, sim_m = sim))
  
  res <- global_envelope_test(cs, type=GET_type)
  
  if(plot== TRUE){
    plot(res)
  }
  
  return(res)
  
}



p_matrix_GET <- function(Rs, ns, outputs, obs, GET_type = "erl") {
  
  result <- matrix(NA, nrow = length(ns), ncol = length(Rs)) 
  
  rownames(result) <- ns
  colnames(result) <- Rs
  
  for (j in 1:length(Rs)) {
    
    output <- outputs[[j]]
    
    for (i in 1:length(ns)) {
      
      index <- ns[i]/50
      
      j %>% paste(.,index, sep = " , ") %>% print()
      
      #output[[index]]$dens_mat[1:3,1:3] %>% print()
      
      g <- GET(dens_mat = output[[index]]$dens_mat, obs = obs, GET_type = GET_type)
      
      result[i,j] <- attributes(g)$p
      
    }
    
  }
  
  return(result)
  
}

plot_density_envelope <- function(metric, R, point_density_index, path = "", name = "boolean_simu", iterations = 2500, 
                                  verbose = TRUE, log = FALSE, bandwidth, res = 2.5, area = c(161, 235), xlim, ylim = NULL,xlab = "", data_2006, data_2015, mfrow, Kernel = "gaussian") {

  stop("This function is depreciated! Use density_envelope() and plot_envelope() instead!")
}

# plot_density_envelope <- function(metric, R, point_density_index, path = "", name = "boolean_simu", iterations = 2500, verbose = TRUE, log = FALSE, bandwidth, res = 2.5, area = c(161, 235), xlim, ylim = NULL,xlab = "", data_2006, data_2015, mfrow, Kernel = "gaussian") {
#   
#   par(mfrow = mfrow)
#   
#   area_size <- area * res
#   
#   for(j in point_density_index[1]:point_density_index[2]){
#   
#   if(verbose == TRUE){
#     print(j)
#   }
#   
#   mypath <- path %>% paste(name, sep = "") %>% paste(R, sep = "_") %>% paste(j, sep = "_")  %>% paste(".Rdata", sep = "")
#   load(mypath)
#   
#   index <- (length(Results) + 1 - iterations):length(Results)
#   
#   Dim <- dim(Results[[index[1]]]) 
#   
#   x_area <- c(floor((Dim[1] - area[2])/2), floor((Dim[1] - area[2])/2) + area[2] - 1)
#   y_area <- c(floor((Dim[2] - area[1])/2), floor((Dim[2] - area[1])/2) + area[1] - 1) 
#   
#   evaluations <- Results[[index[1]]] %>% 
#     raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
#     metric() %>% 
#     filter(class == 0) %>%
#     .$value %>% 
#     round(5) %>%
#     logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) %>% .$x %>% length()
#   
#   dens_mat <- matrix(NA, nrow = iterations, ncol = evaluations)
#   
#   colnames(dens_mat) <- Results[[index[1]]] %>% 
#     raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
#     metric() %>% 
#     filter(class == 0) %>%
#     .$value %>% 
#     round(5) %>%
#     logdensity(.,kernel = Kernel ,  bw = bandwidth, from = xlim[1], to = xlim[2]) %>% .$x %>% as.character()
#   
#   
#   for (i in index){
#     
#     resu <- Results[[i]]
#     
#     resu <- resu[x_area[1]:x_area[2], y_area[1]:y_area[2]]
#     
#     if(log == TRUE){
# 
#     md <-resu %>% 
#       raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
#       metric() %>% 
#       filter(class == 0) %>%
#       .$value %>% 
#       round(5) %>%
#       logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2])
#     }
#     
#     if(log == FALSE){
#       md <- resu %>% 
#         raster(., xmn = 1, xmx	= area_size[1], ymn = 1, ymx = area_size[2]) %>% 
#         metric() %>% 
#         filter(class == 0) %>%
#         .$value %>% 
#         round(5) %>%
#         density(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2])
#     }
#     
#     if(any(as.character(md$x) != colnames(dens_mat))) {
#       stop(paste("Error at i = ", i))
#     }
#     
#     dens_mat[i - (max(index) - iterations), ] <- md$y
#     
#   }
#   
#   plot_mat <- matrix(NA, nrow = evaluations, ncol = 3)
#   names(plot_mat) <- c("mean", "q1", "q2")
#   
#   for (i in 1:evaluations){
#     plot_mat[i,1] <- mean(dens_mat[,i])
#     plot_mat[i,2] <- quantile(dens_mat[,i], 0.025)
#     plot_mat[i,3] <- quantile(dens_mat[,i], 0.975)
#   }
#   
#   X <- as.numeric(colnames(dens_mat))
#   
#   plot(x = X, y = plot_mat[,3], type = "l", xlab = xlab, ylab = "Density", xlim = xlim , ylim = ylim) #)= min(c(max(c(plot_mat[,1], plot_mat[,2], plot_mat[,3]))),1))
#   
#   polygon(c(X ,rev(X)),c(plot_mat[,2],rev(plot_mat[,3])),col = "grey80", border = FALSE)
#   lines(x = X, y = plot_mat[,1], type = "l")
#   
#   if(log == TRUE){
#   data_2006 %>% metric() %>% 
#     filter(class == 0) %>%
#     .$value %>% 
#     round(5) %>%
#     logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) %>% lines(., col = "green")
#   
#   data_2015 %>% metric() %>% 
#     filter(class == 0) %>%
#     .$value %>% 
#     round(5) %>%
#     logdensity(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) %>% lines(., col = "red")
#   }
#   
#   if(log == FALSE){
#     data_2006 %>% metric() %>% 
#       filter(class == 0) %>%
#       .$value %>% 
#       round(5) %>%
#       density(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) %>% lines(., col = "green")
#     
#     data_2015 %>% metric() %>% 
#       filter(class == 0) %>%
#       .$value %>% 
#       round(5) %>%
#       density(., kernel = Kernel, bw = bandwidth, from = xlim[1], to = xlim[2]) %>% lines(., col = "red")
#   }
#   
#   leg <- "Boolean model (R = " %>% paste(R, sep = "") %>% paste(", d = ", sep = "") %>% paste(j*50, sep = "") %>% paste(")", sep = "")
#   
#   legend("topright", legend =  c(leg, "2015", "2006"), col = c("black", "red", "green"), lty = 1) #, fill = c("grey80", NA, NA), border = NA)
#   } 
# }




