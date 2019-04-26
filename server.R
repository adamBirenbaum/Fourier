server <- function(input,output){
  
  convert_string_to_latex <- function(x,f){
    #x <- "exp(-x)*cos(x)*sin*(3*x)*sqrt(2)"  
    x <- gsub("exp\\(([^)]+)\\)","e^{\\1}",x) 
    x <- gsub("*","",x,fixed = T)
    x <- paste0(f,"(x) = ",x)
    x
  }
  
  make_function <- function(s,L,n){
    
    function(x) eval(parse(text = s))
  }
  
  discrete_integ <- function(f,lb,up,L,n,constant = F){
    
    if (constant){
      a <- mapply(function(f,lb,up) (up - lb)*as.numeric(f),f,lb,up,USE.NAMES = F)
      
    }else{
      
      a <- mapply(FUN = function(f,lb,up,n,L) integrate(make_function(f,L,n),lower = lb,upper = up)$value,f,lb,up,n,L)
      
    }
    
    
    sum(a)
    
  }
  
  integ <- function(f,lb,up,L,n){
    integrate(make_function(f,L,n),lower = lb,upper = up)$value
    
  }
  
  
  
  
  get_fourier_coef <- function(f,lb,up,n,constant){
    L <- (max(up)-min(lb)) / 2
    
    
    
    an <- seq_len(n)
    bn <- seq_len(n)
    
    a_integrand <- paste0(f,"*cos(n*pi/L*x)")
    b_integrand <- paste0(f,"*sin(n*pi/L*x)")
    
    if (length(f) > 1){
      
      a0 <- 1/(2*L)*discrete_integ(f,lb,up,L,n,constant)
      an <- 1/L*sapply(X = 1:n,function(x) discrete_integ(a_integrand,lb,up,L,x))
      
      bn <- 1/L*sapply(X = 1:n,function(x) discrete_integ(b_integrand,lb,up,L,x))
    }else{
      a0 <- 1/(2*L)*integ(f,lb,up,L,n)
      an <- 1/L*sapply(X = 1:n,function(n) integ(a_integrand,lb,up,L,n))
      bn <- 1/L*sapply(X = 1:n,function(n) integ(b_integrand,lb,up,L,n))
    }
    
    list(a0 = a0, an = an,bn = bn)
  }
  
  get_fourier_points <- function(f,lb,up,n,constant = F,expand_plot = 1){
    
    coef_list <- get_fourier_coef(f,lb,up,n,constant)
    L <- (max(up)-min(lb)) / 2
    n <- 1:n
    
    A <- paste0(coef_list$an,"*cos(",n*pi/L,"*x)",collapse = "+")
    B <- paste0(coef_list$bn,"*sin(",n*pi/L,"*x)",collapse = "+")
    equation_string <- paste0(coef_list$a0,"+",A,"+",B,collapse = "+")

    x <- seq(from = min(lb)*expand_plot,to = max(up)*expand_plot,length.out = 500)
    y <- eval(parse(text = equation_string))  
    data.frame(x = x, y=y)
  }
  
  animate_fourier <- function(f,lb,up,n,constant = F){
    coef_list <- get_fourier_coef(f,lb,up,n,constant)
    L <- (max(up)-min(lb)) / 2
    n <- 1:n
    
    N <- 100
    x <- seq(from = min(lb),to = max(up),length.out = N)
    R <- sqrt(coef_list$an^2+coef_list$bn^2)
    tt <- seq(from =0, to = 2, length.out = N)
    
    freq <- n
    
    #A <- paste0(coef_list$an,"*cos(",n*pi/L,"*x)")
    #B <- paste0(coef_list$bn,"*sin(",n*pi/L,"*x)")
    #equation_string <- paste0(A,"+",B)
    
    # for(i in n){
    #   if (i == 1) next()
    #       equation_string[i] <- paste0(equation_string[i-1],"+",equation_string[i])
    #     }
    # 
    # equation_string <- paste0(coef_list$a0,"+",equation_string)
    
    termi <- 0
    for(i in n){
      if (R[i] < 1E-10){
        print(i)
        next()
      }else{
        termi <- termi + 1
      } 
      
      if (termi == 1){
        df <- data.frame(x =0 ,y=0,xend = R[1]*cos(2*pi*tt), yend = R[1]*sin(2*pi*tt),time = 1:N,grp = 1,lin = 1)
      }
      ii <- N*(termi-1)+ 1
      ee <- ii + N -1
      df2 <- data.frame(x = df$xend[ii:ee],y = df$yend[ii:ee],xend = df$xend[ii:ee] + R[i]*cos(2*pi*tt*i), yend = df$yend[ii:ee] + R[i]*sin(2*pi*tt*i),time = 1:N,grp = i,lin = 1)
      df <- rbind(df,df2)
    }
    
    ii <- N*(termi)+ 1
    ee <- ii + N -1
    maxx <- max(df$xend)
    df2 <- data.frame(x = maxx*2 ,y = df$yend[ii:ee],xend = df$xend[ii:ee] , yend = df$yend[ii:ee],time = 1:N,grp = termi+1,lin = 2)
    df <- rbind(df,df2)
    df
  }
  
  animate_fourier2 <- function(f,lb,up,n,constant = F){
    coef_list <- get_fourier_coef(f,lb,up,n,constant)
    L <- (max(up)-min(lb)) / 2
    n <- 1:n

    N <- 100
    x <- seq(from = min(lb),to = max(up),length.out = N)
    An <- coef_list$an
    Bn <- coef_list$bn
    tt <- seq(from =0, to = 2, length.out = N)
    
    freq <- n
    
    #A <- paste0(coef_list$an,"*cos(",n*pi/L,"*x)")
    #B <- paste0(coef_list$bn,"*sin(",n*pi/L,"*x)")
    #equation_string <- paste0(A,"+",B)
    
    # for(i in n){
    #   if (i == 1) next()
    #       equation_string[i] <- paste0(equation_string[i-1],"+",equation_string[i])
    #     }
    # 
    # equation_string <- paste0(coef_list$a0,"+",equation_string)
    
    termi <- 0
    first_term <- T
    for(i in n){
      if (An[i] < 1E-10 && Bn[i] < 1E-10){
       
        next()
      }
        if (first_term){
          first_term <- F
          An_bool <- F
          if (An[i] > 1E-10){
            df1 <- data.frame(x =0 ,y=0,xend = An[1]*cos(2*pi*tt), yend = An[1]*sin(2*pi*tt),time = 1:N,grp = 1,lin = 1)
            An_bool <- T
            termi <- termi + 1
          }
          
          
          if (Bn[i] > 1E-10){
            if (An_bool){
              df2 <- data.frame(x =df1$xend[1:N] ,y=df1$yend[1:N],xend = df1$xend[1:N] + Bn[1]*cos(2*pi*tt), yend = df1$yend[1:N] +Bn[1]*sin(2*pi*tt),time = 1:N,grp = 2,lin = 1)
              df <- rbind(df1,df2)
              termi <- termi + 1
            }else{
              df <- data.frame(x =0 ,y=0,xend = Bn[1]*cos(2*pi*tt), yend = Bn[1]*sin(2*pi*tt),time = 1:N,grp = 1,lin = 1)
              termi <- termi + 1
            }
            
            
          }
          
          
        }else{
        
        if (An[i] > 1E-10){
          ii <- N*(termi-1)+ 1
          ee <- ii + N -1
          df1 <- data.frame(x =df$xend[ii:ee] ,y=df$yend[ii:ee],xend = df$xend[ii:ee] + An[1]*cos(2*pi*tt*ii), yend = df$yend[ii:ee] +An[1]*sin(2*pi*tt*ii),time = 1:N,grp = termi + 1,lin = 1)
          df <- rbind(df,df1)
          termi <- termi + 1

        }
        
        if (Bn[i] > 1E-10){
          ii <- N*(termi-1)+ 1
          ee <- ii + N -1
          df1 <- data.frame(x =df$xend[ii:ee] ,y=df$yend[ii:ee],xend = df$xend[ii:ee] + Bn[1]*cos(2*pi*tt*ii), yend = df$yend[ii:ee] +Bn[1]*sin(2*pi*tt*ii),time = 1:N,grp = termi + 1,lin = 1)
          df <- rbind(df,df1)
          termi <- termi + 1
          
        }
        

      }

    }
    
    ii <- N*(termi -1)+ 1
    ee <- ii + N -1
    maxx <- max(df$xend)
    df2 <- data.frame(x = maxx*2 ,y = df$yend[ii:ee],xend = df$xend[ii:ee] , yend = df$yend[ii:ee],time = 1:N,grp = termi+1,lin = 2)
    df <- rbind(df,df2)
    df
  
  }
  
  animate_fourier3 <- function(df,n,speed){
    
    N <- 100
    #x <- seq(from = min(lb),to = max(up),length.out = N)
    npeak <- n / 2 + 1
    tt <- seq(from =0, to = 2, length.out = N)
    
    xx <- fft(df$y)
    df2 <- plot.frequency.spectrum(xx,xlimits = c(0,150))
    
    # summary(df2[1:100,2])
    # iqr <- IQR(df2[1:100,2])
    # inten <- df2[1:100,2][which(df2[1:100,2] > 10*iqr)]
          
    inten <- sort(df2[1:100,2],decreasing = T)[1:npeak]
    freq <- df2[1:100,1][which(df2[1:100,2] %in% inten)] *speed
    R <- inten / inten[1]
    
    z <- 1:(N*length(inten))
    df_final <- data.frame(x = 0, y =0, xend = z, yend = z,time =0, grp = 0, lin = 1)

    
    for (i in 1:length(inten)){
    
      i1 <- N*(i-1) + 1
      e1 <- i1 + N - 1
      if (i == 1){
        df_final$xend[1:N] <- R[i]*cos(2*pi*freq[i]*tt)
        df_final$yend[1:N] <- R[i]*sin(2*pi*freq[i]*tt)
        df_final$time[1:N] <- 1:N
      }else{
   
        df_final$x[i1:e1] <- df_final$xend[(i1:e1) - N]
        df_final$y[i1:e1] <- df_final$yend[(i1:e1) - N]
        df_final$xend[i1:e1] <- df_final$xend[(i1:e1) - N]+ R[i]*cos(2*pi*freq[i]*tt)
        df_final$yend[i1:e1]  <- df_final$yend[(i1:e1) - N] + R[i]*sin(2*pi*freq[i]*tt)
        df_final$time[i1:e1] <- 1:N
        
     
      }
      df_final$grp[i1:e1]  <- i

      
    }
    
    ii <- N*(i-1)+ 1
    ee <- ii + N -1
    maxx <- max(df_final$xend)
    df2 <- data.frame(x = maxx*1.05 ,y = df_final$yend[ii:ee],xend = df_final$xend[ii:ee] , yend = df_final$yend[ii:ee],time = 1:N,grp = i+1,lin = 2)
    df_final <- rbind(df_final,df2)
    
    df_final
    
  }
  
  observeEvent(input$enter,{
    browser()
    df <- animate_fourier(c("-3","3"),c(-pi,0),c(0,pi),7,constant = T)
    
    df <- animate_fourier2(c("-3","3"),c(-pi,0),c(0,pi),21,constant = T)
    
    df2 <- df[(nrow(df) - 99):(nrow(df)),1:2]
    tt <- seq(from =0, to = 2, length.out = 100)
    df2$x <- df2$x + tt*5
    g1 <- ggplot(df, aes(x = x,y = y,xend = xend,yend =yend,color = factor(grp),linetype = factor(lin))) + geom_segment(size = 2,show.legend = F) + transition_time(time) + ease_aes('linear')+
      theme_void() 


    
    g2 <- ggplot(df2, aes(x = x,y = y)) + geom_line(size = 2) + transition_reveal(x) + ease_aes('linear') +      theme_void()
    
    a <- animate(g1,nframes = 100,fps = 20)
    anim_save(filename = "gif1.gif",animation = a,path = path_to_folder,height = 250)
    
    output$anim1 <- renderImage(list(src =paste0(path_to_folder,"/gif1.gif"),contentType = 'image/gif' ),deleteFile = T)
    
    a <- animate(g2,nframes = 100,fps = 20)
    anim_save(filename = "gif2.gif",animation = a,path = path_to_folder,height = 250)
    
    output$anim2 <- renderImage(list(src =paste0(path_to_folder,"/gif2.gif"),contentType = 'image/gif' ),deleteFile = T)
    
  }      
  )
  
  plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
    plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
    
    # TODO: why this scaling is necessary?
    plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
    
    plot(plot.data, t="h", lwd=2, main="", 
         xlab="Frequency (Hz)", ylab="Strength", 
         xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
    plot.data
  }
  
  
  observeEvent(input$animation_enter,{

    #df <- animate_fourier(c("-3","3"),c(-pi,0),c(0,pi),5,constant = T)
    n <- input$anim_square_terms
    speed <- switch(as.numeric(input$speed),2,1,.5,.25)
    fps <- switch (as.numeric(input$fps),5,10,20,30)
    frames <- switch(as.numeric(input$fps), 100,200,300,400)
    df <- get_fourier_points(c("-3","3"),c(-pi,0),c(0,pi),n,constant = T,expand_plot = 3)
    
    df <- animate_fourier3(df,n,speed)
    
    g1 <- ggplot(df, aes(x = x,y = y,xend = xend,yend =yend,color = factor(grp),linetype = factor(lin))) + geom_segment(size = 2,show.legend = F) + transition_time(time) + ease_aes('linear')+
      coord_fixed(expand = F) + theme_void()
    a1 <- animate(g1, nframes = frames, fps = fps)
    
    anim_save(filename = "a1.gif",animation = a1,path = path_to_folder,height = 250)
    
    output$anim1 <- renderImage(list(src =paste0(path_to_folder,"/a1.gif"),contentType = 'image/gif' ),deleteFile = T)
    
    tt <- seq(from =0, to = 2, length.out = 100)
    df2 <- df[(nrow(df) - 99):(nrow(df)),1:2]
    df2$x <- df2$x + tt*5
    
    g2 <- ggplot(df2, aes(x = x,y = y)) + geom_line(size = 2) + transition_reveal(x) + ease_aes('linear') + theme_void()
    
    a2 <- animate(g2, nframes = frames, fps = fps)
    
    anim_save(filename = "a2.gif",animation = a2,path = path_to_folder,height = 250)
    
    output$anim2 <- renderImage(list(src =paste0(path_to_folder,"/a2.gif"),contentType = 'image/gif' ),deleteFile = T)
    
    
  })
  

  
  observeEvent(input$square_enter,{
    
    #amp <- input$square_amplitude
    amp <- 2
    f <- as.character(c(-amp,amp))
    n <- input$square_terms
    df <- get_fourier_points(f,c(-pi,0),c(0,pi),n,constant = T,expand_plot = 3)
    

    output$square_output <- renderPlot(
      ggplot(df, aes(x,y)) + geom_line() + coord_fixed() + theme_void()
    )
    
  })
  
  observeEvent(input$custom_enter,{
    f <- input$custom_function
    lb <- as.numeric(input$custom_lb)
    up <- as.numeric(input$custom_up)
    n <- input$custom_terms
    df <- get_fourier_points(f,lb,up,n)
    
    output$custom_output <- renderPlot(
      ggplot(df, aes(x,y)) + geom_line() + theme_minimal() + xlab("X") + ylab("Y") + ggtitle(TeX(convert_string_to_latex(f,"f"))) +
        theme(axis.text = element_text(size = rel(1.5)),
              title = element_text(size = rel(3))) 
    )
  })
  
  
  }
  