SIR_sim <- function(tend, SIR.ini, paras = c(0.2, 0.2)){
    S.max <- 10000
    I.max <- S.max
    R.max <- S.max
    beta <- paras[1]
    gamma <- paras[2]

    SIR.matrix <- matrix(data = 0, nrow = tend + 1, ncol = 3)
    SIR.matrix[1,] <- SIR.ini
    for(t in 2:(tend+1)){
        S.t1 <- SIR.matrix[t-1,1]
        I.t1 <- SIR.matrix[t-1,2]
        R.t1 <- SIR.matrix[t-1,3]

        S.t <- min(max(S.t1 - beta * I.t1 * S.t1, 0),S.max)
        I.t <- min(max(I.t1 + beta * I.t1 * S.t1 - gamma * I.t1, 0),S.max)
        R.t <- min(max(gamma * I.t1, 0),S.max)


        SIR.matrix[t,] <- c(S.t, I.t, R.t)
    }
    x <- c(1:nrow(SIR.matrix))
    ys <- SIR.matrix[,1]
    yi <- SIR.matrix[,2]
    yr <- SIR.matrix[,3]
    data <- data.frame(x, ys, yi, yr)

    fig <- plot_ly(data, x = ~x, y = ~ys, name = 'S', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~yi, name = 'I', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~yr, name = 'R', mode = 'lines+markers')
    fig
}
