  x1=rnorm(25, mean=0, sd=1)
  y1=dnorm(x1, mean=0, sd=1)
 
    x2=rnorm(25, mean=0, sd=1)
    y2=dnorm(x2, mean=0, sd=1)
    plot(x1, y1, type='p', xlim=range(x1,x2), ylim=range(y1, y2), xlab='x',
            ylab='y')
    points(x2, y2, type='p', col="red", xlab='x', ylab='y')
   
   
    
