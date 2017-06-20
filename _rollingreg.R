rollingreg <- function(oilxts,width)
{
  best.model.rolling.coeff <- rollapply(na.omit(oilxts),
                                        width=width,
                                        FUN = function(Z) 
                                        { 
                                          t = lm(as.data.frame(Z), na.rm=T); 
                                          return(t$coef) 
                                        },
                                        by.column=FALSE, align="right") 
  colnames(best.model.rolling.coeff) <- paste("beta", colnames(best.model.rolling.coeff), sep = "_")
  
  best.model.rolling.rsq <- rollapply(na.omit(oilxts),
                                      width=width,
                                      FUN = function(Z) 
                                      { 
                                        t = lm(as.data.frame(Z), na.rm=T); 
                                        return(summary(t)$adj.r.squared)
                                      },
                                      by.column=FALSE, align="right") 
  colnames(best.model.rolling.rsq)<-"Adj.R2"
  
  best.model.rolling.pred <- rollapply(na.omit(oilxts),
                                       width=width,
                                       FUN = function(Z) 
                                       { 
                                         t = lm(as.data.frame(Z), na.rm=T); 
                                         return(tail(predict(t,interval="prediction",level=0.80),1))
                                       },
                                       by.column=FALSE, align="right") 
  colnames(best.model.rolling.pred)<-c("fit","lwr","upr")
  
  rolling.model <- merge(best.model.rolling.coeff,
                         best.model.rolling.rsq,
                         best.model.rolling.pred)
}

