model2frame <-function(models) {

  if (!('list' %in% class(models))) {
    models = list(models)
  }

  r=data.frame()
  i=1

  for (m in models) {
    # adding the coeffs
    if ( 'mer' %in% class(m)) {
      coefs = summary(m)@coefs
    } else {
      coefs = summary(m)$coef
    }

    if (ncol(coefs)>=4) {
       ld = data.frame(coefs[,c(1,2,4)])
    } else {
       ld = data.frame(value = coefs[,1], sd = coefs[,2] ,pval=rep(NA,nrow(coefs)))
    }
    colnames(ld) <- c('value','sd','pval')
    ld$variable = rownames(coefs)
    rownames(ld) <- NULL

    # adding a BIC/AIC
    bic = BIC(m)
    if (length(bic)>0) ld = rbind(ld, data.frame(variable = 'BIC', value = BIC(m),sd=NA,pval=NA ));

    # try to get rsquare
    sfit = summary(m)
    if ( 'r.squared' %in% names(sfit)) {
      ld = rbind(ld, data.frame(variable = 'Rsq', value = sfit$r.squared,sd=NA,pval=NA ))
    }

    if (str_length(names(models)[i])>0) {
      ld$model = names(models)[i] 
    } else {
      ld$model=paste('model',i)
    }
    r =rbind(r,ld)
    i =i+1
  }

  return(r)
}



taes <- function (x, y, ...) 
{
    aes <- structure(as.list(match.call()[-1]), class = "uneval")
    return(aes)
}