### R helper function to create unit-weighted factor scores using
### method described in pretty much all of my papers, e.g., Weiss et
### al. 2015

### Revised on 8 April 2020: Added 'precision' as an option. This
### option lets you have a bit more control over your cut-point for
### salience.

uwscores <- function(.fa_object, .manifests, min=0, max=0, salient=.4,
                     precision=2)
{
    
    .lambda <- .fa_object$loadings[, 1:ncol(.fa_object$loadings)]

    if (is.null(precision) | is.na(precision) | precision<1) {
        
        .uw <- t(apply(abs(.lambda), 1, function(x) (x==max(x) & x>=salient)))

    }

    else {

        .uw <- t(apply(abs(.lambda), 1, function(x) (x==max(x) &
                                                     round(x, precision)>=salient)))

    }
    
    .uw[.lambda<0] <- .uw[.lambda<0]*-1

    .rawsums <- (as.matrix(.manifests) %*% as.matrix(.uw))

    as.data.frame(t(apply(.rawsums, 1, function(x) (x+(colSums(.uw==-1)*(min+max)))/colSums(.uw!=0))))

}




