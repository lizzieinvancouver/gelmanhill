pgls <- function (formula, data, lambda = 1, kappa = 1, delta = 1, param.CI = 0.95, 
    control = list(fnscale = -1), bounds = NULL) 
{
    Dfun <- function(Cmat) {
        iCmat <- solve(Cmat, tol = .Machine$double.eps)
        svdCmat <- La.svd(iCmat)
        D <- svdCmat$u %*% diag(sqrt(svdCmat$d)) %*% t(svdCmat$v)
        return(t(D))
    }

    m <- model.frame(formula, data$data)
    y <- m[, 1]
    x <- model.matrix(formula, m)
    k <- ncol(x)
    namey <- names(m)[1]
    xVar <- apply(x, 2, var)[-1]
    badCols <- xVar < .Machine$double.eps
    if (any(badCols)) 
        stop("Model matrix contains columns with zero variance: ", 
            paste(names(xVar)[badCols], collapse = ", "))
    if (is.null(data$vcv)) {
        V <- if (kappa == 1) {
            VCV.array(data$phy)
        }
        else {
            VCV.array(data$phy, dim = 3)
        }
        data$vcv <- V
    }
    else {
        V <- data$vcv
    }
    nm <- names(data$data)
    n <- nrow(data$data)
    if (!is.null(param.CI)) {
        if (!is.numeric(param.CI) || param.CI <= 0 || param.CI > 
            1) 
            stop("param.CI is not a number between 0 and 1.")
    }
    usrBounds <- bounds
    bounds <- list(kappa = c(1e-06, 3), lambda = c(1e-06, 1), 
        delta = c(1e-06, 3))
    if (!is.null(usrBounds)) {
        if (!is.list(usrBounds)) 
            stop("Bounds must be a list of named bounds for any or all of kappa, lambda and delta")
        usrNames <- names(usrBounds)
        badNames <- setdiff(usrNames, c("kappa", "lambda", "delta"))
        if (length(badNames) > 0) 
            stop("The list of bounds contains names other than kappa, lambda and delta")
        for (nm in usrNames) {
            bounds[nm] <- usrBounds[nm]
        }
    }
    parVals <- list(kappa = kappa, lambda = lambda, delta = delta)
    for (i in seq_along(parVals)) {
        p <- parVals[[i]]
        nm <- names(parVals)[i]
        if (length(p) > 1) 
            stop(nm, " not of length one.")
        if (is.character(p) & p != "ML") 
            stop(nm, " is character and not 'ML'.")
        bnds <- bounds[[nm]]
        if (length(bnds) > 2) 
            stop("Bounds specified for ", nm, " not of length one.")
        if (!is.numeric(bnds)) 
            stop("Non-numeric bounds specified for ", nm, ".")
        if (any(bnds < 0)) 
            stop("Negative values in bounds specified for ", 
                nm, ".")
        lb <- bnds[1]
        ub <- bnds[2]
        if (lb > ub) 
            stop("Lower bound greater than upper bound for ", 
                nm, ".")
        if (is.numeric(p) & (p < lb | p > ub)) 
            stop(sprintf("%s value (%0.2f) is out of specified bounds [%0.2f, %0.2f]", 
                nm, p, lb, ub))
    }
    if (kappa != 1 && length(dim(V)) != 3) 
        stop("3D VCV.array needed for kappa transformation.")
    mlVals <- sapply(parVals, "==", "ML")
    if (any(mlVals)) {
        parVals[mlVals] <- lapply(bounds, mean)[mlVals]
        parVals <- as.numeric(parVals)
        names(parVals) <- c("kappa", "lambda", "delta")
        optimPar <- parVals[mlVals]
        fixedPar <- parVals[!mlVals]
        lower.b <- sapply(bounds, "[", 1)[mlVals]
        upper.b <- sapply(bounds, "[", 2)[mlVals]
        optim.param.vals <- optim(optimPar, fn = pgls.likelihood, 
            method = "L-BFGS-B", control = control, upper = upper.b, 
            lower = lower.b, V = V, y = y, x = x, fixedPar = fixedPar, 
            optim.output = TRUE)
        if (optim.param.vals$convergence != "0") {
            stop("Problem with optim:", optim.param.vals$convergence, 
                optim.param.vals$message)
        }
        fixedPar <- c(optim.param.vals$par, fixedPar)
        fixedPar <- fixedPar[c("kappa", "lambda", "delta")]
    }
    else {
        fixedPar <- as.numeric(parVals)
        names(fixedPar) <- c("kappa", "lambda", "delta")
    }
    ll <- pgls.likelihood(optimPar = NULL, fixedPar = fixedPar, 
        y, x, V, optim.output = FALSE)
    log.lik <- ll$ll
    Vt <- pgls.blenTransform(V, fixedPar)
    aic <- -2 * log.lik + 2 * k
    aicc <- -2 * log.lik + 2 * k + ((2 * k * (k + 1))/(n - k - 
        1))
    coeffs <- ll$mu
    names(coeffs) <- colnames(x)
    varNames <- names(m)
    pred <- x %*% ll$mu
    res <- y - pred
    D <- Dfun(Vt)
    pres <- D %*% res
    fm <- list(coef = coeffs, aic = aic, log.lik = log.lik)
    RMS <- ll$s2
    RSSQ <- ll$s2 * (n - k)
    xdummy <- matrix(rep(1, length(y)))
    nullMod <- pgls.likelihood(optimPar = NULL, fixedPar = fixedPar, 
        y, xdummy, V, optim.output = FALSE)
    NMS <- nullMod$s2
    NSSQ <- nullMod$s2 * (n - 1)
    errMat <- t(x) %*% solve(Vt) %*% x
    errMat <- solve(errMat) * RMS[1]
    sterr <- diag(errMat)
    sterr <- sqrt(sterr)
    RET <- list(model = fm, formula = formula, call = call, RMS = RMS, 
        NMS = NMS, NSSQ = NSSQ[1], RSSQ = RSSQ[1], aic = aic, 
        aicc = aicc, n = n, k = k, sterr = sterr, fitted = pred, 
        residuals = res, phyres = pres, x = x, data = data, varNames = varNames, 
        y = y, param = fixedPar, mlVals = mlVals, namey = namey, 
        bounds = bounds, Vt = Vt, dname = dname)
    class(RET) <- "pgls"
    if (any(miss.na)) {
        RET$na.action <- structure(which(miss.na), class = "omit", 
            .Names = miss.names)
    }
    if (!is.null(param.CI) && any(mlVals)) {
        param.CI.list <- list(kappa = NULL, lambda = NULL, delta = NULL)
        mlNames <- names(mlVals)[which(mlVals)]
        for (param in mlNames) {
            param.CI.list[[param]] <- pgls.confint(RET, param, 
                param.CI)
        }
        RET$param.CI <- param.CI.list
    }
    return(RET)
}
