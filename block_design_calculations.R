# balanced design
rowfac = as.factor( rep(1:4,each=4) )
varfac = as.factor( rep(1:4,4) )

X = model.matrix( ~ rowfac + varfac )
XtXinv = solve( t(X) %*% X )

# Variance of \hat{v}_B - \hat{v}_A
cvec = c(0,0,0,0,1,0,0)
t(cvec) %*% XtXinv %*% cvec

# Variance of \hat{v}_D - \hat{v}_C
cvec = c(0,0,0,0,0,-1,1)
t(cvec) %*% XtXinv %*% cvec

coefmat = XtXinv %*% t(X)

# coefficients for estimating v_B - v_A
coefmat[5,]
# coefficients for estimating v_C - v_B
coefmat[6,]-coefmat[5,]






# unbalanced design
varfac = as.factor( c(1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,1) )
X = model.matrix( ~ rowfac + varfac )
XtXinv = solve( t(X) %*% X )

# Variance of \hat{v}_B - \hat{v}_A
cvec = c(0,0,0,0,1,0,0)
t(cvec) %*% XtXinv %*% cvec

# Variance of \hat{v}_C - \hat{v}_A
cvec = c(0,0,0,0,0,1,0)
t(cvec) %*% XtXinv %*% cvec

coefmat = XtXinv %*% t(X)
# coefficients for estimating v_B - v_A
coefmat[5,]*12
# coefficients for estimating v_C - v_B
coefmat[6,]*12






# balanced incomplete design
rowfac = as.factor( rep(1:4,each=3) )
varfac = as.factor( rep(1:4,3) )
X = model.matrix( ~ rowfac + varfac )
XtXinv = solve( t(X) %*% X )

# Variance of \hat{v}_B - \hat{v}_A
cvec = c(0,0,0,0,1,0,0)
t(cvec) %*% XtXinv %*% cvec

# Variance of \hat{v}_C - \hat{v}_B
cvec = c(0,0,0,0,-1,1,0)
t(cvec) %*% XtXinv %*% cvec

coefmat = XtXinv %*% t(X)
# coefficients for estimating v_B - v_A
round( coefmat[5,]*8, 2)
# coefficients for estimating v_C - v_B
round( (coefmat[6,]-coefmat[5,])*8, 2)











# balanced overcomplete design (5)
rowfac = as.factor( rep(1:4,each=5) )
varfac = as.factor( c(1:4,1,1:4,2,1:4,3,1:4,4))
X = model.matrix( ~ rowfac + varfac )
XtXinv = solve( t(X) %*% X )

# Variance of \hat{v}_B - \hat{v}_A
cvec = c(0,0,0,0,1,0,0)
t(cvec) %*% XtXinv %*% cvec
# Variance of \hat{v}_C - \hat{v}_A
cvec = c(0,0,0,0,0,1,0)
t(cvec) %*% XtXinv %*% cvec


coefmat = XtXinv %*% t(X)
# coefficients for estimating v_B - v_A
coefmat[5,]
# coefficients for estimating v_D - v_C
coefmat[7,]-coefmat[6,]











# balanced overcomplete design (6)
rowfac = as.factor( rep(1:4,each=6) )
varfac = as.factor( c(rep(1:4,4), 1,3,1:4,2,4) )
varfac = as.factor( c(1:4,1,2,1:4,3,4,1:4,1,3,1:4,2,4) )
X = model.matrix( ~ rowfac + varfac )
XtXinv = solve( t(X) %*% X )


# Variance of \hat{v}_B - \hat{v}_A
cvec = c(0,0,0,0,1,0,0)
t(cvec) %*% XtXinv %*% cvec
# Variance of \hat{v}_C - \hat{v}_A
cvec = c(0,0,0,0,0,1,0)
t(cvec) %*% XtXinv %*% cvec
# Variance of \hat{v}_C - \hat{v}_B
cvec = c(0,0,0,0,-1,1,0)
t(cvec) %*% XtXinv %*% cvec


coefmat = XtXinv %*% t(X)
# coefficients for estimating v_B - v_A
coefmat[5,]
# coefficients for estimating v_C - v_A
coefmat[6,]
# coefficients for estimating v_C - v_B
coefmat[6,]-coefmat[5,]




