# exercise 32 a and b

litterfac_1 = as.factor( rep(1:4,each=4) )
litterfac_1
litterfac = litterfac_1[-16]
litterfac

hormonefac = as.factor(  rep(1:3,5) )
hormonefac

X = model.matrix( ~ hormonefac + litterfac )
X
XtXinv = solve( t(X) %*% X )
XtXinv


# var ( h1 - h2 )

c1 = c(0,-1,0,0,0,0)
t(c1) %*% XtXinv %*% c1

# var ( h1 - h3 )

c2 = c(0,0,-1,0,0,0)
t(c2) %*% XtXinv %*% c2

# var ( h3 - h2 )

c3 = c(0,-1,1,0,0,0)
t(c3) %*% XtXinv %*% c3

# exercise 34

# trial 1 : abcda,CBD,dbabcd,max_variance = 0.3347

blockfac = c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6)
blockfac

treatmentfac = c(1,2,3,4,1,1,2,3,4,4,1,2,3,4,3,4,1,2,2,1,2,3,4,1,2,3,4)
treatmentfac

# trial 2 : bcdab,CBD,dbabcd,max_variance = 0.3687

blockfac = c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6)
blockfac

treatmentfac = c(2,3,4,1,2,1,2,3,4,4,1,2,3,4,3,4,1,2,2,1,2,3,4,1,2,3,4)
treatmentfac

# trial 3 : cdabc,CBD,dbabcd,max_variance = 0.3445

blockfac = c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6)
blockfac

treatmentfac = c(3,4,1,2,3,1,2,3,4,4,1,2,3,4,3,4,1,2,2,1,2,3,4,1,2,3,4)
treatmentfac

# trial 4 : dabcd,CBD,dbabcd,max_variance = 0.3690

blockfac = c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6)
blockfac

treatmentfac = c(4,1,2,3,4,1,2,3,4,4,1,2,3,4,3,4,1,2,2,1,2,3,4,1,2,3,4)
treatmentfac

# trial 5 upon 1 : abcda,CBD,dcabcd,max_variance = 0.3361

blockfac = c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6)
blockfac

treatmentfac = c(1,2,3,4,1,1,2,3,4,4,1,2,3,4,3,4,1,2,3,1,2,3,4,1,2,3,4)
treatmentfac

# trial 6 upon 1 : abcda,CBD,dbaacd,max_variance = 

blockfac = c(1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6)
blockfac

treatmentfac = c(1,2,3,4,1,1,2,3,4,4,1,2,3,4,3,4,1,2,2,1,2,3,4,1,1,3,4)
treatmentfac

## common code for calculations after running specific trial code

X <- model.matrix( ~ as.factor(treatmentfac) + as.factor(blockfac))
X

XtXinv = solve( t(X) %*% X )
XtXinv

# var (t1 - t2)

c1 = c(0,-1,0,0,0,0,0,0,0)
t(c1) %*% XtXinv %*% c1


# var (t1 - t3)

c2 = c(0,0,-1,0,0,0,0,0,0)
t(c2) %*% XtXinv %*% c2

# var (t1 - t4)

c3 = c(0,0,0,-1,0,0,0,0,0)
t(c3) %*% XtXinv %*% c3

# var (t2 - t3)

c4 = c(0,1,-1,0,0,0,0,0,0)
t(c4) %*% XtXinv %*% c4

# var (t2 - t4)

c5 = c(0,1,0,-1,0,0,0,0,0)
t(c5) %*% XtXinv %*% c5

# var (t3 - t4)

c6 = c(0,0,1,-1,0,0,0,0,0)
t(c6) %*% XtXinv %*% c6

max_variance = max(t(c1) %*% XtXinv %*% c1,t(c2) %*% XtXinv %*% c2,t(c3) %*% XtXinv %*% c3,t(c4) %*% XtXinv %*% c4,t(c5) %*% XtXinv %*% c5,t(c6) %*% XtXinv %*% c6)
max_variance




