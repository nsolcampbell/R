# kalman.R
#
# kalman filter
#
# Author: lianmc@qq.com
#

# X(k) = A(k,k-1)X(k-1) + G(k,k-1)w(k-1)
# Z(k) = C(k)X(k) + v(k)
# E(w(k)) = 0, COV(w(k),w(l)) = Q
# E(v(k)) = 0, COV(v(k),v(l)) = R
# COV(w(k),v(l)) = 0
# E(X(0)) = X0, VAR(X(0)) = P0
#
# A: system matrix
# G: error matrix
# C: Output matrix
# Q: covariance of noise w
# R: covariance of noise v
# X0: initial value of the state
# P0: initial variance of the state
# Z: measurement vector
kalman <- function(A,G,C,Q,R,X0,P0,Z)
{
        X <- c(X0)
        P <- c(P0)
        X[1] = X0
        N = length(Z)
        I = diag(dim(A)[1])
        
        for (i in 1:N) {
                # predict error matrix
                Pp = A %*% P[i] %*% t(A) + G %*% Q %*% t(G)
                # Kalman Gain
                K = Pp %*% t(C) %*% solve(C %*% Pp %*% t(C) + R)
                # predict State
                Xp = A %*% X[i]
                # state correction
                X[i+1] = Xp + K %*% (Z[i] - C %*% Xp)
                # filter error
                P[i+1] <- (I - K %*% C) %*% Pp %*% t(I - K %*% C) + K %*% R %*% t(K)
        }

        print(X[1:N+1])
        return(X[1:N+1])
}