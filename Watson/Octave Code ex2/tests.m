thetaHist = rand(1500, length(theta))
theta = [3; 2]
bozo = theta'
thetaHist(1,1) = bozo
ja = thetaHist(1)



thetahist


theta = [1; 2; 4]
X
%theta' *
options = optimset('GradObj','on','MaxIter','100');
initialTheta = zeros(2,1)

[optTheta, functionVal, exitFlag] = fminunc(@costFunction, initialTheta, options)
%Numerically, optimal value is 5,5
%function value is essentially 0.
%exitFlag is 1, convergence status, thus successful.
%theta must be at least 2 dimensions.
