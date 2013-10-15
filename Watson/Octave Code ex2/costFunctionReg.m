function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta

for i = 1:m,

hTheta = sigmoid(theta' * X(i,:)');

J = J - y(i)*log(hTheta) - (1 - y(i))*log(1-hTheta);

end;

J = J/(m);

K = sum(theta.^2) - theta(1)^2;
%subtract the theta(1) term as it corresponds to Theta_0.

K = K*lambda/(2*m);

J = J + K;


t = length(theta);

for j = 1:t,

K = 0;

for i = 1:m,


hTheta = sigmoid(theta' * X(i,:)');

K = K + (hTheta - y(i))*X(i,j);

end;

if(j == 1)
grad(j) = K/m;
else
grad(j) = K/m + lambda*theta(j)/m;

end;

end;





% =============================================================

end
