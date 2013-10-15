function plotData(X, y)
%PLOTDATA Plots the data points X and y into a new figure 
%   PLOTDATA(x,y) plots the data points with + for the positive examples
%   and o for the negative examples. X is assumed to be a Mx2 matrix.

% Create New Figure
figure; hold on;

% ====================== YOUR CODE HERE ======================
% Instructions: Plot the positive and negative examples on a
%               2D plot, using the option 'k+' for the positive
%               examples and 'ko' for the negative examples.
%
pos = find(y==1); neg = find(y == 0);
% Plot Examples
plot(X(pos, 1), X(pos, 2), 'k+','LineWidth', 2, ...
'MarkerSize', 7);
plot(X(neg, 1), X(neg, 2), 'ko', 'MarkerFaceColor', 'y', ...
'MarkerSize', 7);

%plot(x,y, 'rx', 'MarkerSize', 10);
%ylabel('Exam 2 score');
%xlabel('Exam 1 score');

%>>>>> 	x1 = 0:5:10;
%>>>>> 	x2 = 0:10;
%>>>>> 	y1 = rand (size (x1));
%>>>>> 	y2 = rand (size (x2));
%>>>>> 	[ax, h1, h2] = plotyy (x1, y1, x2, y2, @plot, @plot);
%>>>>> 	y1Label = "y1";
%>>>>> 	y2Label = "y2";
%>>>>>
%>>>>> The command below should produce the desired legend (please check, the implementation may have changed since 3.4.0).
%>>>>>
%>>>>> 	legend ([h1, h2], {y1Label, y2Label}, "location", "south")
%>>>>>
%>>>>> The commands below should also work, but currently reverse the legend entries.
%>>>>>
%>>>>> 	legend ({y1Label, y2Label}, "location", "south")
%>>>>>
%>>>>> 	legend (y1Label, y2Label, "location", "south");





% =========================================================================



hold off;

end
