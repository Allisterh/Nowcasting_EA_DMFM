clear
clc

% Set Base_Dir
Base_Dir  = cd;
addpath(genpath(Base_Dir));


P.DataFile = 'Data\ES\Processed\Data_ES';
P.Legend = 'Data\ES\Original\Legend_ES';

% forecast series
P.SerNews  = 'GDP_ES';
% forecast reference period
P.Qnews    = [2024 10];
% first forecast done in
P.StartEv  = [2024 6];
% last forecast done in
P.EndEv    = [2024 12];
% start of the estimation sample
P.StartEst = [2000 2];


% covid period 
P.covid_start = [2020, 3, 1];
P.covid_end = [2020, 12, 1];

% with AR(1) idiosyncratic component 
P.idio = ''; 

P.r = 1; P.p = 1;
P.Model = 'Medium';
funNews_ML(P);

%--------------------------------------------------------------------------
% Graphical representation
%--------------------------------------------------------------------------
% --- Setup ---
covid_start = P.covid_start;
covid_end = P.covid_end;

% Convert dates (if needed)
DateQQ_dt = datetime(DateQQ, 'ConvertFrom', 'datenum');

% --- Forecast Comparison: Old vs New ---
figure;
hold on;
plot(DateQQ_dt, y_old, '--k', 'LineWidth', 1.5, 'DisplayName', 'Old Forecast');
plot(DateQQ_dt, y_new, '-b', 'LineWidth', 2, 'DisplayName', 'Updated Forecast');
if exist('trueSer', 'var') && exist('DateQQ_V', 'var')
    scatter(DateQQ_dt(end), trueSer, 30, 'r', 'filled', 'DisplayName', 'Actual Value');
end
legend('Location', 'Best');
xlabel('Date');
ylabel('GDP Forecast Value');
title('Old vs Updated Forecast');
grid on;
hold off;

% --- News Impact by Group (Stacked Bar Chart) ---
% Ensure DateQQ is in datetime format (month granularity)
% Ensure DateQQ is datetime
if ~isdatetime(DateQQ)
    DateQQ = datetime(DateQQ, 'ConvertFrom', 'datenum');
end

% Create stacked bar chart
figure;
hb = bar(DateQQ, groupnews, 'stacked');

% X-axis formatting
xtickformat('MMM yyyy');
xticks(DateQQ);
xlim([min(DateQQ) - days(15), max(DateQQ) + days(15)]);  % Add 15-day padding on each side

% Aesthetics
xlabel('Month');
ylabel('Impact of News on Forecast');
title('Monthly Impact of News by Group');
legend(GroupNames, 'Location', 'eastoutside');
grid on;
box off;

% Optional: apply color map
numGroups = size(groupnews, 2);
customColors = lines(numGroups);
for i = 1:numGroups
    hb(i).FaceColor = customColors(i, :);
end


% --- Custom Colors for Group Plot (Including CI in Black) ---
numGroups = size(groupnews, 2);
customColors = lines(numGroups);
idx_CI = find(strcmp(GroupNames, 'CI'));
if ~isempty(idx_CI)
    customColors(idx_CI, :) = [0 0 0];
end
for i = 1:numGroups
    hb(i).FaceColor = customColors(i, :);
end

% --- News Impact by Variable (Heatmap) ---
figure;
imagesc(singlenews');
colorbar;
xlabel('Date');
ylabel('Variables');
title('Impact of Individual Variables on Forecasts');
set(gca, 'YTick', 1:length(Series), 'YTickLabel', Series);
colormap(jet);

% --- Forecast Accuracy (Forecast Error over Time) ---
if exist('trueSer', 'var')
    figure;
    error_forecast = trueSer - y_new;
    plot(DateQQ_dt, error_forecast, '-r', 'LineWidth', 2);
    yline(0, '--k');
    xlabel('Date');
    ylabel('Forecast Error');
    title('Forecast Accuracy');
    grid on;
end

%% === Unified News Impact Chart with Custom Colors and CI in Gray ===

% --- File names and country labels ---
fileList = {'newsDE_11.mat', 'newsFR_11.mat', 'newsIT_11.mat', 'newsES_11.mat'};
countryLabels = {'Germany', 'France', 'Italy', 'Spain'};

% --- Custom base colors for groups ---
baseColors = [
    0.00, 0.45, 0.74;  % blue
    0.85, 0.33, 0.10;  % orange
    0.93, 0.69, 0.13;  % yellow
    0.49, 0.18, 0.56;  % purple
    0.47, 0.67, 0.19;  % green
    0.30, 0.75, 0.93;  % cyan
    0.64, 0.08, 0.18;  % red wine
    0.80, 0.80, 0.45;  % olive 
    0.00, 0.50, 0.00;  % dark green
    0.75, 0.25, 0.75   % violet
];

% --- Create figure ---
figure('Name', 'News Impact by Country', 'Units', 'normalized', 'Position', [0.1, 0.1, 0.75, 0.75]);
sgtitle('News Impact by Group', 'FontSize', 19, 'FontWeight', 'bold');

% Define subplot positions (leave space for legend)
subplotPositions = {
    [0.08 0.55 0.35 0.35],  % Germany
    [0.48 0.55 0.35 0.35],  % France
    [0.08 0.10 0.35 0.35],  % Italy
    [0.48 0.10 0.35 0.35]   % Spain
};

% Store legend handles
legendHandles = [];
legendLabels = [];

for i = 1:4
    % Load data
    data = load(fileList{i});
    DateQQ = data.DateQQ;
    groupnews = data.groupnews;
    GroupNames = data.GroupNames;

    if ~isdatetime(DateQQ)
        DateQQ = datetime(DateQQ, 'ConvertFrom', 'datenum');
    end

    % Initialize colors from baseColors
    numGroups = size(groupnews, 2);
    customColors = baseColors(1:numGroups, :);

    % Override CI color to light gray
    idx_CI = find(strcmp(GroupNames, 'CI'));
    if ~isempty(idx_CI)
        customColors(idx_CI, :) = [0.8 0.8 0.8];  % light gray
    end

    % Subplot with manual positioning
    axes('Position', subplotPositions{i});
    hb = bar(DateQQ, groupnews, 'stacked');
    for g = 1:numGroups
        hb(g).FaceColor = customColors(g, :);
    end

    % Save handles once for legend
    if i == 1
        legendHandles = hb;
        legendLabels = GroupNames;
    end

    xtickformat('MMM yyyy');
    xticks(DateQQ(1:3:end));
    xlim([min(DateQQ) - days(15), max(DateQQ) + days(15)]);
    title(countryLabels{i});
    xlabel('Month');
    ylabel('News Impact');
    grid on;
    box off;
end

% Add legend to the right
legend(legendHandles, legendLabels, 'Position', [0.88 0.35 0.1 0.3]);

%%
% === Unified Heatmaps of News Impact by Variable (DE, FR, IT, ES) ===
fileList = {'newsDE_11.mat', 'newsFR_11.mat', 'newsIT_11.mat', 'newsES_11.mat'};
countryLabels = {'Germany', 'France', 'Italy', 'Spain'};

% Create figure
figure('Name', 'News Impact by Variable', 'Units', 'normalized', 'Position', [0.1, 0.1, 0.8, 0.78]);
sgtitle('News Impact by Variable', 'FontSize', 19, 'FontWeight', 'bold');

% Adjusted subplot positions: more vertical space
subplotPositions = {
    [0.08 0.56 0.36 0.33],  % Germany
    [0.53 0.56 0.36 0.33],  % France
    [0.08 0.14 0.36 0.33],  % Italy
    [0.53 0.14 0.36 0.33]   % Spain
};

% Custom blue-white-red colormap
n = 256;
half = floor(n/2);
customMap = [linspace(0,1,half)', linspace(0,1,half)', ones(half,1);  % Blue to white
             ones(half,1), linspace(1,0,half)', linspace(1,0,half)']; % White to red

allVals = [];

% Gather values for consistent color scale
for i = 1:4
    data = load(fileList{i});
    allVals = [allVals; data.singlenews(:)];
end
absMax = max(abs(allVals));

% Plot each country's heatmap
for i = 1:4
    data = load(fileList{i});
    singlenews = data.singlenews;
    DateQQ = data.DateQQ;
    Series = data.Series;

    if ~isdatetime(DateQQ)
        DateQQ = datetime(DateQQ, 'ConvertFrom', 'datenum');
    end

    % Clean variable names by removing suffixes (e.g., "_IT", "_ES", etc.)
    cleanedSeries = regexprep(Series, '_[A-Z]{2}$', '');

    axes('Position', subplotPositions{i});
    imagesc(DateQQ, 1:length(Series), singlenews', [-absMax, absMax]);

    % Axis and ticks
    set(gca, 'YTick', 1:2:length(Series), ...
             'YTickLabel', cleanedSeries(1:2:end), ...
             'FontSize', 6);

    xtickformat('MMM yyyy');
    xticks(DateQQ(1:3:end));
    xlim([min(DateQQ), max(DateQQ)]);

    title(countryLabels{i});
    xlabel('Date');
    ylabel('Variables');
end

% Colorbar (shared)
colormap(customMap);
cb = colorbar('Position', [0.92 0.10 0.015 0.82]);
cb.Label.String = 'News Impact';
cb.FontSize = 10;
