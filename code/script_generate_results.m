%% load data

clear;

path_to_inputFolder = "../data/";

path_to_inputDataFile = fullfile(path_to_inputFolder, 'data_extraction_form.csv');

T_all = readtable(path_to_inputDataFile);

% subgroup must be either
% all - include all studies
% menthol - include only menthol studies
% peppermint - include only peppermint studies
% biofreeze - include only biofreeze studies
subgroup = "all";

%% use all studies or filter by subgroup

if strcmp(subgroup, "all")
    T = T_all;
elseif strcmp(subgroup, "menthol")
    filter_column = "i_MentholSource_broad";
    filter_row = "Menthol";
    idx = strcmp(string(T_all.(filter_column)), filter_row);
    T = T_all(idx,:);
elseif strcmp(subgroup, "peppermint")
    filter_column = "i_MentholSource_broad";
    filter_row = "Peppermint";
    idx = strcmp(string(T_all.(filter_column)), filter_row);
    T = T_all(idx,:);
elseif strcmp(subgroup, "biofreeze")
    filter_column = "i_MentholSource_specific";
    filter_row = "Biofreeze";
    idx = strcmp(string(T_all.(filter_column)), filter_row);
    T = T_all(idx,:);
end


%% make output folder

output_folder = strcat('../results_', subgroup, '/');

if ~exist(output_folder, 'dir')
    mkdir(output_folder)
end

%% set some figure variables

blue_colour = [77 140 174] / 255;

fig_size_x = 16;
fig_size_y = 8;

section_labels_x = [0.062, 0.5];
section_labels_y = [0.99, 0.51];
section_labels_fontsize = 18;

axis_font_size = 12;

%% Table 1: summary characteristics

first_author = string(T{:,"g_FirstAuthor"});
year = string(T{:,"g_Year"});
author_year = string;
for i = 1 : length(first_author)
    author_year(i) = strcat(first_author(i), " (", year(i), ")");
end

author_year = author_year';
study_design = T{:,"d_StudyDesign"};
pain_type = T{:,"d_PainType_specific"};
menthol_vehicle = T{:,"i_MentholVehicle"};
body_location = T{:,"i_MentholBodyLocation"};
cointervention = T{:,"c_Cointervention"};
comparator = T{:,"c_ComparatorType_specific"};

T1 = table(author_year, study_design, pain_type, menthol_vehicle,...
    body_location, cointervention, comparator);

old_var_names = ["author_year", "study_design", "pain_type",...
    "menthol_vehicle", "body_location", "cointervention", "comparator"];

new_var_names = ["Author (Year)", "Study design", "Pain type",...
    "Menthol vehicle", "Body location", "Co-intervention", "Comparator"];

T1 = renamevars(T1, old_var_names, new_var_names);

file_name = strcat(output_folder, 'table_1_summary_characteristics.xlsx');

writetable(T1, file_name);

%% Table 2: adverse events

first_author = string(T{:,"g_FirstAuthor"});
year = string(T{:,"g_Year"});
author_year = string;
for i = 1 : length(first_author)
    author_year(i) = strcat(first_author(i), " (", year(i), ")");
end

author_year = author_year';
adverse_events_broad = T{:,"o_AdverseEvents"};
adverse_events_specific = T{:,"o_AdverseEvents_details"};

T1 = table(adverse_events_broad, author_year, adverse_events_specific);

T1(strcmp(string(T1{:,"adverse_events_broad"}), "No information"), :) = [];

T1 = sortrows(T1,"adverse_events_broad", 'descend');

old_var_names = ["adverse_events_broad", "author_year", "adverse_events_specific"];

new_var_names = ["Presence of AEs", "Author (Year)", "Evidence provided by original investigators"];

T1 = renamevars(T1, old_var_names, new_var_names);

file_name = strcat(output_folder, 'table_2_adverse_events.xlsx');

writetable(T1, file_name);

%% Figure 2: design

file_name = strcat(output_folder, 'fig_2_design.pdf');

figure;

% a. study designs

study_designs = string(T.d_StudyDesign);

study_designs_unique = unique(study_designs);

x = (1:1:length(study_designs_unique))';
y = x*0;

for i_sd_counter = 1 : length(x)
    i_sd = study_designs_unique(i_sd_counter);
    y(i_sd_counter) = sum(strcmp(i_sd, study_designs));
end

[~,ind] = sort(y, 'descend');

X = study_designs_unique(ind);
Y = y(ind);

subplot(2,2,1)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% b. pain scales

scales = string(T.o_PainScales);

scales_new = char;

for i = 1 : length(scales)
    str = scales(i);
    newStr = split(str,',');
    scales_new = char(scales_new, char(newStr));
end

scales = strtrim(string(scales_new));

empty_entries = cellfun('isempty', scales);

scales(empty_entries) = [];

scales(strcmp(scales, 'NI')) = [];

scales_unique = unique(scales);

x = (1:1:length(scales_unique))';
y = x*0;

for i_scales_counter = 1 : length(x)
    i_scale = scales_unique(i_scales_counter);
    y(i_scales_counter) = sum(strcmp(i_scale, scales));
end

[~,ind] = sort(y, 'descend');

X = scales_unique(ind);
Y = y(ind);

subplot(2,2,2)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% c. induced pain

paintype_broad = string(T.d_PainType_broad);

paintype_induced = strcmp(paintype_broad, "Induced");

paintype_intermediate_raw = string(T.d_PainType_intermediate);

paintype_intermediate_induced_raw = paintype_intermediate_raw(paintype_induced, :);

pt_new = char;

for i = 1 : length(paintype_intermediate_induced_raw)
    str = paintype_intermediate_induced_raw(i);
    newStr = split(str,',');
    pt_new = char(pt_new, char(newStr));
end

paintype_intermediate_induced = strtrim(string(pt_new));

empty_entries = cellfun('isempty', paintype_intermediate_induced);

paintype_intermediate_induced(empty_entries) = [];

paintype_intermediate_induced_unique = unique(paintype_intermediate_induced);

x = (1:1:length(paintype_intermediate_induced_unique))';
y = x*0;

for i_pii_counter = 1 : length(x)
    i_pii = paintype_intermediate_induced_unique(i_pii_counter);
    y(i_pii_counter) = sum(strcmp(i_pii, paintype_intermediate_induced));
end

[~,ind] = sort(y, 'descend');

X = paintype_intermediate_induced_unique(ind);
Y = y(ind);

subplot(2,2,3)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% d. pre-existing pain

paintype_broad = string(T.d_PainType_broad);

paintype_preexisting = strcmp(paintype_broad, "Pre-existing");

paintype_intermediate_raw = string(T.d_PainType_intermediate);

paintype_intermediate_preexisting_raw = paintype_intermediate_raw(paintype_preexisting, :);

pt_new = char;

for i = 1 : length(paintype_intermediate_preexisting_raw)
    str = paintype_intermediate_preexisting_raw(i);
    newStr = split(str,',');
    pt_new = char(pt_new, char(newStr));
end

paintype_intermediate_preexisting = strtrim(string(pt_new));

empty_entries = cellfun('isempty', paintype_intermediate_preexisting);

paintype_intermediate_preexisting(empty_entries) = [];

paintype_intermediate_preexisting_unique = unique(paintype_intermediate_preexisting);

x = (1:1:length(paintype_intermediate_preexisting_unique))';
y = x*0;

for i_paintype_counter = 1 : length(x)
    i_paintype = paintype_intermediate_preexisting_unique(i_paintype_counter);
    y(i_paintype_counter) = sum(strcmp(i_paintype, paintype_intermediate_preexisting));
end

[~,ind] = sort(y, 'descend');

X = paintype_intermediate_preexisting_unique(ind);
Y = y(ind);

subplot(2,2,4)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;


% improve figure layout

set(gcf,...
    'Units', 'Inches', ...
    'Position', [0, 0, fig_size_x, fig_size_y], ...
    'PaperPositionMode', 'auto');

a = annotation('textbox', [section_labels_x(1), section_labels_y(1), 0, 0], 'string', 'a.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(1), 0, 0], 'string', 'b.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(1), section_labels_y(2), 0, 0], 'string', 'c.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(2), 0, 0], 'string', 'd.');
a.FontSize = section_labels_fontsize;


% save figure

exportgraphics(gcf, file_name);

%% Figure 3: Population

file_name = strcat(output_folder, 'fig_3_population.pdf');

figure;

% a. WHO region

who_regions = string(T.p_WHORegion);

who_regions_unique = unique(who_regions);

x = (1:1:length(who_regions_unique))';
y = x*0;

for i_wr_counter = 1 : length(x)
    i_wr = who_regions_unique(i_wr_counter);
    y(i_wr_counter) = sum(strcmp(i_wr, who_regions));
end

[~,ind] = sort(y, 'descend');

X = who_regions_unique(ind);
Y = y(ind);

subplot(2,2,1)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% b. country

countries = string(T.p_Country);

countries_unique = unique(countries);


x = (1:1:length(countries_unique))';
y = x*0;

for i_country_counter = 1 : length(x)
    i_country = countries_unique(i_country_counter);
    y(i_country_counter) = sum(strcmp(i_country, countries));
end

Country = countries_unique;
Count = y;

country_count_table = table(Country, Count);
writetable(country_count_table, strcat(output_folder, 'fig_3b_country_count_table.csv'));

% c. sex

sex = string(T.p_Sex);

sex_unique = unique(sex);

x = (1:1:length(sex_unique))';
y = x*0;

for i_sex_counter = 1 : length(x)
    i_sex = sex_unique(i_sex_counter);
    y(i_sex_counter) = sum(strcmp(i_sex, sex));
end

[~,ind] = sort(y, 'descend');

X = sex_unique(ind);
Y = y(ind);

subplot(2,2,3)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% d. sample size

sample_sizes = T.p_SampleSize;

subplot(2,2,4)

raincloud_plot(sample_sizes, 'box_on', 1, 'color', blue_colour, ...
    'box_dodge', 1, 'box_dodge_amount', 0.3, 'dot_dodge_amount', 0.6);
xlim([-2.5 410])
xlabel("Sample size");
yticks([]);
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% improve figure layout

set(gcf,...
    'Units', 'Inches', ...
    'Position', [0, 0, fig_size_x, fig_size_y], ...
    'PaperPositionMode', 'auto');

a = annotation('textbox', [section_labels_x(1), section_labels_y(1), 0, 0], 'string', 'a.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(1), 0, 0], 'string', 'b.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(1), section_labels_y(2), 0, 0], 'string', 'c.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(2), 0, 0], 'string', 'd.');
a.FontSize = section_labels_fontsize;


% save figure

exportgraphics(gcf, file_name);

%% Figure 4: Intervention and comparator

file_name = strcat(output_folder, 'fig_4_int_and_comp.pdf');

figure;

% a. menthol vehicle

vehicles = string(T.i_MentholVehicle);
to_delete = strcmp(vehicles, 'N/A');
vehicles(to_delete) = [];

vehicles_unique = unique(vehicles);

x = (1:1:length(vehicles_unique))';
y = x*0;

for i_vehicles_counter = 1 : length(x)
    i_vehicles = vehicles_unique(i_vehicles_counter);
    y(i_vehicles_counter) = sum(strcmp(i_vehicles, vehicles));
end

[~,ind] = sort(y, 'descend');

X = vehicles_unique(ind);
Y = y(ind);

subplot(2,2,1)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% b. menthol concentrations

concentrations = T.i_MentholConcentrationPct;

subplot(2,2,2)

conc_num_non_nan = sum(~isnan(concentrations));

if conc_num_non_nan >= 2
    raincloud_plot(concentrations, 'box_on', 1, 'color', blue_colour, ...
        'box_dodge', 1, 'box_dodge_amount', 0.3, 'dot_dodge_amount', 0.6);
    xlim([-0.5 43]);
    xlabel("Menthol concentration (%)");
    yticks([]);
    box("off")
elseif conc_num_non_nan == 1
    histogram(concentrations, 'FaceColor', blue_colour, 'FaceAlpha', 1);
    xlim([-0.5 43])
    xlabel("Menthol concentration (%)");
    yticks([]);
    box("off")
end

ax = gca;
ax.FontSize = axis_font_size;

% c. comparator type intermediate - inactive

comparators_broad = string(T.c_ComparatorType_broad);
comparators_intermediate = string(T.c_ComparatorType_intermediate);
to_delete = strcmp(comparators_broad, 'N/A');
comparators_broad(to_delete) = [];
comparators_intermediate(to_delete) = [];

comparators_broad_inactive_idx = strcmp(comparators_broad, "Inactive");
comparators_inactive_intermediate = comparators_intermediate(comparators_broad_inactive_idx);

comparators_inactive_intermediate_unique = unique(comparators_inactive_intermediate);

x = (1:1:length(comparators_inactive_intermediate_unique))';
y = x*0;

for i_cii_counter = 1 : length(x)
    i_cii = comparators_inactive_intermediate_unique(i_cii_counter);
    y(i_cii_counter) = sum(strcmp(i_cii, comparators_inactive_intermediate));
end

[~,ind] = sort(y, 'descend');

X = comparators_inactive_intermediate_unique(ind);
Y = y(ind);

subplot(2,2,3)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;


% c. comparator type intermediate - active

comparators_broad = string(T.c_ComparatorType_broad);
comparators_intermediate = string(T.c_ComparatorType_intermediate);
to_delete = strcmp(comparators_broad, 'N/A');
comparators_broad(to_delete) = [];
comparators_intermediate(to_delete) = [];

comparators_broad_active_idx = strcmp(comparators_broad, "Active");
comparators_active_intermediate = comparators_intermediate(comparators_broad_active_idx);

comparators_active_intermediate_unique = unique(comparators_active_intermediate);

x = (1:1:length(comparators_active_intermediate_unique))';
y = x*0;

for i_cai_counter = 1 : length(x)
    i_cai = comparators_active_intermediate_unique(i_cai_counter);
    y(i_cai_counter) = sum(strcmp(i_cai, comparators_active_intermediate));
end

[~,ind] = sort(y, 'descend');

X = comparators_active_intermediate_unique(ind);
Y = y(ind);

subplot(2,2,4)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% improve figure layout

set(gcf,...
    'Units', 'Inches', ...
    'Position', [0, 0, fig_size_x, fig_size_y], ...
    'PaperPositionMode', 'auto');

a = annotation('textbox', [section_labels_x(1), section_labels_y(1), 0, 0], 'string', 'a.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(1), 0, 0], 'string', 'b.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(1), section_labels_y(2), 0, 0], 'string', 'c.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(2), 0, 0], 'string', 'd.');
a.FontSize = section_labels_fontsize;


% save figure

exportgraphics(gcf, file_name);

%% Figure 5: Outcome

file_name = strcat(output_folder, 'fig_5_outcome.pdf');

figure;

% a. efficacy direction

effect_direction = string(T.o_EfficacyDirection);

effect_direction_unique = unique(effect_direction);

x = (1:1:length(effect_direction_unique))';
y = x*0;

for i_ed_counter = 1 : length(x)
    i_ed = effect_direction_unique(i_ed_counter);
    y(i_ed_counter) = sum(strcmp(i_ed, effect_direction));
end

[~,ind] = sort(y, 'descend');

X = effect_direction_unique(ind);
Y = y(ind);

subplot(2,2,1)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;


% b. efficacy significance

effect_significance = string(T.o_EfficacySignificance);

effect_significance_unique = unique(effect_significance);

x = (1:1:length(effect_significance_unique))';
y = x*0;

for i_es_counter = 1 : length(x)
    i_es = effect_significance_unique(i_es_counter);
    y(i_es_counter) = sum(strcmp(i_es, effect_significance));
end

[~,ind] = sort(y, 'descend');

X = effect_significance_unique(ind);
Y = y(ind);

subplot(2,2,2)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% c. pre-registration

prereg = string(T.q_Preregistration);

prereg_unique = unique(prereg);

x = (1:1:length(prereg_unique))';
y = x*0;

for i_prereg_counter = 1 : length(x)
    i_prereg = prereg_unique(i_prereg_counter);
    y(i_prereg_counter) = sum(strcmp(i_prereg, prereg));
end

[~,ind] = sort(y, 'descend');

X = prereg_unique(ind);
Y = y(ind);

subplot(2,2,3)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% d. safety

AEs = string(T.o_AdverseEvents);

AEs_unique = unique(AEs);

x = (1:1:length(AEs_unique))';
y = x*0;

for i_AEs_counter = 1 : length(x)
    i_AEs = AEs_unique(i_AEs_counter);
    y(i_AEs_counter) = sum(strcmp(i_AEs, AEs));
end

[~,ind] = sort(y, 'descend');

X = AEs_unique(ind);
Y = y(ind);

subplot(2,2,4)

bar(X,Y, 'FaceColor',blue_colour);
ylabel("Count");
box("off")

ax = gca;
ax.FontSize = axis_font_size;

% improve figure layout

set(gcf,...
    'Units', 'Inches', ...
    'Position', [0, 0, fig_size_x, fig_size_y], ...
    'PaperPositionMode', 'auto');

a = annotation('textbox', [section_labels_x(1), section_labels_y(1), 0, 0], 'string', 'a.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(1), 0, 0], 'string', 'b.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(1), section_labels_y(2), 0, 0], 'string', 'c.');
a.FontSize = section_labels_fontsize;

a = annotation('textbox', [section_labels_x(2), section_labels_y(2), 0, 0], 'string', 'd.');
a.FontSize = section_labels_fontsize;


% save figure

exportgraphics(gcf, file_name);
