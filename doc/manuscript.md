# Examining the bidirectional relationship between sleep and physical activity using pooled individual data


<div class="no-count">

Taren Sanders PhD<sup>1</sup>, Timothy Hartwig PhD<sup>2</sup>, James
Conigrave PhD<sup>3</sup>, Borja del Pozo Cruz PhD<sup>4</sup>, Philip
Parker PhD<sup>5</sup>, Michael Noetel PhD<sup>6</sup>, Devan Antczak
PhD<sup>7</sup>, Stuart J. Fairclough PhD<sup>8</sup>, Shona Halson
PhD<sup>2</sup>, ***DATASET PROVIDERS TBD***<sup>9</sup>, Chris Lonsdale
PhD<sup>10</sup>

<sup>1</sup>Institute for Positive Psychology and Education, Australian
Catholic University, North Sydney, Australia\
<sup>2</sup>School of Behavioural and Health Science, Australian
Catholic University, North Sydney, Australia\
<sup>3</sup>Centre for Alcohol Policy Research, La Trobe University,
Bundoora, Australia\
<sup>4</sup>Department of Sport Sciences, Faculty of Medicine, Health,
and Sports, Universidad Europea de Madrid\
<sup>5</sup>Office of the Deputy Vice-Chancellor (Research and
Enterprise), Australian Catholic University, North Sydney, Australia\
<sup>6</sup>School of Psychology, University of Queensland, Brisbane,
Australia\
<sup>7</sup>School of Education, University of Wollongong, Wollongong,
Australia\
<sup>8</sup>Department of Sport and Physical Activity, Edge Hill
University, Ormskirk, United Kingdom\
<sup>9</sup>***TBD***\
<sup>10</sup>Office of the Provost, Australian Catholic University,
North Sydney, Australia

Correspondence to: Taren Sanders, 33 Berry St, North Sydney, NSW,
Australia; Taren.Sanders@acu.edu.au, +61 2 9701 4704

Main text: 3317 words

</div>



<div class="no-count">

# Summary

**Background** Physical activity is recommended to improve sleep in
adults, but evidence in children and adolescents is unclear and largely
self-reported. We examined day-to-day, bidirectional associations
between device-measured physical activity and sleep across the lifespan.

**Methods** We pooled individual participant data from studies using
research-grade accelerometers worn 24 hours a day for at least four
days. We derived daily physical activity volume and intensity, and sleep
duration, efficiency, onset, and regularity, from harmonised raw data.
Mixed-effects models with age interactions estimated associations of
physical activity with that night’s sleep and of sleep with next-day
activity, adjusting for sex, BMI, and socioeconomic status. The protocol
was pre-registered (https://osf.io/ahd96).

**Findings** We included 27,236 participants aged 2 to 94 years from 20
studies. Higher physical activity volume was associated with longer,
more efficient, earlier, and more regular sleep. The association with
sleep regularity was strongest and similar at ages 11 and 65 years
(standardised β \[95% CI\] 0.30 \[0.29, 0.31\] and 0.27 \[0.25, 0.29\]),
whereas that of intensity declined from 0.26 \[0.25, 0.28\] to 0.00
\[-0.02, 0.01\]. More regular sleep was followed by more next-day
physical activity (0.14 \[0.13, 0.14\] and 0.09 \[0.08, 0.10\],
respectively); associations with other sleep measures were small.

**Interpretation** Physical activity was associated most consistently
with more regular sleep, at all ages and in both directions. Public
health messages could promote physical activity for regular sleep,
rather than as a universal sleep remedy.

**Funding** This research was unfunded. Refer to the included studies
for their funding sources.

</div>



<div class="no-count">

# Research in context

## Evidence before this study

Meta-analyses of trials in adults show that physical activity improves
sleep, with small effects on sleep duration and efficiency and the
largest benefits in people who sleep poorly, and the WHO recommends
physical activity for this purpose. In children and adolescents the
evidence is less clear. Meta-analyses of observational studies find
inconsistent associations. Trials suggest that exercise improves sleep
quality and efficiency but not total sleep time, and guidelines do not
recommend physical activity to improve sleep in these age groups. Most
studies relied on self-reported sleep or physical activity, and
meta-analyses that pool study-level effects cannot show how associations
change with age. To understand if the relationship runs in both
directions needs device-measured, day-level data. These have been
synthesised separately in adults, where a meta-analysis of 33 studies
did not support a bidirectional daily association, and in children and
youth. Recent studies of consumer wearables with millions of
person-nights included only adults and did not examine sleep regularity.

## Added value of this study

To our knowledge, this is the first study to examine day-to-day
associations between device-measured physical activity and sleep in a
large, geographically diverse sample spanning the lifespan. We
reprocessed raw accelerometer data from 20 studies with a common method,
giving daily data for 27,236 participants aged 2 to 94 years. Higher
physical activity volume was associated with longer, more efficient,
earlier, and more regular sleep that night. The association with sleep
regularity was the strongest and changed little with age, whereas the
association of physical activity intensity with sleep regularity, and
those of sleep with next-day physical activity, weakened with age. More
regular sleep was followed by more physical activity the next day, so
the link between physical activity and sleep regularity appeared to run
in both directions. All associations were small: at age 11 years, a 1 SD
higher physical activity volume was associated with 4.7 points higher
sleep regularity and 11.8 min longer sleep.

## Implications of all the available evidence

Trials show that physical activity improves sleep in adults, and our
findings show that its most consistent association, at every age, is
with more regular sleep. Irregular sleep predicts mortality,
cardiovascular events, and depression independently of sleep duration,
so this association may matter for population health. Public health
messages should present physical activity as a way to support consistent
sleep schedules, as Australia’s 24-hour movement guidelines do, rather
than as a universal remedy for poor sleep. That our strongest
association varied little with age suggests that the weaker evidence in
children and adolescents may reflect study design or measurement rather
than age. Trials should test whether increasing physical activity
improves sleep regularity, whether more regular sleep in turn supports
physical activity, and whether either benefits health beyond sleep,
particularly in adolescents and young adults, who were under-represented
in our data.

</div>



# Introduction

Sleep is an important determinant of population health across the
lifespan: poor sleep is associated with all-cause, cardiovascular
disease, and cancer mortality<sup>1</sup> and with poor mental
health.<sup>2</sup> Yet 14-68% of children and adolescents<sup>3</sup>
and 31% of adults<sup>4</sup> sleep less than recommended, and safe,
accessible strategies to promote sleep are needed.<sup>5,6</sup> In
adults, physical activity appears to improve both the quantity and
quality of sleep,<sup>7,8</sup> and WHO recommends it for this
purpose.<sup>9</sup> In children and adolescents, the evidence is less
clear.<sup>6,10,11</sup> Trials suggest exercise improves sleep quality
and efficiency but not total sleep time,<sup>12</sup> and guidelines do
not support a role for physical activity in improving sleep in this age
group.

The existing evidence has three main limitations. First, most studies
relied on self-reported sleep and physical activity,<sup>6,10,11</sup>
which could bias associations and limit the precision of dose-response
analyses. Second, existing reviews and meta-analyses<sup>6</sup> pool
study-level effects, which limits how well they can examine the
influence of age.<sup>13</sup> Third, no study has examined day-to-day
data in a large, geographically diverse sample spanning the lifespan.
Daily associations have been synthesised separately in
adults<sup>14</sup> and in children and youth,<sup>15</sup> and
consumer-wearable studies with millions of person-nights included only
adults and did not examine sleep regularity.<sup>16,17</sup> Day-level
data matter because the relationship may run in both directions.
Physical activity may promote sleep through circadian and stress-related
pathways,<sup>18</sup> while a late or poor night’s sleep may reduce
activity the next day.<sup>14</sup> Weekly averages may mask these
patterns, and pooling device-measured individual participant data can
overcome these limitations.<sup>13</sup>

We harmonised individual participant data from accelerometer studies
around the world to investigate the relationship between device-measured
physical activity and sleep across the lifespan. We addressed three
research questions (RQs): (RQ1) do individuals who are more physically
active have better sleep duration and quality; (RQ2) does age moderate
the association between physical activity and sleep; and (RQ3) is the
association bidirectional?

# Methods

We pre-registered all methods and analyses on the Open Science Framework
(OSF) before data acquisition began (https://osf.io/ahd96).

## Data sources and data acquisition

We included studies of any design that used research-grade
accelerometers of any brand, worn at any location (hip, wrist, or
thigh), from which raw accelerations ($m/s^2$ or $mg$) could be
obtained, with a 24-hour wear protocol over at least four days,
including one weekend day.

We identified potentially relevant studies by examining the reference
lists of recent systematic reviews that explored the relationship
between sleep and physical activity in children, adolescents, and
adults.<sup>7,10,11</sup> We then used forward searching on potentially
eligible studies. We additionally performed a literature search to
identify accelerometry consortiums and individual studies, searched
trial registries (the Australian New Zealand Clinical Trials Registry,
the World Health Organization International Clinical Trials Registry
Platform, and the ISRCTN registry) and the Open Science Framework for
unpublished results, and asked collaborators to help identify any
additional studies.

To keep data acquisition manageable, we invited only studies with more
than 400 participants, emailing chief investigators or consortium leads
to request deidentified individual data. All studies included in the
pooled data meta-analysis had to have been approved by their local or
institutional ethical committees, and all participants (or, for
children, their parents or guardians) had to have provided written
informed consent, with children providing assent where required. The
Australian Catholic University’s Research Ethics Committee granted
permission for each study’s data to be obtained (approval: 2020-143N),
and investigators transferred their deidentified data via a secure
file-sharing system.

## Data harmonisation and outcomes

We harmonised all accelerometer data by reprocessing the raw files with
the open-source *GGIR* package (version 2.5-1) in R,<sup>19,20</sup> to
produce outcomes as comparable as possible across studies, device wear
locations, and populations. Processing used our *sleepIPD* R package,
which fixed all settings across studies and documents the decisions made
for specific devices.<sup>21</sup> The core research team or the data
contributors ran *sleepIPD* on each study’s raw files. *GGIR*’s sleep
detection has been evaluated against polysomnography in both
adults<sup>20</sup> and children.<sup>22</sup> Non-wear time and
abnormally high accelerations were detected with the default settings,
classified as missing, and imputed from the same time interval on the
remaining days of the week.<sup>19</sup>

Each day ran from waking to waking the following day, so each day’s
physical activity, calculated over its waking hours, was paired with the
sleep period that followed it. For wrist-worn devices, the sleep period
was detected with the HDCZA algorithm<sup>20</sup> and for hip-worn
devices *GGIR* instead detected time in bed from the device’s angle. For
sleep, *GGIR* produced total sleep time (minutes), sleep efficiency
(percentage of the sleep period, from sleep onset to final waking, spent
asleep), sleep onset (clock time), and sleep regularity (the sleep
regularity index: the likelihood of being in the same sleep or wake
state at time points 24 hours apart, calculated for each pair of
consecutive days, with higher values indicating more regular sleep). For
physical activity, we used two metrics proposed by Rowlands and
colleagues:<sup>23</sup> average acceleration (mg)—a measure of
volume—and the intensity gradient (unitless), the slope of log time
against log intensity, which describes how time is distributed across
intensities, with less negative values indicating relatively more time
at higher intensities. These metrics capture the whole intensity
continuum without the population-specific cut-points that collapse it
into categories.

## Covariates and moderators

The following individual participant data were obtained when available:
age, sex, height, weight, waist circumference, maturational status,
socioeconomic status (SES), ethnicity, screen time, sleep medications,
and medically diagnosed conditions known to affect sleep. Sex was
ascertained from the original studies, which primarily used self- or
parent-report for sex assigned at birth. The country, city, time zone,
daylight length, and season in which data were collected were also
obtained. Because studies measured socioeconomic status differently
(e.g., education or income), we harmonised each study’s measure into
low, medium, and high categories (appendix).

Body mass index (BMI) was converted to an age- and sex-standardised
z-score. For participants under 18 years we used the CDC 2000
BMI-for-age growth reference, applied with the *childsds* package. For
adults we standardised BMI against the adults in this sample.

## Statistical analysis

To reduce the risk of bias, we used a blind analysis approach: we
developed and tested models on a 10% holdout subsample of the dataset
and conducted the final analysis once we believed the code was ready.
Data screening had four stages: values outside fixed physiological
limits (for average acceleration, above 200 mg); all physical activity
variables from files whose mean average acceleration exceeded that
limit, indicating a likely device failure (76 participants); values more
than 4 SD from the mean of their own study, so that between-study
differences in device, protocol, and population were not mistaken for
implausible measurement; and, for participants under 18 years, heights,
weights, and BMI values that the CDC 2000 growth reference classifies as
biologically implausible.

A day was valid if it recorded more than 10 hours of wear time and,
where sleep was detected, a sleep period longer than 200 minutes.
Participants required at least 4 valid days. A sensitivity analysis
restricted the sample to the 14,981 participants with at least seven
valid days.

We assumed missing covariate and moderator data to be missing at random
or completely at random and imputed them using multilevel predictive
mean matching, clustered by participant, with multiple imputation by
chained equations in the *mice* package, creating 50 imputed datasets.
The imputation model also included the sleep and physical activity
variables, but each analysis used only the person-days on which its
outcome, its sleep or physical activity predictor, and its moderator
were observed, so imputed values are only covariates. We pooled results
across imputed datasets with Rubin’s rules, computing 95% CIs and p
values on the Barnard-Rubin degrees of freedom to account for the
additional uncertainty introduced by imputation, and used p\<0.05 to
assess statistical significance. We fitted 16 models for each moderator
and report all of them without adjustment for multiple comparisons, so p
values close to 0.05 should be interpreted with caution. We used R
(version 4.6.1) for all data cleaning and analysis.

To examine the relationship between sleep and physical activity we ran
mixed-effects models with random intercepts for study and participant,
accounting for the nesting of days within participants and of
participants within studies. We predicted each sleep indicator from each
physical activity indicator, adjusting for SES, sex, and BMI z-score,
with quadratic terms to allow curvilinear associations. We interacted
each predictor with age to examine how associations change over the
lifespan (RQ2). Age was centred at its mean across person-days (35.6
years) and scaled per 10 years. For interpretability, we report
associations at ages 11, 18, 35, and 65 years. Models predicting
physical activity from the preceding night’s sleep (RQ3) used each sleep
indicator lagged by one day, so person-days whose preceding day was
absent or ineligible did not contribute, leaving 136,319 of 168,613
eligible person-days. Sleep regularity was missing most often, so its
models with physical activity volume used 125,155 person-days (RQ1) and
109,033 (RQ3).

## Protocol deviations

This paper addresses the first two hypotheses in our registered
protocol: that physical activity volume and intensity would be
positively associated with optimum sleep duration and quality, with an
inverted U-shaped dose-response, and that associations would be stronger
in adolescents and adults than in children and older adults. Moderation
by the other pre-registered moderators is reported in the appendix, and
the remaining hypotheses will be addressed in future research. While
working with the holdout data we made three changes to the protocol. We
modelled study as a random rather than a fixed effect, so that estimates
would generalise beyond the included studies. Sensitivity analyses of
fixed-effect models gave qualitatively similar results. We analysed each
sleep metric separately, because no validated composite sleep indicator
could be estimated from our data. We did not log-transform skewed
variables, because back-transformed predictions followed extreme
exponential patterns near the edges of the data range. Log-scale models
are reported in the appendix. Additionally, after running the full
analysis we identified a mistake in the BMI variable and in the
imputation model, which we corrected and reran the analysis.

## Role of the funding source

There was no funding source for this study.

# Results

The pooled dataset contained 220,988 days of physical activity and sleep
data from 28,824 participants in 20 studies. We excluded 52,375 days
with insufficient wear time, which removed 1,175 participants entirely,
and a further 413 participants (619 days) with fewer than 4 valid days,
leaving 27,236 participants and 167,994 person-days
(<a href="#tbl-demo-participants" class="quarto-xref">Table 1</a>; study
characteristics are in the appendix). Participants were aged 2 to 94
years and 50.3% were female. Most came from Europe and South America,
followed by Oceania, and the sample skewed towards younger children
(2-11 years, 50.9%) and older adults (66+ years, 22.3%).

<div class="landscape">

<div id="tbl-demo-participants">

Table 1: Participant characteristics

<div class="cell-output-display">

<table class="cell no-count" style="width:97%;">
<colgroup>
<col style="width: 31%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
<col style="width: 13%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr>
<th></th>
<th colspan="5">Age group</th>
</tr>
<tr>
<th>Characteristic</th>
<th>2-11 years</th>
<th>12-18 years</th>
<th>19-35 years</th>
<th>36-65 years</th>
<th>66+ years</th>
</tr>
</thead>
<tbody>
<tr>
<td colspan="6">Numeric variables</td>
</tr>
<tr>
<td>N</td>
<td>13,820</td>
<td>1,923</td>
<td>310</td>
<td>5,044</td>
<td>6,067</td>
</tr>
<tr>
<td>Age</td>
<td>8.5 (2.8)</td>
<td>15.7 (1.7)</td>
<td>29.2 (5.1)</td>
<td>57.7 (7.4)</td>
<td>72.7 (4.5)</td>
</tr>
<tr>
<td>BMI</td>
<td>17.7 (3.0)</td>
<td>21.6 (3.8)</td>
<td>26.5 (6.5)</td>
<td>26.8 (4.8)</td>
<td>27.1 (4.3)</td>
</tr>
<tr>
<td>BMI z-score</td>
<td>0.31 (1.10)</td>
<td>0.09 (1.02)</td>
<td>-0.07 (1.40)</td>
<td>-0.01 (1.03)</td>
<td>0.05 (0.93)</td>
</tr>
<tr>
<td>Valid Weartime Hours</td>
<td>23.4 (1.3)</td>
<td>23.4 (1.5)</td>
<td>23.3 (1.7)</td>
<td>23.8 (0.7)</td>
<td>23.9 (0.4)</td>
</tr>
<tr>
<td>Valid Weartime Days</td>
<td>5.7 (1.8)</td>
<td>5.0 (1.3)</td>
<td>5.0 (1.4)</td>
<td>7.5 (2.3)</td>
<td>6.6 (2.1)</td>
</tr>
<tr>
<td>PA Intensity Gradient (unitless)</td>
<td>-2.10 (0.18)</td>
<td>-2.23 (0.19)</td>
<td>-2.43 (0.19)</td>
<td>-2.50 (0.19)</td>
<td>-2.68 (0.22)</td>
</tr>
<tr>
<td>PA Volume (average acceleration in mg)</td>
<td>49.0 (26.4)</td>
<td>46.7 (14.3)</td>
<td>39.3 (10.6)</td>
<td>38.8 (11.6)</td>
<td>30.9 (9.7)</td>
</tr>
<tr>
<td>Sleep Duration (min)</td>
<td>471.7 (61.5)</td>
<td>416.4 (55.0)</td>
<td>421.0 (64.0)</td>
<td>409.1 (55.8)</td>
<td>404.3 (63.9)</td>
</tr>
<tr>
<td>Sleep Efficiency (%)</td>
<td>80.9 (7.0)</td>
<td>87.1 (6.3)</td>
<td>87.4 (6.2)</td>
<td>87.8 (5.5)</td>
<td>86.4 (6.2)</td>
</tr>
<tr>
<td>Sleep Onset (clock time; SD in h:mm)</td>
<td>21:32 (01:42)</td>
<td>23:29 (01:27)</td>
<td>23:33 (01:49)</td>
<td>23:25 (01:17)</td>
<td>23:57 (01:16)</td>
</tr>
<tr>
<td>Sleep Regularity Index</td>
<td>53.4 (12.2)</td>
<td>57.0 (12.1)</td>
<td>55.8 (12.8)</td>
<td>59.4 (11.4)</td>
<td>54.5 (12.5)</td>
</tr>
<tr>
<td colspan="6">Accelerometer Wear Location</td>
</tr>
<tr>
<td>Wrist</td>
<td>5,509 (39.9%)</td>
<td>1,885 (98.0%)</td>
<td>310 (100.0%)</td>
<td>5,044 (100.0%)</td>
<td>6,067 (100.0%)</td>
</tr>
<tr>
<td>Hip</td>
<td>8,311 (60.1%)</td>
<td>38 (2.0%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td colspan="6">Country</td>
</tr>
<tr>
<td>Australia</td>
<td>1,510 (10.9%)</td>
<td>305 (15.9%)</td>
<td>110 (35.5%)</td>
<td>188 (3.7%)</td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Finland</td>
<td>508 (3.7%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td>801 (15.9%)</td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>New Zealand</td>
<td>455 (3.3%)</td>
<td><ul>
<li></li>
</ul></td>
<td>37 (11.9%)</td>
<td>152 (3.0%)</td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Spain</td>
<td>809 (5.9%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td>37 (0.7%)</td>
<td>2,386 (39.3%)</td>
</tr>
<tr>
<td>Switzerland</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td>1,905 (37.8%)</td>
<td>1,023 (16.9%)</td>
</tr>
<tr>
<td>United Kingdom</td>
<td>447 (3.2%)</td>
<td><ul>
<li></li>
</ul></td>
<td>144 (46.5%)</td>
<td>1,961 (38.9%)</td>
<td>2,658 (43.8%)</td>
</tr>
<tr>
<td>Brazil</td>
<td>3,425 (24.8%)</td>
<td>727 (37.8%)</td>
<td>10 (3.2%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Czech Republic</td>
<td>281 (2.0%)</td>
<td>848 (44.1%)</td>
<td>9 (2.9%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Canada</td>
<td>522 (3.8%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Chile</td>
<td>138 (1.0%)</td>
<td>5 (0.3%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>China</td>
<td>125 (0.9%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Colombia</td>
<td>889 (6.4%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Denmark</td>
<td>689 (5.0%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Ecuador</td>
<td>981 (7.1%)</td>
<td>38 (2.0%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>India</td>
<td>575 (4.2%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Kenya</td>
<td>528 (3.8%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Portugal</td>
<td>702 (5.1%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>South Africa</td>
<td>507 (3.7%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>United States</td>
<td>729 (5.3%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td colspan="6">Ethnicity</td>
</tr>
<tr>
<td>Non-White</td>
<td>2,672 (19.3%)</td>
<td>169 (8.8%)</td>
<td>14 (4.5%)</td>
<td>94 (1.9%)</td>
<td>240 (4.0%)</td>
</tr>
<tr>
<td>Other or unknown</td>
<td>4,780 (34.6%)</td>
<td>1,194 (62.1%)</td>
<td>154 (49.7%)</td>
<td>1,340 (26.6%)</td>
<td>2,424 (40.0%)</td>
</tr>
<tr>
<td>White</td>
<td>6,368 (46.1%)</td>
<td>560 (29.1%)</td>
<td>142 (45.8%)</td>
<td>3,610 (71.6%)</td>
<td>3,403 (56.1%)</td>
</tr>
<tr>
<td colspan="6">Region</td>
</tr>
<tr>
<td>Europe</td>
<td>3,436 (24.9%)</td>
<td>848 (44.1%)</td>
<td>153 (49.4%)</td>
<td>4,704 (93.3%)</td>
<td>6,067 (100.0%)</td>
</tr>
<tr>
<td>Oceania</td>
<td>1,965 (14.2%)</td>
<td>305 (15.9%)</td>
<td>147 (47.4%)</td>
<td>340 (6.7%)</td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>South America</td>
<td>5,433 (39.3%)</td>
<td>770 (40.0%)</td>
<td>10 (3.2%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Africa</td>
<td>1,035 (7.5%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>Asia</td>
<td>700 (5.1%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td>North America</td>
<td>1,251 (9.1%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
</tr>
<tr>
<td colspan="6">Season</td>
</tr>
<tr>
<td>Autumn</td>
<td>4,672 (33.8%)</td>
<td>445 (23.1%)</td>
<td>39 (12.6%)</td>
<td>1,241 (24.6%)</td>
<td>1,268 (20.9%)</td>
</tr>
<tr>
<td>Spring</td>
<td>2,859 (20.7%)</td>
<td>1,230 (64.0%)</td>
<td>125 (40.3%)</td>
<td>1,226 (24.3%)</td>
<td>1,988 (32.8%)</td>
</tr>
<tr>
<td>Summer</td>
<td>2,113 (15.3%)</td>
<td>82 (4.3%)</td>
<td>85 (27.4%)</td>
<td>1,271 (25.2%)</td>
<td>1,252 (20.6%)</td>
</tr>
<tr>
<td>Winter</td>
<td>4,176 (30.2%)</td>
<td>166 (8.6%)</td>
<td>61 (19.7%)</td>
<td>1,306 (25.9%)</td>
<td>1,559 (25.7%)</td>
</tr>
<tr>
<td colspan="6">Sex</td>
</tr>
<tr>
<td>Female</td>
<td>7,154 (51.8%)</td>
<td>1,002 (52.1%)</td>
<td>222 (71.6%)</td>
<td>2,717 (53.9%)</td>
<td>2,551 (42.0%)</td>
</tr>
<tr>
<td>Male</td>
<td>6,664 (48.2%)</td>
<td>921 (47.9%)</td>
<td>88 (28.4%)</td>
<td>2,327 (46.1%)</td>
<td>3,516 (58.0%)</td>
</tr>
<tr>
<td colspan="6">Sleep Conditions Reported</td>
</tr>
<tr>
<td>No</td>
<td>625 (4.5%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td>3,512 (69.6%)</td>
<td>5,141 (84.7%)</td>
</tr>
<tr>
<td>Yes</td>
<td>50 (0.4%)</td>
<td><ul>
<li></li>
</ul></td>
<td><ul>
<li></li>
</ul></td>
<td>310 (6.1%)</td>
<td>626 (10.3%)</td>
</tr>
<tr>
<td>Not Reported</td>
<td>13,145 (95.1%)</td>
<td>1,923 (100.0%)</td>
<td>310 (100.0%)</td>
<td>1,222 (24.2%)</td>
<td>300 (4.9%)</td>
</tr>
<tr>
<td colspan="6">Socioeconomic Status</td>
</tr>
<tr>
<td>Low</td>
<td>2,892 (24.1%)</td>
<td>208 (11.5%)</td>
<td>2 (0.7%)</td>
<td>1,074 (22.0%)</td>
<td>2,388 (39.4%)</td>
</tr>
<tr>
<td>Medium</td>
<td>4,638 (38.7%)</td>
<td>885 (49.1%)</td>
<td>191 (70.2%)</td>
<td>2,409 (49.3%)</td>
<td>2,325 (38.3%)</td>
</tr>
<tr>
<td>High</td>
<td>4,456 (37.2%)</td>
<td>708 (39.3%)</td>
<td>79 (29.0%)</td>
<td>1,406 (28.8%)</td>
<td>1,352 (22.3%)</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="6">For categorical variables the value is the count, and
percentage. For numeric variables the value is the Mean and SD. Sleep
onset is a clock time; its SD is a duration in hours:minutes. Total N =
27,236. 72 participants had missing age data and are counted in the
total but not in any age band.</td>
</tr>
</tfoot>
&#10;</table>

</div>

</div>

</div>

## Physical activity volume and sleep

Higher physical activity volume was associated with longer sleep
duration, higher sleep efficiency, earlier sleep onset, and more regular
sleep (<a href="#tbl-sleep-outcomes" class="quarto-xref">Table 2</a>;
<a href="#fig-sleep-by-volume" class="quarto-xref">Figure 1</a>).
Curvilinear terms were statistically significant but did not
meaningfully change these associations. The association with sleep
regularity was the largest and declined only slightly with age ($\beta$
= 0.30 \[0.29, 0.31\] at age 11 and 0.27 \[0.25, 0.29\] at age 65). The
associations with sleep duration and efficiency weakened with age, and
by age 65 higher volume was associated with slightly lower sleep
efficiency ($\beta$ = -0.03 \[-0.04, -0.02\]), whereas the association
with earlier sleep onset strengthened with age ($\beta$ = -0.09 \[-0.10,
-0.08\] at age 11 and -0.15 \[-0.16, -0.14\] at age 65).

<div class="landscape">

<div id="tbl-sleep-outcomes">

Table 2: Physical activity predicting sleep controlling for SES, sex,
and BMI z-score.

<div class="cell-output-display">

<table class="cell no-count" style="width:98%;">
<colgroup>
<col style="width: 21%" />
<col style="width: 21%" />
<col style="width: 14%" />
<col style="width: 23%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr>
<th>Term</th>
<th>PA volume (z) β [95% CI]</th>
<th>PA volume (z) p</th>
<th>PA intensity (z) β [95% CI]</th>
<th>PA intensity (z) p</th>
</tr>
</thead>
<tbody>
<tr>
<td colspan="5">Sleep duration (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.00 [-0.16, 0.16]</td>
<td>.996</td>
<td>0.00 [-0.15, 0.14]</td>
<td>.995</td>
</tr>
<tr>
<td>Physical activity</td>
<td>0.10 [0.09, 0.11]</td>
<td>&lt; .001</td>
<td>0.05 [0.05, 0.06]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.02 [-0.03, 0.00]</td>
<td>.007</td>
<td>-0.02 [-0.03, -0.01]</td>
<td>.003</td>
</tr>
<tr>
<td>Physical activity²</td>
<td>-0.02 [-0.02, -0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.01]</td>
<td>.612</td>
</tr>
<tr>
<td>Physical activity × Age</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity²</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.001</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep efficiency (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.15 [-0.01, 0.31]</td>
<td>.062</td>
<td>0.15 [-0.02, 0.32]</td>
<td>.086</td>
</tr>
<tr>
<td>Physical activity</td>
<td>0.03 [0.03, 0.04]</td>
<td>&lt; .001</td>
<td>0.02 [0.02, 0.03]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.01 [-0.02, 0.01]</td>
<td>.323</td>
<td>-0.01 [-0.03, 0.00]</td>
<td>.067</td>
</tr>
<tr>
<td>Physical activity²</td>
<td>-0.02 [-0.02, -0.01]</td>
<td>&lt; .001</td>
<td>-0.02 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity × Age</td>
<td>-0.02 [-0.02, -0.02]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.095</td>
<td>0.01 [0.00, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep onset (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.09 [-0.18, 0.37]</td>
<td>.503</td>
<td>0.12 [-0.14, 0.37]</td>
<td>.379</td>
</tr>
<tr>
<td>Physical activity</td>
<td>-0.12 [-0.12, -0.11]</td>
<td>&lt; .001</td>
<td>-0.06 [-0.06, -0.05]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.01 [-0.02, 0.00]</td>
<td>.036</td>
<td>0.00 [-0.02, 0.01]</td>
<td>.524</td>
</tr>
<tr>
<td>Physical activity²</td>
<td>0.02 [0.02, 0.03]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity × Age</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
<td>0.01 [0.00, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity²</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep regularity (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.18 [0.04, 0.33]</td>
<td>.014</td>
<td>0.06 [-0.14, 0.25]</td>
<td>.569</td>
</tr>
<tr>
<td>Physical activity</td>
<td>0.29 [0.27, 0.30]</td>
<td>&lt; .001</td>
<td>0.14 [0.13, 0.15]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>0.00 [-0.02, 0.02]</td>
<td>.730</td>
<td>-0.08 [-0.10, -0.06]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity²</td>
<td>-0.15 [-0.16, -0.14]</td>
<td>&lt; .001</td>
<td>-0.09 [-0.09, -0.08]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity × Age</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.049</td>
<td>-0.05 [-0.05, -0.05]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity²</td>
<td>-0.03 [-0.04, -0.03]</td>
<td>&lt; .001</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="5">Adjusted for SES, sex, and BMI z-score. Rows are grouped
by sleep outcome; columns give the physical activity predictor.
Variables marked (z) are standardised; ² denotes a quadratic term and ×
an interaction. Age was centred at 35.6 years (the mean across
person-days) and scaled per 10 years, so main effects are associations
at age 35.6 and age interactions are the change per 10 years.</td>
</tr>
</tfoot>
&#10;</table>

</div>

</div>

</div>

<div id="fig-sleep-by-volume">

<img
src="../Figures/main/Sleep%20on%20scale_pa_volume%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 1: Standardised sleep metrics predicted by physical activity
volume. The panels on the left show the curvilinear relationship between
PA volume and each of the sleep outcomes at the ages indicated in each
column. The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.

</div>

## Physical activity intensity and sleep

Physical activity intensity was weakly associated with sleep duration,
efficiency, and onset, and more strongly with sleep regularity
(<a href="#tbl-sleep-outcomes" class="quarto-xref">Table 2</a>;
<a href="#fig-sleep-by-intensity" class="quarto-xref">Figure 2</a>).
Curvilinear terms were statistically significant for all sleep outcomes
except duration but were meaningful only for sleep regularity. Lower
intensity was associated with poorer regularity, and the benefit of
higher intensity diminished beyond 1 SD above the mean. This association
was steepest in children and young adults, although few of them had very
low intensity. The associations with sleep duration and efficiency
weakened with age, and the association with sleep regularity declined
markedly, from $\beta$ = 0.26 \[0.25, 0.28\] at age 11 to 0.00 \[-0.02,
0.01\] at age 65.

<div id="fig-sleep-by-intensity">

<img
src="../Figures/main/Sleep%20on%20scale_pa_intensity%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 2: Sleep metrics predicted by physical activity intensity. The
panels on the left show the curvilinear relationship between PA
intensity and sleep outcomes at the ages indicated in each column. The
panels on the right show the same relationships but by age continuously.
The white band indicates predictions which were not significantly
different from zero.

</div>

## Sleep and next-day physical activity

There was a weak curvilinear association between sleep duration and
next-day physical activity volume and intensity, with physical activity
highest after nights of slightly longer than average sleep
(<a href="#tbl-pa-outcomes" class="quarto-xref">Table 3</a>;
<a href="#fig-pa-by-sleep-duration" class="quarto-xref">Figure 3</a>).
The association with volume weakened with age and was slightly negative
by age 65 ($\beta$ = 0.05 \[0.04, 0.05\] at age 11 and -0.02 \[-0.02,
-0.01\] at age 65), whereas the association with intensity changed
little with age. Sleep efficiency had a negligible association with
next-day volume at all ages ($\beta$ = -0.01 \[-0.01, 0.00\] at age 11
and 0.01 \[0.01, 0.02\] at age 65) and only a weak positive association
with intensity
(<a href="#fig-pa-by-sleep-efficiency" class="quarto-xref">Figure 4</a>).
Earlier sleep onset was weakly associated with higher next-day volume
and intensity, and these associations were attenuated at older ages
(volume: $\beta$ = -0.03 \[-0.04, -0.02\] at age 11 and -0.01 \[-0.02,
-0.01\] at age 65;
<a href="#fig-pa-by-sleep-onset" class="quarto-xref">Figure 5</a>). By
age 65, earlier onset was associated with marginally lower next-day
intensity ($\beta$ = 0.01 \[0.00, 0.02\]). More regular sleep was
linearly associated with higher next-day volume and intensity
(<a href="#fig-pa-by-sleep-regularity" class="quarto-xref">Figure 6</a>).
These associations weakened with age (volume: $\beta$ = 0.14 \[0.13,
0.14\] at age 11 and 0.09 \[0.08, 0.10\] at age 65), and the association
with intensity was negligible in older adults ($\beta$ = 0.02 \[0.02,
0.03\] at age 65).

<div class="landscape">

<div id="tbl-pa-outcomes">

Table 3: Sleep predicting physical activity controlling for SES, sex,
and BMI z-score

<div class="cell-output-display">

<table class="cell no-count" style="width:98%;">
<colgroup>
<col style="width: 20%" />
<col style="width: 21%" />
<col style="width: 14%" />
<col style="width: 24%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr>
<th>Term</th>
<th>PA volume (z) β [95% CI]</th>
<th>PA volume (z) p</th>
<th>PA intensity (z) β [95% CI]</th>
<th>PA intensity (z) p</th>
</tr>
</thead>
<tbody>
<tr>
<td colspan="5">Sleep duration (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.14 [-0.17, 0.44]</td>
<td>.378</td>
<td>-0.07 [-0.19, 0.05]</td>
<td>.254</td>
</tr>
<tr>
<td>Sleep duration</td>
<td>0.02 [0.01, 0.02]</td>
<td>&lt; .001</td>
<td>0.03 [0.02, 0.03]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.23 [-0.24, -0.22]</td>
<td>&lt; .001</td>
<td>-0.27 [-0.28, -0.26]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep duration²</td>
<td>-0.02 [-0.03, -0.02]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep duration × Age</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.051</td>
</tr>
<tr>
<td>Age × Sleep duration²</td>
<td>0.00 [0.00, 0.00]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.326</td>
</tr>
<tr>
<td colspan="5">Sleep efficiency (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.13 [-0.17, 0.42]</td>
<td>.412</td>
<td>-0.07 [-0.20, 0.05]</td>
<td>.242</td>
</tr>
<tr>
<td>Sleep efficiency</td>
<td>0.00 [0.00, 0.01]</td>
<td>.279</td>
<td>0.01 [0.00, 0.01]</td>
<td>.009</td>
</tr>
<tr>
<td>Age</td>
<td>-0.23 [-0.24, -0.21]</td>
<td>&lt; .001</td>
<td>-0.27 [-0.28, -0.26]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep efficiency²</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep efficiency × Age</td>
<td>0.00 [0.00, 0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.200</td>
</tr>
<tr>
<td>Age × Sleep efficiency²</td>
<td>0.00 [0.00, 0.00]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.227</td>
</tr>
<tr>
<td colspan="5">Sleep onset (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.13 [-0.18, 0.43]</td>
<td>.410</td>
<td>-0.08 [-0.21, 0.04]</td>
<td>.202</td>
</tr>
<tr>
<td>Sleep onset</td>
<td>-0.02 [-0.03, -0.02]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.22 [-0.23, -0.21]</td>
<td>&lt; .001</td>
<td>-0.27 [-0.28, -0.26]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep onset²</td>
<td>0.00 [-0.01, 0.00]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.811</td>
</tr>
<tr>
<td>Sleep onset × Age</td>
<td>0.00 [0.00, 0.00]</td>
<td>.012</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep onset²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.002</td>
<td>0.00 [0.00, 0.00]</td>
<td>.020</td>
</tr>
<tr>
<td colspan="5">Sleep regularity (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.04 [-0.27, 0.34]</td>
<td>.817</td>
<td>-0.18 [-0.30, -0.05]</td>
<td>.006</td>
</tr>
<tr>
<td>Sleep regularity</td>
<td>0.12 [0.11, 0.12]</td>
<td>&lt; .001</td>
<td>0.06 [0.06, 0.07]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.24 [-0.26, -0.22]</td>
<td>&lt; .001</td>
<td>-0.32 [-0.34, -0.31]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep regularity²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.448</td>
<td>0.00 [0.00, 0.00]</td>
<td>.414</td>
</tr>
<tr>
<td>Sleep regularity × Age</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep regularity²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.145</td>
<td>0.00 [0.00, 0.00]</td>
<td>.023</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="5">Adjusted for SES, sex, and BMI z-score. Rows are grouped
by the previous night’s sleep predictor; columns give the physical
activity outcome. Variables marked (z) are standardised; ² denotes a
quadratic term and × an interaction. Age was centred at 35.6 years (the
mean across person-days) and scaled per 10 years, so main effects are
associations at age 35.6 and age interactions are the change per 10
years.</td>
</tr>
</tfoot>
&#10;</table>

</div>

</div>

</div>

<div id="fig-pa-by-sleep-duration">

<img
src="../Figures/main/PA%20on%20scale_sleep_duration_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 3: Physical activity is predicted by the duration of sleep the
previous night. The panels on the left show the curvilinear relationship
between sleep duration and the physical activity outcomes at the ages
indicated in each column. The panels on the right show the same
relationships but by age continuously. The white band indicates
predictions which were not significantly different from zero.

</div>

<div id="fig-pa-by-sleep-efficiency">

<img
src="../Figures/main/PA%20on%20scale_sleep_efficiency_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 4: Physical activity predicted by previous night sleep
efficiency. The panels on the left show the curvilinear relationship
between sleep efficiency and each of the physical activity outcomes at
the ages indicated in each column. The panels on the right show the same
relationships but by age continuously. The white band indicates
predictions which were not significantly different from zero.

</div>

<div id="fig-pa-by-sleep-onset">

<img
src="../Figures/main/PA%20on%20scale_sleep_onset_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 5: Physical activity predicted by previous night sleep onset. The
panels on the left show the curvilinear relationship between sleep onset
and each of the physical activity outcomes at the ages indicated in each
column. The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.

</div>

<div id="fig-pa-by-sleep-regularity">

<img
src="../Figures/main/PA%20on%20scale_sleep_regularity_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 6: Physical activity predicted by previous night sleep
regularity. The panels on the left show the curvilinear relationship
between sleep regularity and each of the physical activity outcomes at
the ages indicated in each column. The panels on the right show the same
relationships but by age continuously. The white band indicates
predictions which were not significantly different from zero.

</div>

# Discussion

Pooling accelerometer data from 20 studies spanning the lifespan, we
found that both the volume and intensity of physical activity were
associated with longer sleep duration, higher sleep efficiency, earlier
sleep onset, and more regular sleep. These relationships were the
strongest for sleep regularity, although the relationship with physical
activity intensity weakened with age. Although robust, these
associations were small: a 1 SD (23.7 mg) higher physical activity
volume was associated with 4.7 points higher sleep regularity, 11.8 min
longer sleep, and 11 min earlier sleep onset at age 11, and with 4.4
points, 4.8 min, and 19 min, respectively, at age 65.

In the reverse direction, more regular sleep was followed by more total
and more intense physical activity the next day, although this
association weakened with age, whereas associations of sleep onset,
duration, and efficiency with next-day physical activity were small. Our
sensitivity analyses suggest that these findings were largely robust to
the decisions made during analysis.

Our findings broadly align with meta-analyses of experimental research,
although the strengths of the relationships we observed were much
weaker.<sup>7,8</sup> For example, Kredlow and colleagues<sup>7</sup>
found small ($0.22 - 0.30$) but significant relationships between
physical activity and sleep duration and efficiency in adults using
experimental studies for both acute and regular physical activity. These
effect sizes are not directly comparable, however, as the experiments
manipulated physical activity, whereas we examined habitual physical
activity. Accelerometry also distinguishes quiet wakefulness from sleep
less well than polysomnography, particularly in children,<sup>22</sup>
which may attenuate associations. Trials also suggest that the benefits
of exercise for sleep are concentrated in people whose sleep is poor at
baseline,<sup>8</sup> whereas our analysis did not target people with
poor sleep. The timing of activity may also matter, as strenuous
exercise close to bedtime can delay and disrupt sleep,<sup>24</sup>
although in our data associations were broadly similar regardless of the
time of day of the most active hour, and across seasons.

Among observational studies of day-to-day sleep and physical activity, a
meta-analysis of 33 studies in adults did not support a bidirectional
daily association.<sup>14</sup> Some sleep parameters were weakly
associated with next-day physical activity, and more active individuals
slept slightly less. Two recent studies of consumer wearables found that
previous-day activity had little association with sleep,<sup>16</sup>
that shorter sleep was followed by more activity the next
day,<sup>16,17</sup> and that earlier-than-usual sleep onset was
followed by more moderate-to-vigorous activity.<sup>17</sup> Our finding
that earlier sleep onset preceded more physical activity is consistent
with these studies, although the association was small in our data.
However, we found clearer associations between physical activity and
that night’s sleep, and physical activity was highest after slightly
longer, rather than shorter, than average sleep. One explanation is that
our models did not separate within-person (day-to-day) from
between-person (habitual) associations, which can differ,<sup>25</sup>
so our estimates partly reflect stable differences between people.

The association between sleep regularity and physical activity appeared
bidirectional. Physical activity predicted more regular sleep that
night, and more regular sleep predicted more physical activity the next
day. This is consistent with the idea that sleep and physical activity
are mutually reinforcing behaviours,<sup>6</sup> or that both reflect
shared underlying factors, such as daily routines, work or school
schedules, and health priorities. Consistent physical activity may also
help stabilise circadian rhythms.<sup>18</sup> Sleep regularity is a
stronger predictor of mortality than sleep duration,<sup>26</sup> and
irregular sleepers remain at higher risk of cardiovascular events and
depression even when they meet sleep duration
recommendations,<sup>2,27</sup> so the finding that physical activity is
associated with sleep regularity may be particularly important.

The association between physical activity volume and sleep regularity
was largely consistent across the lifespan, as a large study of adult
wearable users found for other day-level associations.<sup>16</sup>
Other associations changed more markedly with age: those of physical
activity intensity with sleep regularity, and of sleep with next-day
physical activity, weakened with age and were small or negligible in
older adults, consistent with evidence that the interplay between sleep
and activity differs in very old adults.<sup>25</sup> That our strongest
association varied little with age suggests that the clearer evidence
from meta-analyses in adults than in children and
adolescents<sup>6,7,10,11</sup> may reflect study design or measurement
rather than age.

Strengths of our study include device-measured sleep and physical
activity, harmonised from raw data collected with 24-hour wear protocols
across multiple studies, regions, and age groups, and the avoidance of
intensity cut-points. It also has limitations. Despite our efforts to
harmonise data, variability in accelerometer wear locations and slight
differences in data collection protocols across studies may have
introduced measurement inconsistencies. Physical activity estimates from
different wear locations are strongly correlated but agree less well in
absolute terms, particularly between wrist and hip.<sup>28</sup> To
maximise the data available, we included participants with four or more
days of valid wear time, although some recommend at least seven nights
to estimate sleep regularity;<sup>29</sup> restricting the sample to
participants with at least seven valid days gave a similar association
between physical activity volume and sleep regularity ($\beta$ = 0.31
\[0.30, 0.33\] at age 11 and 0.24 \[0.22, 0.27\] at age 65). As in any
observational study, the temporal ordering of our lagged models does not
establish causality, and although we controlled for several potential
confounders, unmeasured factors may explain the observed associations.
Finally, adolescents and young adults were under-represented, which
limits generalisability to these groups and matters because young adults
are the most likely to report difficulty falling asleep.<sup>4</sup>

These findings have implications for public health recommendations. Our
results support promoting physical activity as a strategy for
maintaining regular sleep patterns across the lifespan. This message
fits with Australia’s recent 24-hour movement guidelines for adults,
which pair physical activity recommendations with consistent bed and
wake times.<sup>30</sup> Given the well-established links between sleep
and overall health,<sup>1</sup> incorporating physical activity into
public health messaging may have downstream benefits for population
health. However, physical activity was more weakly and less consistently
associated with sleep duration, efficiency, and onset, so public health
messages should emphasise its role in promoting consistent sleep
schedules rather than framing it as a universal solution for improving
sleep. We did not examine these relationships specifically in
individuals with sleep disorders or other health conditions, where the
effects of physical activity may differ. Future research should test
whether physical activity interventions improve sleep regularity,
whether more regular sleep in turn supports physical activity, and
whether either brings health benefits beyond sleep itself.

<div class="no-count">

# Contributors

CL, TH, TS, BdPC, PP, and MN conceived and designed the study. TH
managed the data acquisition. TS managed the data harmonisation and
developed the accelerometer processing pipeline. JC, TS, and MN
conducted the statistical analyses. TH, TS, JC, and MN had direct access
to and verified the underlying data. TH, TS, BdPC, MN, PP, JC, and CL
drafted the manuscript. All authors contributed to the interpretation of
results and critically revised the manuscript for important intellectual
content. All authors approved the final version of the manuscript and
agree to be accountable for all aspects of the work.

# Declaration of interests

We declare no competing interests

# Data sharing

Individual participant data remains the property of the original study
investigators, and we cannot share it without their permission.
Reasonable requests for access to the pooled dataset will be considered
by the corresponding author, and we will seek permission from the
original investigators. Related documents, including the study protocol
and analysis plan, are publicly available (https://osf.io/ahd96,
https://github.com/Motivation-and-Behaviour/sleepIPD_analysis).

# Acknowledgments

The authors acknowledge the funders of the original studies, and the
participants who contributed their data.

# AI declaration

We used AI tools, primarily Claude Opus 5.5, to assist with code review
and editing of the analysis code. The authors had full oversight of the
analysis and code, and all decisions were made by the authors.

</div>



# References

<div id="refs" class="references csl-bib-body">

<div id="ref-huangSleepPhysicalActivity2022" class="csl-entry">

<span class="csl-left-margin">1
</span><span class="csl-right-inline">Huang B-H, Duncan MJ, Cistulli PA,
Nassar N, Hamer M, Stamatakis E. [Sleep and physical activity in
relation to all-cause, cardiovascular disease and cancer mortality
risk](https://doi.org/10.1136/bjsports-2021-104046). *Br J Sports Med*
2022; **56**: 718–24.</span>

</div>

<div id="ref-liRegularSleepPatterns2025" class="csl-entry">

<span class="csl-left-margin">2 </span><span class="csl-right-inline">Li
D-R, Li Z-X, Li M-H, *et al.* [Regular sleep patterns, not just
duration, critical for mental health: Association of
accelerometer-derived sleep regularity with incident depression and
anxiety](https://doi.org/10.1017/S0033291725101281). *Psychol Med* 2025;
**55**: e239.</span>

</div>

<div id="ref-gariepyHowAreAdolescents2020" class="csl-entry">

<span class="csl-left-margin">3
</span><span class="csl-right-inline">Gariepy G, Danna S, Gobiņa I, *et
al.* [How Are Adolescents Sleeping? Adolescent Sleep Patterns and
Sociodemographic Differences in 24 European and North American
Countries](https://doi.org/10.1016/j.jadohealth.2020.03.013). *J Adolesc
Health* 2020; **66**: S81–8.</span>

</div>

<div id="ref-ngShortSleepDuration2026" class="csl-entry">

<span class="csl-left-margin">4 </span><span class="csl-right-inline">Ng
AE, Black LI, Adjaye-Gbewonyo D. Short Sleep Duration and Sleep
Difficulties Among Adults: United States, 2024. National Center for
Health Statistics (U.S.), 2026
DOI:[10.15620/cdc/252438](https://doi.org/10.15620/cdc/252438).</span>

</div>

<div id="ref-limNeedPromoteSleep2023" class="csl-entry">

<span class="csl-left-margin">5
</span><span class="csl-right-inline">Lim DC, Najafi A, Afifi L, *et
al.* [The need to promote sleep health in public health agendas across
the globe](https://doi.org/10.1016/S2468-2667(23)00182-2). *Lancet
Public Health* 2023; **8**: e820–6.</span>

</div>

<div id="ref-klinePhysicalActivitySleep2021" class="csl-entry">

<span class="csl-left-margin">6
</span><span class="csl-right-inline">Kline CE, Hillman CH, Bloodgood
Sheppard B, *et al.* [Physical activity and sleep: An updated umbrella
review of the 2018 Physical Activity Guidelines Advisory Committee
report](https://doi.org/10.1016/j.smrv.2021.101489). *Sleep Med Rev*
2021; **58**: 101489.</span>

</div>

<div id="ref-kredlowEffectsPhysicalActivity2015" class="csl-entry">

<span class="csl-left-margin">7
</span><span class="csl-right-inline">Kredlow MA, Capozzoli MC, Hearon
BA, Calkins AW, Otto MW. [The effects of physical activity on sleep: A
meta-analytic review](https://doi.org/10.1007/s10865-015-9617-6). *J
Behav Med* 2015; **38**: 427–49.</span>

</div>

<div id="ref-liangDeterminantsInterrelationsExercise2026"
class="csl-entry">

<span class="csl-left-margin">8
</span><span class="csl-right-inline">Liang Z, Zhao S, Tian S, *et al.*
[Determinants and interrelations of exercise effects on sleep quality: A
multimethod meta-analysis](https://doi.org/10.1016/j.smrv.2026.102239).
*Sleep Med Rev* 2026; **86**: 102239.</span>

</div>

<div id="ref-bullWorldHealthOrganization2020" class="csl-entry">

<span class="csl-left-margin">9
</span><span class="csl-right-inline">Bull FC, Al-Ansari SS, Biddle S,
*et al.* [World Health Organization 2020 guidelines on physical activity
and sedentary behaviour](https://doi.org/10.1136/bjsports-2020-102955).
*Br J Sports Med* 2020; **54**: 1451–62.</span>

</div>

<div id="ref-antczakPhysicalActivitySleep2020" class="csl-entry">

<span class="csl-left-margin">10
</span><span class="csl-right-inline"><span class="nocase">Antczak D,
Lonsdale C, Lee J, *et al.*</span> [Physical activity and sleep are
inconsistently related in healthy children: A systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2020.101278). *Sleep Med
Rev* 2020; **51**: 101278.</span>

</div>

<div id="ref-langRelationshipPhysicalActivity2016" class="csl-entry">

<span class="csl-left-margin">11
</span><span class="csl-right-inline">Lang C, Kalak N, Brand S,
Holsboer-Trachsler E, Pühse U, Gerber M. [The relationship between
physical activity and sleep from mid adolescence to early adulthood. A
systematic review of methodological approaches and
meta-analysis](https://doi.org/10.1016/j.smrv.2015.07.004). *Sleep Med
Rev* 2016; **28**: 32–45.</span>

</div>

<div id="ref-songEffectsPhysicalActivity2026" class="csl-entry">

<span class="csl-left-margin">12
</span><span class="csl-right-inline">Song H, Jiang X, Li Y, *et al.*
[Effects of physical activity on sleep in children and adolescents: A
systematic review and meta-analysis of randomised controlled
trials](https://doi.org/10.1007/s00787-025-02892-6). *Eur Child Adolesc
Psychiatry* 2026; **35**: 397–411.</span>

</div>

<div id="ref-rileyUsingIndividualParticipant2023" class="csl-entry">

<span class="csl-left-margin">13
</span><span class="csl-right-inline">Riley RD, Dias S, Donegan S, *et
al.* [Using individual participant data to improve network meta-analysis
projects](https://doi.org/10.1136/bmjebm-2022-111931). *BMJ Evid-Based
Med* 2023; **28**: 197–203.</span>

</div>

<div id="ref-atouiDailyAssociationsSleep2021" class="csl-entry">

<span class="csl-left-margin">14
</span><span class="csl-right-inline">Atoui S, Chevance G, Romain A-J,
Kingsbury C, Lachance J-P, Bernard P. [Daily associations between sleep
and physical activity: A systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2021.101426). *Sleep Med
Rev* 2021; **57**: 101426.</span>

</div>

<div id="ref-huangRelationshipsPhysicalActivity2021" class="csl-entry">

<span class="csl-left-margin">15
</span><span class="csl-right-inline">Huang WY, Ho RS-T, Tremblay MS,
Wong SH-S. [Relationships of physical activity and sedentary behaviour
with the previous and subsequent nights’ sleep in children and youth: A
systematic review and meta-analysis](https://doi.org/10.1111/jsr.13378).
*J Sleep Res* 2021; **30**: e13378.</span>

</div>

<div id="ref-fittonBidirectionalAssociationsSleep2025"
class="csl-entry">

<span class="csl-left-margin">16
</span><span class="csl-right-inline">Fitton J, Nguyen DP, Lechat B, *et
al.* [Bidirectional associations between sleep and physical activity
investigated using large-scale objective monitoring
data](https://doi.org/10.1038/s43856-025-01226-6). *Commun Med* 2025;
**5**: 519.</span>

</div>

<div id="ref-leotaSleepDurationTiming2025" class="csl-entry">

<span class="csl-left-margin">17
</span><span class="csl-right-inline">Leota J, Czeisler MÉ, Le F, *et
al.* [Sleep duration and timing are associated with next-day physical
activity: Insights from two large-scale wearable sensor
studies](https://doi.org/10.1073/pnas.2420846122). *Proc Natl Acad Sci*
2025; **122**: e2420846122.</span>

</div>

<div id="ref-korkutataImpactExerciseSleep2025" class="csl-entry">

<span class="csl-left-margin">18
</span><span class="csl-right-inline">Korkutata A, Korkutata M, Lazarus
M. [The impact of exercise on sleep and sleep
disorders](https://doi.org/10.1038/s44323-024-00018-w). *Npj Biol Timing
Sleep* 2025; **2**: 5.</span>

</div>

<div id="ref-miguelesGGIRResearchCommunity2019" class="csl-entry">

<span class="csl-left-margin">19
</span><span class="csl-right-inline">Migueles JH, Rowlands AV, Huber F,
Sabia S, Van Hees VT. [GGIR: A Research Community–Driven Open Source R
Package for Generating Physical Activity and Sleep Outcomes From
Multi-Day Raw Accelerometer
Data](https://doi.org/10.1123/jmpb.2018-0063). *J Meas Phys Behav* 2019;
**2**: 188–96.</span>

</div>

<div id="ref-vanheesEstimatingSleepParameters2018" class="csl-entry">

<span class="csl-left-margin">20
</span><span class="csl-right-inline">Van Hees VT, Sabia S, Jones SE,
*et al.* Estimating sleep parameters using an accelerometer without
sleep diary. *Sci Rep* 2018; **8**.
DOI:[10.1038/s41598-018-31266-z](https://doi.org/10.1038/s41598-018-31266-z).</span>

</div>

<div id="ref-sandersSleepIPDOpinionatedAccelerometer2023"
class="csl-entry">

<span class="csl-left-margin">21
</span><span class="csl-right-inline">Sanders T.
<span class="nocase">sleepIPD</span>: Opinionated accelerometer
reprocessing for the SleepIPD study. 2023.
<https://motivation-and-behaviour.github.io/sleepIPD>.</span>

</div>

<div id="ref-chenPerformanceAutomatedSleep2026" class="csl-entry">

<span class="csl-left-margin">22
</span><span class="csl-right-inline">Chen P-W, Jansen EC, Cielo CM, *et
al.* [Performance of an automated sleep scoring approach for actigraphy
data in children and
adolescents](https://doi.org/10.1093/sleep/zsaf282). *Sleep* 2026;
**49**: zsaf282.</span>

</div>

<div id="ref-rowlandsMovingForwardAccelerometerAssessed2018"
class="csl-entry">

<span class="csl-left-margin">23
</span><span class="csl-right-inline">Rowlands AV. [Moving Forward With
Accelerometer-Assessed Physical Activity: Two Strategies to Ensure
Meaningful, Interpretable, and Comparable
Measures](https://doi.org/10.1123/pes.2018-0201). *Pediatr Exerc Sci*
2018; **30**: 450–6.</span>

</div>

<div id="ref-leotaDoseresponseRelationshipEvening2025"
class="csl-entry">

<span class="csl-left-margin">24
</span><span class="csl-right-inline">Leota J, Presby DM, Le F, *et al.*
[Dose-response relationship between evening exercise and
sleep](https://doi.org/10.1038/s41467-025-58271-x). *Nat Commun* 2025;
**16**: 3297.</span>

</div>

<div id="ref-phamBidirectionalAssociationPhysical2025"
class="csl-entry">

<span class="csl-left-margin">25
</span><span class="csl-right-inline">Pham H, Svensson T, Chung U,
Svensson AK. [Bidirectional association between physical activity and
sleep in healthy Japanese super-seniors: The Japan Healthy Aging Study
(J-HAS)](https://doi.org/10.1038/s41514-025-00223-2). *npj Aging* 2025;
**11**: 36.</span>

</div>

<div id="ref-windredSleepRegularityStronger2024" class="csl-entry">

<span class="csl-left-margin">26
</span><span class="csl-right-inline">Windred DP, Burns AC, Lane JM, *et
al.* [Sleep regularity is a stronger predictor of mortality risk than
sleep duration: A prospective cohort
study](https://doi.org/10.1093/sleep/zsad253). *SLEEP* 2024; **47**:
zsad253.</span>

</div>

<div id="ref-chaputSleepRegularityMajor2025" class="csl-entry">

<span class="csl-left-margin">27
</span><span class="csl-right-inline">Chaput J-P, Biswas RK, Ahmadi M,
*et al.* [Sleep regularity and major adverse cardiovascular events: A
device-based prospective study in 72 269 UK
adults](https://doi.org/10.1136/jech-2024-222795). *J Epidemiol
Community Health* 2025; **79**: 257–64.</span>

</div>

<div id="ref-zisouHarmonisedAccelerometerderivedPhysical2026"
class="csl-entry">

<span class="csl-left-margin">28
</span><span class="csl-right-inline">Zisou C, Maylor BD, Acquah A, *et
al.* Towards harmonised accelerometer-derived physical activity: A
comparability study across wrist, thigh, and hip wear locations. *Int J
Behav Nutr Phys Act* 2026; published online Sept.
DOI:[10.1186/s12966-026-01975-0](https://doi.org/10.1186/s12966-026-01975-0).</span>

</div>

<div id="ref-lokHowManyDays2024" class="csl-entry">

<span class="csl-left-margin">29
</span><span class="csl-right-inline">Lok R, Suh S, Rue S, Weed L,
Zeitzer JM. [How many days are enough? Sleep–wake timing regularity and
fragmentation scores change with the number of days
included](https://doi.org/10.1111/jsr.14332). *J Sleep Res* 2024; :
e14332.</span>

</div>

<div id="ref-australiangovernmentdepartmentofhealth24hourMovementGuidelines2026"
class="csl-entry">

<span class="csl-left-margin">30
</span><span class="csl-right-inline">Australian Government Department
of Health, Disability and Ageing. 24-hour movement guidelines for all
Australians. 2026; published online April.
<https://www.health.gov.au/topics/physical-activity/24-hour-movement-guidelines-for-all-australians>
(accessed Oct 10, 2026).</span>

</div>

</div>
