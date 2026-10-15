# Introduction

Sleep is an important determinant of population health across the
lifespan \[1,2\]. The consequences of poor sleep include increased
all-cause mortality \[3\], cardiovascular disease \[4\], cancers \[5\],
and poor mental health \[6\]. Beyond duration and quality, the
regularity of sleep timing is now recognised as a distinct dimension of
sleep health \[7,8\], and irregular sleep is consistently associated
with poorer mental, cardiometabolic, and survival outcomes \[9\]. Poor
sleep - characterised by low sleep quality, duration, or the presence of
sleep disorders - is a frequent health problem: 14-68% of children and
adolescents \[10\] and 31% of adults \[11\] report lower sleep duration
than recommended \[12,13\]. Improving sleep should be a priority for
public health policy \[14\], and more safe and accessible strategies
that promote sleep are needed \[15\]. In adults, including older adults,
being physically active appears to improve both the quantity and quality
of sleep \[16–18\], and exercise is an effective treatment for insomnia
\[19\]. The World Health Organization recommends physical activity as a
method to improve sleep in adults and older adults \[20\]. In children
and adolescents, associations are less clear \[15,21,22\]. Recent trials
suggest exercise improves sleep quality and efficiency but not total
sleep time \[23\], and guidelines do not currently support a role for
physical activity to improve sleep in this age group.

Promoting physical activity may be an effective method to improve sleep
across the lifespan, but the published literature has at least two
limitations. First, most studies have relied on self-report measures of
sleep and physical activity \[15,21,22\], which could confound
associations and limit the precision of analyses designed to identify a
dose-response relationship between physical activity and sleep. Second,
no study has collected day-to-day data from a large and geographically
diverse sample that included participants across the lifespan. Daily
associations have been synthesised separately in adults \[24\] and in
children and youth \[25\], and recent consumer-wearable studies with
millions of person-nights were restricted to adults and did not examine
sleep regularity \[26,27\]. It is necessary to focus on day-level data
as the relationship may run in both directions. Physical activity may
promote sleep through circadian and stress-related pathways \[28\],
while a late or poor night’s sleep may reduce activity the next day
\[24,29\]. Averaging behaviour over a week can mask these day-to-day
patterns. Umbrella reviews \[15\] and meta-analyses
\[4,5,16,21,22,30–45\] have provided some suggestive evidence about how
physical activity may improve sleep across the lifespan. However, their
reliance on aggregated study-level effects limits the quality and scope
of data that can be used to analyse the influence of age \[46\].
Device-measured individual participant data analysis can overcome these
limitations \[47\].

In this study we harmonised individual participant data from
accelerometer observations obtained from studies around the world. The
aim of this pooled study was to investigate the relationship between
device-measured physical activity and sleep across the lifespan. We
addressed three research questions (RQs): (RQ1) Do individuals who are
more physically active have better sleep duration and sleep quality;
(RQ2) is age a moderator of the association between physical activity
and sleep and does this vary across the lifespan; and (RQ3) is there a
bidirectional association between physical activity and sleep?

# Methods

We pre-registered all methods and analyses on the Open Science Framework
(OSF) before data acquisition began (<https://osf.io/ahd96>).

## Data sources and data acquisition

We included data from studies, with any design, that used research-grade
accelerometers and a 24-hour wear time protocol over a minimum of 4
days, including 1 weekend day. Studies needed to have used
accelerometers from which raw accelerations (*m*/*s*<sup>2</sup> or
*m**g*) could be obtained. Studies could use any wear location for the
accelerometer (hip, wrist, or thigh) or accelerometer brand.

We identified potentially relevant studies by examining the reference
lists of recent systematic reviews that explored the relationship
between sleep and physical activity in children, adolescents, and adults
\[16,21,22\]. We then used forward searching on potentially eligible
studies \[48\]. We additionally performed a literature search to
identify accelerometry consortiums and individual studies, searched
trial registries (the Australian New Zealand Clinical Trials Registry,
the World Health Organization International Clinical Trials Registry
Platform, and the ISRCTN registry) and the Open Science Framework for
unpublished results, and asked collaborators to help identify any
additional studies.

We contacted chief investigators or consortium leads by email to request
access to deidentified individual data. To make the data acquisition
manageable, we only invited studies with sample sizes larger than 400
participants. If required by the contributing research group, the
contributing investigators or their university signed a data transfer
agreement. All studies included in the pooled data meta-analysis had to
have been approved by their local or institutional ethical committees,
and all participants (or, for children, their parents or guardians) had
to have provided written informed consent, with children providing
assent where required. The lead university’s Research Ethics Committee
granted permission for each study’s data to be obtained, and
investigators transferred their deidentified data via a secure
file-sharing system.

## Data harmonisation and outcomes

All accelerometer-derived data were harmonised by reprocessing raw files
with the open-source *GGIR* package (version 2.5-1) in R to produce
physical activity and sleep outcomes \[49,50\], using the *sleepIPD* R
package, which fixed all processing settings across studies \[51\].
*GGIR*’s sleep detection has been evaluated against polysomnography in
both adults \[50\] and children \[52,53\]. By reprocessing all raw
accelerometer data, we were able to generate outcomes that could be
compared across studies, device wear locations, and populations \[54\].
Data were reprocessed by the core research team or the data contributors
themselves using the *GGIR* package processing script provided by the
core research team. Reprocessed individual study datasets were then
pooled into a single dataset. A detailed description of the reprocessing
method and decisions for specific accelerometer devices is available
from the *sleepIPD* package documentation \[51\]. Non-wear time and
abnormally high acceleration values were detected using the data
processing default setting \[55\]. These data were classified as missing
and imputed using the accelerometer data from the same time interval on
the remaining days of the week \[49\].

Each day ran from waking to waking the following day, so each day’s
physical activity was paired with the sleep period that followed it.
Physical activity metrics were calculated over the waking hours of each
day, and sleep metrics over the sleep period. For wrist-worn devices,
the sleep period was detected with the HDCZA algorithm \[50\] and for
hip-worn devices *GGIR* instead detected time in bed from the device’s
angle. For sleep, *GGIR* produced the following metrics: total sleep
time (minutes), sleep efficiency (percentage of the sleep period, from
sleep onset to final waking, spent asleep), sleep onset (clock time),
and sleep regularity (the sleep regularity index: the likelihood of
being in the same sleep or wake state at time points 24 hours apart,
calculated for each pair of consecutive days, with higher values
indicating more regular sleep). For physical activity, we used volume
and intensity metrics proposed by Rowlands et al \[56\]. These measures
are 1) average acceleration (mg; a measure of physical activity volume)
and 2) intensity gradient (unitless; a measure of intensity distribution
where the natural logs of time and intensity are used to produce a
straight-line graph with the slope of the graph being the intensity
gradient, so that less negative values indicate relatively more time at
higher intensities). We used this approach to model the entire continuum
of physical activity intensity for our primary analyses \[57\] and avoid
collapsing physical activity data into population-specific intensity
categories, which occurs when intensity cut-points are applied to
accelerometer data \[58\].

*GGIR* also produced time in intensity bands, cut-point-based estimates
of light and moderate-to-vigorous activity, bout durations \[59\], and
*M**X* metrics \[57\], which are not analysed here.

## Covariates and moderators

The following individual participant data were obtained when available:
age, sex, height, weight, waist circumference, maturational status,
socioeconomic status (SES), ethnicity, screen time, sleep medications,
and medically diagnosed conditions known to affect sleep. The country,
city, time zone, daylight length, and season in which data were
collected were also obtained. Because studies measured socioeconomic
status differently (e.g., education or income), we harmonised each
study’s measure into low, medium, and high categories (see supplementary
materials for method).

Body mass index was converted to an age- and sex-standardised z-score.
For participants under 18 years we used the CDC 2000 BMI-for-age growth
reference, applied with the *childsds* package. For adults we
standardised body mass index against the adults in this sample.

## Statistical analysis

To reduce the risk of bias in secondary data analysis of IPD, we used a
blind analysis approach \[60\]. We developed and tested models on a 10%
holdout subsample of the dataset. When the analysis code was ready, we
conducted the final analysis \[61\].

We screened all data for implausibly low or high measurements, as these
likely indicate measurement device failures. Screening had four stages.
First, we excluded values falling outside fixed physiological limits.
For average acceleration this was 200 mg, a value above any sustainable
24-hour average. Second, because a miscalibrated accelerometer corrupts
every variable derived from acceleration magnitude rather than isolated
days, we excluded all physical activity variables for files whose mean
average acceleration exceeded that limit (76 participants). Third, we
excluded remaining values falling outside ±4 SD of the mean **of their
own study**, rather than of the pooled sample, so that the threshold
reflected implausible measurement rather than between-study differences
in device, protocol and population. Fourth, for participants under 18
years, we excluded heights, weights, and body mass index values that the
CDC classifies as biologically implausible for the child’s age and sex
against its 2000 growth reference (height-for-age z &lt; -5 or &gt; 4,
weight-for-age z &lt; -5 or &gt; 8, and BMI-for-age z &lt; -4 or &gt;
8). Values excluded at any stage were treated as missing data.

A day contributed to the analysis if it recorded more than 10 hours of
wear time and, where sleep was detected, a sleep period longer than 200
minutes. Participants were additionally required to have at least 4 days
of valid wear time. 413 participants, contributing 619 person-days, did
not meet this criterion, leaving 27,236 participants and 167,994
person-days for analysis. A sensitivity analysis restricted the sample
to the 14,981 participants with at least seven days of valid wear time.

We assumed missing covariate and moderator data to be missing at random
or completely at random and imputed these missing values using multiple
imputations by chained equations with the *mice* package in R \[62\] to
provide 50 imputed datasets. Specifically, we used multilevel predictive
mean matching imputation clustered by participants. The imputation model
also included the sleep and physical activity variables, but each
analysis used only the person-days on which its outcome, its sleep or
physical activity predictor, and its moderator were observed, so imputed
values are only covariates. We used Rubin’s rule to pool the results
from analyses conducted using each imputed dataset into one single set
of results. We examined 95% confidence intervals for all analyses and
used P&lt;0.05 to assess for statistical significance. Both the
intervals and the p-values are computed on the pooled Barnard-Rubin
degrees of freedom, which account for the additional uncertainty
introduced by imputation. We fitted 16 models for each moderator and
report all of them without adjustment for multiple comparisons, so
p-values close to 0.05 should be interpreted with caution. We used R
(version 4.6.1) for all data cleaning and analysis. We report mean and
95% confidence intervals. For categorical variables, percentages are
reported. We examined model diagnostics from all models (see
supplementary materials).

To examine the relationship between sleep and physical activity we ran
mixed-effects models with random intercepts for study ID and for
participant, accounting for the nesting of repeated days within
participants and of participants within studies \[63\]. We predicted
each sleep indicator by each physical activity indicator controlling for
socioeconomic status, sex, and body mass index z-score. Quadratic terms
were introduced in each model to allow for curvilinear relationships to
be modelled. We interacted each predictor with age to reveal how the
relationship between physical activity and sleep changes over the
lifespan (RQ2). Age was centred at its mean across person-days (35.6
years) and scaled per 10 years, and we report each association at ages
11, 18, 35, and 65, pooled across imputations with Rubin’s rules. Models
predicting physical activity from the preceding night’s sleep (RQ3) use
each sleep indicator lagged by one day. Person-days whose preceding day
was absent from the data or did not meet the eligibility criteria
therefore have no lagged value and do not contribute to these models,
leaving 136,319 of 168,613 eligible person-days. Sleep regularity was
missing most often, and was not measured in 1 of the 20 studies, so the
models relating it to physical activity volume used 125,155 person-days
(RQ1) and 109,033 (RQ3).

## Protocol deviations

This paper addresses Hypotheses 1 and 2 from our registered protocol: 1.
Volume and intensity of physical activity will be positively associated
with optimum sleep duration and sleep quality with an inverted U
dose-response. 2. Age will moderate the association between physical
activity and sleep. Associations will be stronger in adults and
adolescents than in children and older adults. Moderation by the other
pre-registered moderators is reported in the supplementary materials,
and we will address the remaining hypotheses in future research. To
streamline our analysis and reduce redundancy, we made the following
changes to our protocol while working with the holdout data.

### Using Study ID as fixed effect

We initially planned to use Study ID as a fixed effect, given that our
study selection was not random. But, as we aimed for our findings to
generalise to other studies and populations, we instead opted to include
random intercepts for Study ID which can better model generalisable
patterns. This approach allows us to account for variability between
studies without fully partialling out study effects, thereby better
modelling common phenomena. While this change lessened the strength of
model intercepts, it did not significantly alter model slopes. The
results using Study ID as a fixed effect (i.e., those that align with
the protocol) can be found in the supplementary materials and are
qualitatively similar to the main results.

### Sleep indicator variable

Our research examines various metrics of sleep: onset, duration,
regularity, and efficiency. We initially planned to merge these
variables into a single indicator to simplify analysis. However, no
validated indicator existed which could be estimated from our measured
variables. We decided against creating one as we could not validate it
with our data. Consequently, our models illustrate the relationship
between each aspect of sleep and physical activity, instead of
describing a straightforward link between overall sleep and physical
activity.

### Log transformation

We aimed to improve model diagnostics using log transformations of
skewed variables. Pilot analysis using holdout data revealed that while
diagnostics improved on the log scale, model predictions followed
extreme exponential patterns near the edge of the data range when
back-transformed. Accordingly, we do not use log transformations in the
main paper. However, these models can be found in supplementary
materials.

## Patient and public involvement

Patients and members of the public were not involved in the design,
analysis or interpretation of this IPD study using secondary data. The
results of this study have broad public health interest, particularly
for policymakers. Findings from this study will be disseminated through
institutional websites, press and media platform releases, and tailored
messages to public health Organizations and governing bodies.

# Results

The aggregated dataset describes 220,988 observations of daily physical
activity and sleep from 28,824 unique participants. Of these, 52,375
days were excluded as they had insufficient wear time, which also
resulted in the exclusion of 1,175 participants. A further 413
participants (619 days) were excluded for having fewer than 4 days of
valid wear time, leaving 27,236 participants and 167,994 person-days.
Table @ref(tab:demo-participants) shows demographic information for the
included participants. Most participants came from Europe and South
America, followed by Oceania, with limited data from other regions.
Participants were aged 2 to 94 years. There was a near-even split by sex
(50.3% female), but age skewed towards both younger children (2-11
years, 50.9%) and older adults (66+ years, 22.3%).

Valid observations were not uniform across the days of the week
(*χ*<sub>(6)</sub><sup>2</sup> = 343.32, p &lt; .001). Weekends and
Fridays were over-represented (Saturday: z = 11.24; Sunday: z = 7.84;
Friday: z = 5.57), while Wednesdays (z = -9.30) and Tuesdays (z = -8.22)
were under-represented. A table of study characteristics can be found in
the supplementary materials.

<table>
<caption>(#tab:demo-participants) Participant characteristics</caption>
<thead>
<tr>
<th style="text-align: left;">Characteristic</th>
<th style="text-align: left;">2-11 years</th>
<th style="text-align: left;">12-18 years</th>
<th style="text-align: left;">19-35 years</th>
<th style="text-align: left;">36-65 years</th>
<th style="text-align: left;">66+ years</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Numeric variables</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   N</td>
<td style="text-align: left;">13,820</td>
<td style="text-align: left;">1,923</td>
<td style="text-align: left;">310</td>
<td style="text-align: left;">5,044</td>
<td style="text-align: left;">6,067</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">8.5 (2.8)</td>
<td style="text-align: left;">15.7 (1.7)</td>
<td style="text-align: left;">29.2 (5.1)</td>
<td style="text-align: left;">57.7 (7.4)</td>
<td style="text-align: left;">72.7 (4.5)</td>
</tr>
<tr>
<td style="text-align: left;">   BMI</td>
<td style="text-align: left;">17.7 (3.0)</td>
<td style="text-align: left;">21.6 (3.8)</td>
<td style="text-align: left;">26.5 (6.5)</td>
<td style="text-align: left;">26.8 (4.8)</td>
<td style="text-align: left;">27.1 (4.3)</td>
</tr>
<tr>
<td style="text-align: left;">   BMI z-score</td>
<td style="text-align: left;">0.31 (1.10)</td>
<td style="text-align: left;">0.09 (1.02)</td>
<td style="text-align: left;">-0.07 (1.40)</td>
<td style="text-align: left;">-0.01 (1.03)</td>
<td style="text-align: left;">0.05 (0.93)</td>
</tr>
<tr>
<td style="text-align: left;">   Valid Weartime Hours</td>
<td style="text-align: left;">23.4 (1.3)</td>
<td style="text-align: left;">23.4 (1.5)</td>
<td style="text-align: left;">23.3 (1.7)</td>
<td style="text-align: left;">23.8 (0.7)</td>
<td style="text-align: left;">23.9 (0.4)</td>
</tr>
<tr>
<td style="text-align: left;">   Valid Weartime Days</td>
<td style="text-align: left;">5.7 (1.8)</td>
<td style="text-align: left;">5.0 (1.3)</td>
<td style="text-align: left;">5.0 (1.4)</td>
<td style="text-align: left;">7.5 (2.3)</td>
<td style="text-align: left;">6.6 (2.1)</td>
</tr>
<tr>
<td style="text-align: left;">   PA Intensity Gradient (unitless)</td>
<td style="text-align: left;">-2.10 (0.18)</td>
<td style="text-align: left;">-2.23 (0.19)</td>
<td style="text-align: left;">-2.43 (0.19)</td>
<td style="text-align: left;">-2.50 (0.19)</td>
<td style="text-align: left;">-2.68 (0.22)</td>
</tr>
<tr>
<td style="text-align: left;">   PA Volume (average acceleration in
mg)</td>
<td style="text-align: left;">49.0 (26.4)</td>
<td style="text-align: left;">46.7 (14.3)</td>
<td style="text-align: left;">39.3 (10.6)</td>
<td style="text-align: left;">38.8 (11.6)</td>
<td style="text-align: left;">30.9 (9.7)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Duration (min)</td>
<td style="text-align: left;">471.7 (61.5)</td>
<td style="text-align: left;">416.4 (55.0)</td>
<td style="text-align: left;">421.0 (64.0)</td>
<td style="text-align: left;">409.1 (55.8)</td>
<td style="text-align: left;">404.3 (63.9)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Efficiency (%)</td>
<td style="text-align: left;">80.9 (7.0)</td>
<td style="text-align: left;">87.1 (6.3)</td>
<td style="text-align: left;">87.4 (6.2)</td>
<td style="text-align: left;">87.8 (5.5)</td>
<td style="text-align: left;">86.4 (6.2)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Onset (clock time; SD in
h:mm)</td>
<td style="text-align: left;">21:32 (01:42)</td>
<td style="text-align: left;">23:29 (01:27)</td>
<td style="text-align: left;">23:33 (01:49)</td>
<td style="text-align: left;">23:25 (01:17)</td>
<td style="text-align: left;">23:57 (01:16)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Regularity Index</td>
<td style="text-align: left;">53.4 (12.2)</td>
<td style="text-align: left;">57.0 (12.1)</td>
<td style="text-align: left;">55.8 (12.8)</td>
<td style="text-align: left;">59.4 (11.4)</td>
<td style="text-align: left;">54.5 (12.5)</td>
</tr>
<tr>
<td style="text-align: left;">Accelerometer Wear Location</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Wrist</td>
<td style="text-align: left;">5,509 (39.9%)</td>
<td style="text-align: left;">1,885 (98.0%)</td>
<td style="text-align: left;">310 (100.0%)</td>
<td style="text-align: left;">5,044 (100.0%)</td>
<td style="text-align: left;">6,067 (100.0%)</td>
</tr>
<tr>
<td style="text-align: left;">   Hip</td>
<td style="text-align: left;">8,311 (60.1%)</td>
<td style="text-align: left;">   38 (2.0%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">Country</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Australia</td>
<td style="text-align: left;">1,510 (10.9%)</td>
<td style="text-align: left;">305 (15.9%)</td>
<td style="text-align: left;">110 (35.5%)</td>
<td style="text-align: left;">  188 (3.7%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Finland</td>
<td style="text-align: left;">  508 (3.7%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">  801 (15.9%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   New Zealand</td>
<td style="text-align: left;">  455 (3.3%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">37 (11.9%)</td>
<td style="text-align: left;">  152 (3.0%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Spain</td>
<td style="text-align: left;">  809 (5.9%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">   37 (0.7%)</td>
<td style="text-align: left;">2,386 (39.3%)</td>
</tr>
<tr>
<td style="text-align: left;">   Switzerland</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">1,905 (37.8%)</td>
<td style="text-align: left;">1,023 (16.9%)</td>
</tr>
<tr>
<td style="text-align: left;">   United Kingdom</td>
<td style="text-align: left;">  447 (3.2%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">144 (46.5%)</td>
<td style="text-align: left;">1,961 (38.9%)</td>
<td style="text-align: left;">2,658 (43.8%)</td>
</tr>
<tr>
<td style="text-align: left;">   Brazil</td>
<td style="text-align: left;">3,425 (24.8%)</td>
<td style="text-align: left;">727 (37.8%)</td>
<td style="text-align: left;">10 (3.2%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Czech Republic</td>
<td style="text-align: left;">  281 (2.0%)</td>
<td style="text-align: left;">848 (44.1%)</td>
<td style="text-align: left;">  9 (2.9%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Canada</td>
<td style="text-align: left;">  522 (3.8%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Chile</td>
<td style="text-align: left;">  138 (1.0%)</td>
<td style="text-align: left;">  5 (0.3%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   China</td>
<td style="text-align: left;">  125 (0.9%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Colombia</td>
<td style="text-align: left;">  889 (6.4%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Denmark</td>
<td style="text-align: left;">  689 (5.0%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Ecuador</td>
<td style="text-align: left;">  981 (7.1%)</td>
<td style="text-align: left;">38 (2.0%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   India</td>
<td style="text-align: left;">  575 (4.2%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Kenya</td>
<td style="text-align: left;">  528 (3.8%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Portugal</td>
<td style="text-align: left;">  702 (5.1%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   South Africa</td>
<td style="text-align: left;">  507 (3.7%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   United States</td>
<td style="text-align: left;">  729 (5.3%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">Ethnicity</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Non-White</td>
<td style="text-align: left;">2,672 (19.3%)</td>
<td style="text-align: left;">  169 (8.8%)</td>
<td style="text-align: left;">14 (4.5%)</td>
<td style="text-align: left;">   94 (1.9%)</td>
<td style="text-align: left;">  240 (4.0%)</td>
</tr>
<tr>
<td style="text-align: left;">   Other or unknown</td>
<td style="text-align: left;">4,780 (34.6%)</td>
<td style="text-align: left;">1,194 (62.1%)</td>
<td style="text-align: left;">154 (49.7%)</td>
<td style="text-align: left;">1,340 (26.6%)</td>
<td style="text-align: left;">2,424 (40.0%)</td>
</tr>
<tr>
<td style="text-align: left;">   White</td>
<td style="text-align: left;">6,368 (46.1%)</td>
<td style="text-align: left;">  560 (29.1%)</td>
<td style="text-align: left;">142 (45.8%)</td>
<td style="text-align: left;">3,610 (71.6%)</td>
<td style="text-align: left;">3,403 (56.1%)</td>
</tr>
<tr>
<td style="text-align: left;">Region</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Europe</td>
<td style="text-align: left;">3,436 (24.9%)</td>
<td style="text-align: left;">848 (44.1%)</td>
<td style="text-align: left;">153 (49.4%)</td>
<td style="text-align: left;">4,704 (93.3%)</td>
<td style="text-align: left;">6,067 (100.0%)</td>
</tr>
<tr>
<td style="text-align: left;">   Oceania</td>
<td style="text-align: left;">1,965 (14.2%)</td>
<td style="text-align: left;">305 (15.9%)</td>
<td style="text-align: left;">147 (47.4%)</td>
<td style="text-align: left;">  340 (6.7%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   South America</td>
<td style="text-align: left;">5,433 (39.3%)</td>
<td style="text-align: left;">770 (40.0%)</td>
<td style="text-align: left;">10 (3.2%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Africa</td>
<td style="text-align: left;">1,035 (7.5%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Asia</td>
<td style="text-align: left;">  700 (5.1%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   North America</td>
<td style="text-align: left;">1,251 (9.1%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">Season</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Autumn</td>
<td style="text-align: left;">4,672 (33.8%)</td>
<td style="text-align: left;">  445 (23.1%)</td>
<td style="text-align: left;">39 (12.6%)</td>
<td style="text-align: left;">1,241 (24.6%)</td>
<td style="text-align: left;">1,268 (20.9%)</td>
</tr>
<tr>
<td style="text-align: left;">   Spring</td>
<td style="text-align: left;">2,859 (20.7%)</td>
<td style="text-align: left;">1,230 (64.0%)</td>
<td style="text-align: left;">125 (40.3%)</td>
<td style="text-align: left;">1,226 (24.3%)</td>
<td style="text-align: left;">1,988 (32.8%)</td>
</tr>
<tr>
<td style="text-align: left;">   Summer</td>
<td style="text-align: left;">2,113 (15.3%)</td>
<td style="text-align: left;">   82 (4.3%)</td>
<td style="text-align: left;">85 (27.4%)</td>
<td style="text-align: left;">1,271 (25.2%)</td>
<td style="text-align: left;">1,252 (20.6%)</td>
</tr>
<tr>
<td style="text-align: left;">   Winter</td>
<td style="text-align: left;">4,176 (30.2%)</td>
<td style="text-align: left;">  166 (8.6%)</td>
<td style="text-align: left;">61 (19.7%)</td>
<td style="text-align: left;">1,306 (25.9%)</td>
<td style="text-align: left;">1,559 (25.7%)</td>
</tr>
<tr>
<td style="text-align: left;">Sex</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Female</td>
<td style="text-align: left;">7,154 (51.8%)</td>
<td style="text-align: left;">1,002 (52.1%)</td>
<td style="text-align: left;">222 (71.6%)</td>
<td style="text-align: left;">2,717 (53.9%)</td>
<td style="text-align: left;">2,551 (42.0%)</td>
</tr>
<tr>
<td style="text-align: left;">   Male</td>
<td style="text-align: left;">6,664 (48.2%)</td>
<td style="text-align: left;">  921 (47.9%)</td>
<td style="text-align: left;">88 (28.4%)</td>
<td style="text-align: left;">2,327 (46.1%)</td>
<td style="text-align: left;">3,516 (58.0%)</td>
</tr>
<tr>
<td style="text-align: left;">Sleep Conditions Reported</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   No</td>
<td style="text-align: left;">   625 (4.5%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">3,512 (69.6%)</td>
<td style="text-align: left;">5,141 (84.7%)</td>
</tr>
<tr>
<td style="text-align: left;">   Yes</td>
<td style="text-align: left;">    50 (0.4%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">  310 (6.1%)</td>
<td style="text-align: left;">  626 (10.3%)</td>
</tr>
<tr>
<td style="text-align: left;">   Not Reported</td>
<td style="text-align: left;">13,145 (95.1%)</td>
<td style="text-align: left;">1,923 (100.0%)</td>
<td style="text-align: left;">310 (100.0%)</td>
<td style="text-align: left;">1,222 (24.2%)</td>
<td style="text-align: left;">  300 (4.9%)</td>
</tr>
<tr>
<td style="text-align: left;">Socioeconomic Status</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   Low</td>
<td style="text-align: left;">2,892 (24.1%)</td>
<td style="text-align: left;">208 (11.5%)</td>
<td style="text-align: left;">  2 (0.7%)</td>
<td style="text-align: left;">1,074 (22.0%)</td>
<td style="text-align: left;">2,388 (39.4%)</td>
</tr>
<tr>
<td style="text-align: left;">   Medium</td>
<td style="text-align: left;">4,638 (38.7%)</td>
<td style="text-align: left;">885 (49.1%)</td>
<td style="text-align: left;">191 (70.2%)</td>
<td style="text-align: left;">2,409 (49.3%)</td>
<td style="text-align: left;">2,325 (38.3%)</td>
</tr>
<tr>
<td style="text-align: left;">   High</td>
<td style="text-align: left;">4,456 (37.2%)</td>
<td style="text-align: left;">708 (39.3%)</td>
<td style="text-align: left;">79 (29.0%)</td>
<td style="text-align: left;">1,406 (28.8%)</td>
<td style="text-align: left;">1,352 (22.3%)</td>
</tr>
</tbody>
</table>

*Note.* For categorical variables the value is the count, and
percentage. For numeric variables the value is the Mean and SD. Sleep
onset is a clock time; its SD is a duration in hours:minutes. Total N =
27,236. 72 participants had missing age data and are counted in the
total but not in any age band.

 

## The association between physical activity volume and sleep

We estimated the association between physical activity and sleep (RQ1)
using mixed-effects models. These associations by age are presented in
Table @ref(tab:sleep-outcomes) and Figure @ref(fig:sleep-by-volume-fig).
Higher physical activity volume was associated with longer sleep
duration, higher sleep efficiency, earlier sleep onset, and more regular
sleep. Although we observed statistically significant curvilinear
relationships, these did not appear to meaningfully change the
associations. The association with sleep regularity was the largest and
declined only slightly with age (*β* = 0.30 \[0.29, 0.31\] at age 11 and
0.27 \[0.25, 0.29\] at age 65). The associations with sleep duration and
efficiency weakened with age, and by age 65 higher volume was associated
with slightly lower sleep efficiency (*β* = -0.03 \[-0.04, -0.02\]),
whereas the association with earlier sleep onset strengthened with age
(*β* = -0.09 \[-0.10, -0.08\] at age 11 and -0.15 \[-0.16, -0.14\] at
age 65).

<table>
<caption>(#tab:sleep-outcomes) Physical activity predicting sleep
controlling for SES, sex, and BMI z-score.</caption>
<thead>
<tr>
<th style="text-align: left;">Term</th>
<th style="text-align: left;">PA volume (z) <span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">PA volume (z) <span
class="math inline"><em>p</em></span></th>
<th style="text-align: left;">PA intensity (z) <span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">PA intensity (z) <span
class="math inline"><em>p</em></span></th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Sleep duration (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.00 [-0.16, 0.16]</td>
<td style="text-align: left;">.996</td>
<td style="text-align: left;">0.00 [-0.15, 0.14]</td>
<td style="text-align: left;">.995</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.10 [0.09, 0.11]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.05 [0.05, 0.06]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.02 [-0.03, 0.00]</td>
<td style="text-align: left;">.007</td>
<td style="text-align: left;">-0.02 [-0.03, -0.01]</td>
<td style="text-align: left;">.003</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">.612</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">-0.01 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">.001</td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">Sleep efficiency (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.15 [-0.01, 0.31]</td>
<td style="text-align: left;">.062</td>
<td style="text-align: left;">0.15 [-0.02, 0.32]</td>
<td style="text-align: left;">.086</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.03 [0.03, 0.04]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.02 [0.02, 0.03]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.01 [-0.02, 0.01]</td>
<td style="text-align: left;">.323</td>
<td style="text-align: left;">-0.01 [-0.03, 0.00]</td>
<td style="text-align: left;">.067</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.02 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">-0.02 [-0.02, -0.02]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.095</td>
<td style="text-align: left;">0.01 [0.00, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">Sleep onset (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.09 [-0.18, 0.37]</td>
<td style="text-align: left;">.503</td>
<td style="text-align: left;">0.12 [-0.14, 0.37]</td>
<td style="text-align: left;">.379</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">-0.12 [-0.12, -0.11]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.06 [-0.06, -0.05]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.01 [-0.02, 0.00]</td>
<td style="text-align: left;">.036</td>
<td style="text-align: left;">0.00 [-0.02, 0.01]</td>
<td style="text-align: left;">.524</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.02 [0.02, 0.03]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.01 [0.00, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.01 [0.01, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.01 [0.01, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">Sleep regularity (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.18 [0.04, 0.33]</td>
<td style="text-align: left;">.014</td>
<td style="text-align: left;">0.06 [-0.14, 0.25]</td>
<td style="text-align: left;">.569</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.29 [0.27, 0.30]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.14 [0.13, 0.15]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [-0.02, 0.02]</td>
<td style="text-align: left;">.730</td>
<td style="text-align: left;">-0.08 [-0.10, -0.06]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.15 [-0.16, -0.14]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.09 [-0.09, -0.08]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">.049</td>
<td style="text-align: left;">-0.05 [-0.05, -0.05]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.03 [-0.04, -0.03]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.01 [0.01, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
</tbody>
</table>

*Note.* Adjusted for SES, sex, and BMI z-score. Rows are grouped by
sleep outcome; columns give the physical activity predictor. Variables
marked (z) are standardised; <sup>2</sup> denotes a quadratic term and ×
an interaction. Age was centred at 35.6 years (the mean across
person-days) and scaled per 10 years, so main effects are associations
at age 35.6 and age interactions are the change per 10 years.

 

<img src="../Figures/main/Sleep on scale_pa_volume by Age_nolog.jpg" alt="Standardized sleep metrics predicted by physical activity volume. The panels on the left show the curvilinear relationship between PA volume and each of the sleep outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Standardized sleep metrics predicted by physical activity volume. The
panels on the left show the curvilinear relationship between PA volume
and each of the sleep outcomes at the ages indicated in each column. The
panels on the right show the same relationships but by age continuously.
The white band indicates predictions which were not significantly
different from zero.
</p>

## The association between physical activity intensity and sleep

We estimated how physical activity intensity is associated with sleep
across different age groups and present the results controlling for sex,
SES, and BMI, in Table @ref(tab:sleep-outcomes) and Figure
@ref(fig:sleep-by-intensity-fig). We observed weak associations between
physical activity intensity and sleep duration, sleep efficiency, and
sleep onset, with a stronger relationship observed with sleep
regularity. As with physical activity volume, we observed statistically
significant curvilinear relationships for all sleep outcomes except
sleep duration, but these were only meaningful for the association
between physical activity intensity and sleep regularity, in which lower
intensity was associated with poorer sleep regularity and the benefits
of increasing intensity diminished past one standard deviation above the
mean. This association was steepest in children and young adults,
although few of them had very low intensity. The associations with sleep
duration and efficiency weakened with age, and the association with
sleep regularity declined markedly, from *β* = 0.26 \[0.25, 0.28\] at
age 11 to 0.00 \[-0.02, 0.01\] at age 65.

<img src="../Figures/main/Sleep on scale_pa_intensity by Age_nolog.jpg" alt="Sleep metrics predicted by physical activity intensity. The panels on the left show the curvilinear relationship between PA intensity and sleep outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Sleep metrics predicted by physical activity intensity. The panels on
the left show the curvilinear relationship between PA intensity and
sleep outcomes at the ages indicated in each column. The panels on the
right show the same relationships but by age continuously. The white
band indicates predictions which were not significantly different from
zero.
</p>

## The association between of sleep duration and physical activity

We estimated the association between sleep duration and physical
activity by age. Results, controlling for sex, SES, and BMI are
presented in Table @ref(tab:pa-outcomes) and Figure
@ref(fig:PA-by-sleep-duration-fig). At higher ages, both physical
activity volume and intensity are lower than for lower ages. There was a
weak curvilinear association between sleep duration and next-day
physical activity volume and intensity, with physical activity highest
following nights of slightly longer than average sleep. The association
with volume weakened with age and was slightly negative by age 65 (*β* =
0.05 \[0.04, 0.05\] at age 11 and -0.02 \[-0.02, -0.01\] at age 65),
whereas the association with intensity changed little with age.

<table>
<caption>(#tab:pa-outcomes) Sleep predicting physical activity
controlling for SES, sex, and BMI z-score</caption>
<thead>
<tr>
<th style="text-align: left;">Term</th>
<th style="text-align: left;">PA volume (z) <span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">PA volume (z) <span
class="math inline"><em>p</em></span></th>
<th style="text-align: left;">PA intensity (z) <span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">PA intensity (z) <span
class="math inline"><em>p</em></span></th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Sleep duration (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.14 [-0.17, 0.44]</td>
<td style="text-align: left;">.378</td>
<td style="text-align: left;">-0.07 [-0.19, 0.05]</td>
<td style="text-align: left;">.254</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration</td>
<td style="text-align: left;">0.02 [0.01, 0.02]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.03 [0.02, 0.03]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.23 [-0.24, -0.22]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.27 [-0.28, -0.26]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.03, -0.02]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.051</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep duration<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.326</td>
</tr>
<tr>
<td style="text-align: left;">Sleep efficiency (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.13 [-0.17, 0.42]</td>
<td style="text-align: left;">.412</td>
<td style="text-align: left;">-0.07 [-0.20, 0.05]</td>
<td style="text-align: left;">.242</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency</td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">.279</td>
<td style="text-align: left;">0.01 [0.00, 0.01]</td>
<td style="text-align: left;">.009</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.23 [-0.24, -0.21]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.27 [-0.28, -0.26]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.200</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep efficiency<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.227</td>
</tr>
<tr>
<td style="text-align: left;">Sleep onset (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.13 [-0.18, 0.43]</td>
<td style="text-align: left;">.410</td>
<td style="text-align: left;">-0.08 [-0.21, 0.04]</td>
<td style="text-align: left;">.202</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset</td>
<td style="text-align: left;">-0.02 [-0.03, -0.02]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.02, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.22 [-0.23, -0.21]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.27 [-0.28, -0.26]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.811</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.012</td>
<td style="text-align: left;">0.01 [0.01, 0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep onset<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.002</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.020</td>
</tr>
<tr>
<td style="text-align: left;">Sleep regularity (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.04 [-0.27, 0.34]</td>
<td style="text-align: left;">.817</td>
<td style="text-align: left;">-0.18 [-0.30, -0.05]</td>
<td style="text-align: left;">.006</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity</td>
<td style="text-align: left;">0.12 [0.11, 0.12]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.06 [0.06, 0.07]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.24 [-0.26, -0.22]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.32 [-0.34, -0.31]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.448</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.414</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep regularity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.145</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">.023</td>
</tr>
</tbody>
</table>

*Note.* Adjusted for SES, sex, and BMI z-score. Rows are grouped by the
previous night’s sleep predictor; columns give the physical activity
outcome. Variables marked (z) are standardised; <sup>2</sup> denotes a
quadratic term and × an interaction. Age was centred at 35.6 years (the
mean across person-days) and scaled per 10 years, so main effects are
associations at age 35.6 and age interactions are the change per 10
years.

 

<img src="../Figures/main/PA on scale_sleep_duration_lag by Age_nolog.jpg" alt="Physical activity is predicted by the duration of sleep the previous night. The panels on the left show the curvilinear relationship between sleep duration and the physical activity outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Physical activity is predicted by the duration of sleep the previous
night. The panels on the left show the curvilinear relationship between
sleep duration and the physical activity outcomes at the ages indicated
in each column. The panels on the right show the same relationships but
by age continuously. The white band indicates predictions which were not
significantly different from zero.
</p>

## The association between sleep efficiency and physical activity

We estimated the association between sleep efficiency and physical
activity by age. Results, controlling for sex, SES, and BMI are
presented in Table @ref(tab:pa-outcomes) and Figure
@ref(fig:PA-by-sleep-efficiency-fig). At the mean age there was no
linear association between sleep efficiency and physical activity
volume; this association shifted slightly with age (*β* = -0.01 \[-0.01,
0.00\] at age 11 and 0.01 \[0.01, 0.02\] at age 65) but was negligible
throughout. There was only a weak positive association between sleep
efficiency and more intense physical activity.

<img src="../Figures/main/PA on scale_sleep_efficiency_lag by Age_nolog.jpg" alt="Physical activity predicted by previous night sleep efficiency. The panels on the left show the curvilinear relationship between sleep efficiency and each of the physical activity outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Physical activity predicted by previous night sleep efficiency. The
panels on the left show the curvilinear relationship between sleep
efficiency and each of the physical activity outcomes at the ages
indicated in each column. The panels on the right show the same
relationships but by age continuously. The white band indicates
predictions which were not significantly different from zero.
</p>

## The association between sleep onset and physical activity

We estimated the association between sleep onset and physical activity
by age. Results, controlling for sex, SES, and BMI are presented in
Table @ref(tab:pa-outcomes) and Figure @ref(fig:PA-by-sleep-onset-fig).
Earlier sleep onset was weakly associated with higher next-day physical
activity volume and intensity, and these associations were attenuated at
older ages (volume: *β* = -0.03 \[-0.04, -0.02\] at age 11 and -0.01
\[-0.02, -0.01\] at age 65). By age 65, earlier sleep onset was
associated with marginally lower next-day intensity (*β* = 0.01 \[0.00,
0.02\]).

<img src="../Figures/main/PA on scale_sleep_onset_lag by Age_nolog.jpg" alt="Physical activity predicted by previous night sleep onset. The panels on the left show the curvilinear relationship between sleep onset and each of the physical activity outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Physical activity predicted by previous night sleep onset. The panels on
the left show the curvilinear relationship between sleep onset and each
of the physical activity outcomes at the ages indicated in each column.
The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.
</p>

## The association between sleep regularity and physical activity

We estimated the association between sleep regularity and physical
activity by age. Results, controlling for sex, SES, and BMI are
presented in Table @ref(tab:pa-outcomes) and Figure
@ref(fig:PA-by-sleep-regularity-fig). There was a positive linear
relationship between sleep regularity and both physical activity volume
and intensity. These relationships weakened with age (volume: *β* = 0.14
\[0.13, 0.14\] at age 11 and 0.09 \[0.08, 0.10\] at age 65), and the
association with intensity was negligible in older adults (*β* = 0.02
\[0.02, 0.03\] at age 65).

<img src="../Figures/main/PA on scale_sleep_regularity_lag by Age_nolog.jpg" alt="Physical activity predicted by previous night sleep regularity. The panels on the left show the curvilinear relationship between sleep regularity and each of the physical activity outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Physical activity predicted by previous night sleep regularity. The
panels on the left show the curvilinear relationship between sleep
regularity and each of the physical activity outcomes at the ages
indicated in each column. The panels on the right show the same
relationships but by age continuously. The white band indicates
predictions which were not significantly different from zero.
</p>

# Discussion

In this study, we collated and harmonised accelerometer-assessed
physical activity and sleep data from 20 studies around the world,
covering participants across the lifespan. We found that both the volume
and intensity of physical activity were associated with longer sleep
duration, higher sleep efficiency, earlier sleep onset, and more regular
sleep. These relationships were the strongest for sleep regularity,
although the relationship with physical activity intensity weakened with
age. Although robust, these associations were small: a 1 SD (23.7 mg)
higher physical activity volume was associated with 4.7 points higher
sleep regularity, 11.8 min longer sleep, and 11 min earlier sleep onset
at age 11, and with 4.4 points, 4.8 min, and 19 min, respectively, at
age 65.

When examining the reverse direction, we observed that those who slept
at a more regular time engaged in more total and more intense physical
activity the following day, although this association weakened with age.
Earlier sleep onset was also associated with more physical activity the
following day, but this association was small. Longer sleep duration was
associated with slightly more physical activity the next day, up to just
above average sleep duration, while sleep efficiency was only weakly
associated with physical activity intensity and not with volume. Our
sensitivity analyses suggest that these findings were largely robust to
the decisions made during analysis.

Our findings broadly align with meta-analyses of experimental research,
although the strengths of the relationships we observed were much weaker
\[16,17\]. For example, \[16\] found small (0.22 − 0.30) but significant
relationships between physical activity and sleep duration and
efficiency in adults using experimental studies for both acute and
regular physical activity. It is difficult to directly compare these
effect sizes, however, as the experimental studies directly manipulated
physical activity, whereas our study examined the relationship between
habitual physical activity and sleep. Accelerometry also distinguishes
quiet wakefulness from sleep less well than polysomnography,
particularly in children \[52,53\], which may attenuate associations.
Trials also suggest that the benefits of exercise for sleep are
concentrated in people whose sleep is poor at baseline \[17,18\],
whereas our analysis did not target people with poor sleep. Similarly,
most of the previous studies which have examined acute changes to
physical activity were conducted in laboratory settings. These studies,
while useful for understanding the potential mechanisms, are not
ecologically valid and may not reflect the relationship between physical
activity and sleep in the real world. Laboratory studies also tend to
use more sensitive measures of sleep and physical activity, which may
generate less statistical noise and make relationships easier to detect.
It is important to note that sleep duration will sometimes be influenced
by parameters external to the individual, such as work or school
schedules or parenting duties, rather than by individual health
behaviours. The timing of activity may also matter, as strenuous
exercise close to bedtime can delay and disrupt sleep \[64,65\]. In
supplementary analyses, however, associations were broadly similar
regardless of the time of day of the most active hour, and across
seasons.

Our findings can also be compared with observational studies of
day-to-day sleep and physical activity. A meta-analysis of 33 studies in
adults did not support a bidirectional daily association: some sleep
parameters were weakly associated with next-day physical activity, and
more active individuals slept slightly less \[24\]. Two recent studies
of consumer wearables found that previous-day activity had little
association with sleep \[26\], that shorter sleep was followed by more
activity the next day \[26,27\], and that earlier-than-usual sleep onset
was followed by more moderate-to-vigorous activity \[27\]. Our finding
that earlier sleep onset preceded more physical activity is consistent
with these studies, although the association was small in our data.
However, we found clearer associations between physical activity and
that night’s sleep, and physical activity was highest after slightly
longer, rather than shorter, than average sleep. One explanation is that
our models did not separate within-person (day-to-day) from
between-person (habitual) associations, which can differ \[66\], so our
estimates partly reflect stable differences between people. Neither
wearable study examined sleep regularity, the outcome most strongly
associated with physical activity in our data. However, it was
identified as a direction for future research \[27\].

We found the most evidence for relationships between sleep regularity
with physical activity volume and intensity. This relationship appeared
to be bi-directional, with sleep regularity predicting physical activity
the following day, and physical activity predicting sleep onset and
regularity that night. This is consistent with the idea that sleep and
physical activity are mutually reinforcing behaviours \[15\], or that
both reflect shared underlying factors, such as daily routines, work or
school schedules, and health priorities. Consistent physical activity
may also help stabilise circadian rhythms \[28\]. Sleep regularity is a
stronger predictor of mortality than sleep duration \[67\], and
irregular sleepers remain at higher risk of cardiovascular events and
depression even when they meet sleep duration recommendations \[68,69\],
so the finding that physical activity is associated with sleep
regularity may be particularly important. In pooled data from six
cohorts, the combination of irregular sleep and low daily step counts
was associated with higher adiposity and a less favourable lipid profile
\[70\].

The association between physical activity volume and sleep regularity
was largely consistent across the lifespan, as a large study of adult
wearable users found for other day-level associations \[26\]. Other
associations changed more markedly with age: those of physical activity
intensity with sleep regularity, and of sleep with next-day physical
activity, weakened with age and were small or negligible in older
adults, consistent with evidence that the interplay between sleep and
activity differs in very old adults \[66\]. Meta-analyses in adult
populations have generally found associations \[16\], while those
conducted in children and adolescents are less clear \[15,21,22\]. That
our strongest association, with sleep regularity, varied little with age
suggests that differences between these meta-analyses may reflect
factors beyond the age of their populations, such as study design or the
measures of physical activity and sleep, and demonstrates the value of
individual participant data for examining these questions.

These findings have implications for public health recommendations. Our
results support promoting physical activity as a strategy for
maintaining regular sleep patterns across the lifespan. This message
fits with Australia’s recent 24-hour movement guidelines for adults,
which pair physical activity recommendations with consistent bed and
wake times \[71\]. Given the well-established links between sleep and
overall health \[3,4\], incorporating physical activity into public
health messaging may have downstream benefits for population health.
However, our findings suggest that physical activity is not a universal
solution for improving all aspects of sleep. Public health messages
should, therefore, emphasise the role of physical activity in promoting
consistent sleep schedules rather than framing physical activity as a
universal solution for improving sleep. Additionally, we did not examine
these relationships specifically in individuals with sleep disorders or
other health conditions, where the effects of physical activity may
differ. Future research should explore whether targeting sleep
regularity through physical activity interventions can provide
meaningful health benefits beyond sleep itself.

Our study has several strengths, including the use of
accelerometer-assessed measures, which reduce the bias associated with
self-reported sleep and activity data, and the harmonisation of
individual data from multiple studies, conducted in geographically
diverse regions and among multiple age groups, allowing for robust and
generalisable findings. We also used modern methods for processing and
estimating physical activity, including 24-hour wear protocols, and
avoiding the use of intensity cut-points. However, the study is not
without limitations. Despite our efforts to harmonise data, variability
in accelerometer wear locations and slight differences in data
collection protocols across studies may have introduced measurement
inconsistencies. Physical activity estimates from different wear
locations are strongly correlated but agree less well in absolute terms,
particularly between wrist and hip \[72\]. We also chose to maximise the
data available for analysis, at the expense of some precision in the
estimates. Specifically, we included participants with four or more days
of valid wear time, while some recommend at least seven nights to
estimate metrics such as sleep regularity \[73\], and reliable estimates
of an individual’s night-to-night variability in sleep may require more
than a month of data \[74\]. However, the processing method we used
specifically allows for comparisons to day-pairs and this methodology
has been used by others \[e.g., 67\]. Restricting the sample to
participants with at least seven valid days gave a similar association
between physical activity volume and sleep regularity (*β* = 0.31
\[0.30, 0.33\] at age 11 and 0.24 \[0.22, 0.27\] at age 65). Sleep
regularity scores also depend on the software used to calculate them,
with *GGIR* producing scores around 20 points lower than the *sleepreg*
package \[75\], so absolute values from our data should not be compared
directly with studies that used other implementations. As in any
observational study, the temporal ordering of our lagged models does not
establish causality, and although we controlled for several potential
confounders, unmeasured factors may explain the observed associations.
Finally, while our study captured data from across the lifespan, the
limited representation of certain age groups, especially adolescents and
young adults, restricts the generalisability of our conclusions to these
populations. The limited research on those aged 18-35 is particularly
concerning given that young adults are the most likely to report
difficulty falling asleep \[11\].

# Conclusion

Our study provides new insights into the relationship between physical
activity and sleep across the lifespan using a large, globally diverse
dataset with device-based measurements. We found that physical activity
was most consistently associated with sleep regularity, with weaker and
less consistent associations observed for sleep duration, efficiency,
and onset. Higher physical activity levels were associated with greater
sleep regularity at all ages, and more regular sleep patterns predicted
higher physical activity the following day. However, the association of
sleep with next-day physical activity was attenuated in older adults,
suggesting that the interplay between sleep and physical activity may
change with age. Findings suggest that public health efforts to promote
physical activity may be particularly valuable for improving sleep
consistency rather than sleep duration or efficiency. While physical
activity is unlikely to be a universal solution for sleep improvement,
encouraging regular movement could support stable sleep patterns, which
are increasingly recognized as a key determinant of overall health.
Future research should explore how targeted interventions—particularly
those addressing sleep regularity—may enhance both sleep and physical
activity behaviours across different age groups and populations.

# References

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Luyster FS, Strollo PJ, Zee PC,
Walsh JK. [Sleep: A Health
Imperative](https://doi.org/10.5665/sleep.1846). Sleep. 2012
June;35(6):727–34. </span>

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Ramar K, Malhotra RK, Carden KA,
Martin JL, Abbasi-Feinberg F, Aurora RN, Kapur VK, Olson EJ, Rosen CL,
Rowley JA, Shelgikar AV, Trotti LM. [Sleep is essential to health: An
American Academy of Sleep Medicine position
statement](https://doi.org/10.5664/jcsm.9476). Journal of Clinical Sleep
Medicine. 2021 Oct;17(10):2115–9. </span>

<span class="csl-left-margin">3.
</span><span class="csl-right-inline">Huang BH, Duncan MJ, Cistulli PA,
Nassar N, Hamer M, Stamatakis E. [Sleep and physical activity in
relation to all-cause, cardiovascular disease and cancer mortality
risk](https://doi.org/10.1136/bjsports-2021-104046). British Journal of
Sports Medicine. 2022 July;56(13):718–24. </span>

<span class="csl-left-margin">4.
</span><span class="csl-right-inline">Kwok CS, Kontopantelis E,
Kuligowski G, Gray M, Muhyaldeen A, Gale CP, Peat GM, Cleator J,
Chew-Graham C, Loke YK, Mamas MA. [Self-Reported Sleep Duration and
Quality and Cardiovascular Disease and Mortality: A Dose-Response
Meta-Analysis](https://doi.org/10.1161/jaha.118.008552). Journal of the
American Heart Association. 2018 Aug;7(15). </span>

<span class="csl-left-margin">5.
</span><span class="csl-right-inline">Ma QQ, Yao Q, Lin L, Chen GC, Yu
JB. [Sleep duration and total cancer mortality: A meta-analysis of
prospective studies](https://doi.org/10.1016/j.sleep.2016.06.036). Sleep
Medicine. 2016 Nov;27–28:39–44. </span>

<span class="csl-left-margin">6.
</span><span class="csl-right-inline">Wakefield JRH, Bowe M, Kellezi B,
Butcher A, Groeger JA. [Longitudinal associations between family
identification, loneliness, depression, and sleep
quality](https://doi.org/10.1111/bjhp.12391). British Journal of Health
Psychology. 2020 Feb;25(1):1–16. </span>

<span class="csl-left-margin">7.
</span><span class="csl-right-inline">Sletten TL, Weaver MD, Foster RG,
Gozal D, Klerman EB, Rajaratnam SMW, Roenneberg T, Takahashi JS, Turek
FW, Vitiello MV, Young MW, Czeisler CA. [The importance of sleep
regularity: A consensus statement of the National Sleep Foundation sleep
timing and variability
panel](https://doi.org/10.1016/j.sleh.2023.07.016). Sleep Health. 2023
Dec;9(6):801–20. </span>

<span class="csl-left-margin">8.
</span><span class="csl-right-inline"><span class="nocase">St-Onge MP,
Aggarwal B, Fernandez-Mendoza J, Johnson D, Kline CE, Knutson KL,
Redeker N, Grandner MA, on behalf of the American Heart Association
Council on Lifestyle and Cardiometabolic Health; Council on
Cardiovascular and Stroke Nursing; Council on Clinical Cardiology; and
Council on Quality of Care and Outcomes Research</span>.
[Multidimensional Sleep Health: Definitions and Implications for
Cardiometabolic Health: A Scientific Statement From the American Heart
Association](https://doi.org/10.1161/HCQ.0000000000000139). Circulation:
Cardiovascular Quality and Outcomes. 2025 May;18(5). </span>

<span class="csl-left-margin">9.
</span><span class="csl-right-inline">Kalkanis A, Lenkens D,
Steiropoulos P, Testelmans D. [Sleep regularity as an important
component of sleep hygiene: A systematic
review](https://doi.org/10.1016/j.smrv.2025.102203). Sleep Medicine
Reviews. 2025 Dec;84:102203. </span>

<span class="csl-left-margin">10.
</span><span class="csl-right-inline">Gariepy G, Danna S, Gobiņa I,
Rasmussen M, Gaspar De Matos M, Tynjälä J, Janssen I, Kalman M,
Villeruša A, Husarova D, Brooks F, Elgar FJ, Klavina-Makrecka S,
Šmigelskas K, Gaspar T, Schnohr C. [How Are Adolescents Sleeping?
Adolescent Sleep Patterns and Sociodemographic Differences in 24
European and North American
Countries](https://doi.org/10.1016/j.jadohealth.2020.03.013). Journal of
Adolescent Health. 2020 June;66(6):S81–8. </span>

<span class="csl-left-margin">11.
</span><span class="csl-right-inline">Ng AE, Black LI, Adjaye-Gbewonyo
D. Short sleep duration and sleep difficulties among adults: United
States, 2024 \[Internet\]. Hyattsville, MD: National Center for Health
Statistics; 2026 Apr \[cited 2026 Oct 10\]. Report No.: 559. Available
from: <https://www.cdc.gov/nchs/products/databriefs/db559.htm></span>

<span class="csl-left-margin">12.
</span><span class="csl-right-inline">Hirshkowitz M, Whiton K, Albert
SM, Alessi C, Bruni O, DonCarlos L, Hazen N, Herman J, Adams Hillard PJ,
Katz ES, Kheirandish-Gozal L, Neubauer DN, O’Donnell AE, Ohayon M,
Peever J, Rawding R, Sachdeva RC, Setters B, Vitiello MV, Ware JC.
[National Sleep Foundation’s updated sleep duration recommendations:
Final report](https://doi.org/10.1016/j.sleh.2015.10.004). Sleep Health.
2015 Dec;1(4):233–43. </span>

<span class="csl-left-margin">13.
</span><span class="csl-right-inline">Dzierzewski JM, Hirshkowitz M,
Dautovich ND, Nielson SA, Miller AN, Erickson AJ, Vitiello MV. [How much
sleep do you need: A 10-year systematic review of National Sleep
Foundation’s sleep duration recommendations and examination of sex
differences](https://doi.org/10.1016/j.sleh.2026.05.011). Sleep Health.
2026 Aug;12(4):599–611. </span>

<span class="csl-left-margin">14.
</span><span class="csl-right-inline">Lim DC, Najafi A, Afifi L,
Bassetti CL, Buysse DJ, Han F, Högl B, Melaku YA, Morin CM, Pack AI,
Poyares D, Somers VK, Eastwood PR, Zee PC, Jackson CL. [The need to
promote sleep health in public health agendas across the
globe](https://doi.org/10.1016/S2468-2667(23)00182-2). The Lancet Public
Health. 2023 Oct;8(10):e820–6. </span>

<span class="csl-left-margin">15.
</span><span class="csl-right-inline">Kline CE, Hillman CH, Bloodgood
Sheppard B, Tennant B, Conroy DE, Macko RF, Marquez DX, Petruzzello SJ,
Powell KE, Erickson KI. [Physical activity and sleep: An updated
umbrella review of the 2018 Physical Activity Guidelines Advisory
Committee report](https://doi.org/10.1016/j.smrv.2021.101489). Sleep
Medicine Reviews. 2021 Aug;58:101489. </span>

<span class="csl-left-margin">16.
</span><span class="csl-right-inline">Kredlow MA, Capozzoli MC, Hearon
BA, Calkins AW, Otto MW. [The effects of physical activity on sleep: A
meta-analytic review](https://doi.org/10.1007/s10865-015-9617-6).
Journal of Behavioral Medicine. 2015 June;38(3):427–49. </span>

<span class="csl-left-margin">17.
</span><span class="csl-right-inline">Liang Z, Zhao S, Tian S, Wang C,
Zhang M, Xu K, Yin M, Ng S, Ning Z. [Determinants and interrelations of
exercise effects on sleep quality: A multimethod
meta-analysis](https://doi.org/10.1016/j.smrv.2026.102239). Sleep
Medicine Reviews. 2026 Apr;86:102239. </span>

<span class="csl-left-margin">18.
</span><span class="csl-right-inline">Xiong Z, Yuan Y, Qiu B, Yang Y,
Bai Y, Wang J, Wang T, Liu H, ShangGuan Y, Jiang S, Wang F, Ding W, Wang
Z, Li Y, Zhang L. [Optimal exercise type and dose to improve sleep
quality in older adults: A systematic review and network
meta-analysis](https://doi.org/10.1186/s12877-025-06607-z). BMC
Geriatrics. 2025 Nov;25(1):1031. </span>

<span class="csl-left-margin">19.
</span><span class="csl-right-inline">Bu Z, Liu F, Shahjalal M, Song Y,
Li M, Zhuo R, Zhong Q, Du Y, Lu C, Yang Z, Yang H, Zhong P, Liu J, Liu
Z. [Effects of various exercise interventions in insomnia patients: A
systematic review and network
meta-analysis](https://doi.org/10.1136/bmjebm-2024-113512). BMJ
Evidence-Based Medicine. 2026 Apr;31(2):102–13. </span>

<span class="csl-left-margin">20.
</span><span class="csl-right-inline">Bull FC, Al-Ansari SS, Biddle S,
Borodulin K, Buman MP, Cardon G, Carty C, Chaput JP, Chastin S, Chou R,
Dempsey PC, DiPietro L, Ekelund U, Firth J, Friedenreich CM, Garcia L,
Gichu M, Jago R, Katzmarzyk PT, Lambert E, Leitzmann M, Milton K, Ortega
FB, Ranasinghe C, Stamatakis E, Tiedemann A, Troiano RP, Van Der Ploeg
HP, Wari V, Willumsen JF. [World Health Organization 2020 guidelines on
physical activity and sedentary
behaviour](https://doi.org/10.1136/bjsports-2020-102955). British
Journal of Sports Medicine. 2020 Dec;54(24):1451–62. </span>

<span class="csl-left-margin">21.
</span><span class="csl-right-inline"><span class="nocase">Antczak D,
Lonsdale C, Lee J, Hilland T, Duncan MJ, del Pozo Cruz B, Hulteen RM,
Parker PD, Sanders T</span>. [Physical activity and sleep are
inconsistently related in healthy children: A systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2020.101278). Sleep
Medicine Reviews. 2020 June;51:101278. </span>

<span class="csl-left-margin">22.
</span><span class="csl-right-inline">Lang C, Kalak N, Brand S,
Holsboer-Trachsler E, Pühse U, Gerber M. [The relationship between
physical activity and sleep from mid adolescence to early adulthood. A
systematic review of methodological approaches and
meta-analysis](https://doi.org/10.1016/j.smrv.2015.07.004). Sleep
Medicine Reviews. 2016 Aug;28:32–45. </span>

<span class="csl-left-margin">23.
</span><span class="csl-right-inline">Song H, Jiang X, Li Y, Cao Y, He
C, Zuo H, Liu T, Zhang M. [Effects of physical activity on sleep in
children and adolescents: A systematic review and meta-analysis of
randomised controlled
trials](https://doi.org/10.1007/s00787-025-02892-6). European Child &
Adolescent Psychiatry. 2026 Feb;35(2):397–411. </span>

<span class="csl-left-margin">24.
</span><span class="csl-right-inline">Atoui S, Chevance G, Romain AJ,
Kingsbury C, Lachance JP, Bernard P. [Daily associations between sleep
and physical activity: A systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2021.101426). Sleep
Medicine Reviews. 2021 June;57:101426. </span>

<span class="csl-left-margin">25.
</span><span class="csl-right-inline">Huang WY, Ho RS, Tremblay MS, Wong
SH. [Relationships of physical activity and sedentary behaviour with the
previous and subsequent nights’ sleep in children and youth: A
systematic review and meta-analysis](https://doi.org/10.1111/jsr.13378).
Journal of Sleep Research. 2021;30(6):e13378. </span>

<span class="csl-left-margin">26.
</span><span class="csl-right-inline">Fitton J, Nguyen DP, Lechat B,
Scott H, Toson B, Manners J, Dunbar C, Sansom K, Pinilla L, Hudson A,
Naik G, Vakulin A, Reynolds AC, Catcheside P, Escourrou P, Eckert DJ.
[Bidirectional associations between sleep and physical activity
investigated using large-scale objective monitoring
data](https://doi.org/10.1038/s43856-025-01226-6). Communications
Medicine. 2025 Dec;5(1):519. </span>

<span class="csl-left-margin">27.
</span><span class="csl-right-inline">Leota J, Czeisler MÉ, Le F, Presby
DM, Capodilupo ER, Scott H, Wiley JF, Drummond SPA, Rajaratnam SMW,
Facer-Childs ER. [Sleep duration and timing are associated with next-day
physical activity: Insights from two large-scale wearable sensor
studies](https://doi.org/10.1073/pnas.2420846122). Proceedings of the
National Academy of Sciences. 2025 July;122(27):e2420846122. </span>

<span class="csl-left-margin">28.
</span><span class="csl-right-inline">Korkutata A, Korkutata M, Lazarus
M. [The impact of exercise on sleep and sleep
disorders](https://doi.org/10.1038/s44323-024-00018-w). npj Biological
Timing and Sleep. 2025 Feb;2(1):5. </span>

<span class="csl-left-margin">29.
</span><span class="csl-right-inline">Plekhanova T, Rowlands AV, Davies
M, Edwardson CL, Hall A, Yates T. Effect of exercise on sleep and
bi-directional associations with accelerometer-assessed physical
activity in men with obesity. Applied Physiology, Nutrition, and
Metabolism \[Internet\]. 2021 June \[cited 2026 Oct 12\];46(6):597–605.
Available from:
<https://cdnsciencepub.com/doi/10.1139/apnm-2020-0361></span>

<span class="csl-left-margin">30.
</span><span class="csl-right-inline">Aiello KD, Caughey WG, Nelluri B,
Sharma A, Mookadam F, Mookadam M. [Effect of exercise training on sleep
apnea: A systematic review and
meta-analysis](https://doi.org/10.1016/j.rmed.2016.05.015). Respiratory
Medicine. 2016;116:85–92. </span>

<span class="csl-left-margin">31.
</span><span class="csl-right-inline">Banno M, Harada Y, Taniguchi M,
Tobita R, Tsujimoto H, Tsujimoto Y, Kataoka Y, Noda A. [Exercise can
improve sleep quality: A systematic review and
meta-analysis](https://doi.org/10.7717/peerj.5172). PeerJ. 2018;2018(7).
</span>

<span class="csl-left-margin">32.
</span><span class="csl-right-inline">Bartel KA, Gradisar M, Williamson
P. [Protective and risk factors for adolescent sleep: A meta-analytic
review](https://doi.org/10.1016/j.smrv.2014.08.002). Sleep Medicine
Reviews. 2015 June;21:72–85. </span>

<span class="csl-left-margin">33.
</span><span class="csl-right-inline">Edwards BA, Bristow C, O’Driscoll
DM, Wong A-M, Ghazi L, Davidson ZE, Young A, Truby H, Haines TP,
Hamilton GS. [Assessing the impact of diet, exercise and the combination
of the two as a treatment for OSA: A systematic review and
meta-analysis](https://doi.org/10.1111/resp.13580). Respirology.
2019;24(8):740–51. </span>

<span class="csl-left-margin">34.
</span><span class="csl-right-inline">Gao Y-N, Wu Y-C, Lin S-Y, Chang
JZ-C, Tu Y-K. [Short-term efficacy of minimally invasive treatments for
adult obstructive sleep apnea: A systematic review and network
meta-analysis of randomized controlled
trials](https://doi.org/10.1016/j.jfma.2018.02.008). Journal of the
Formosan Medical Association. 2019;118(4):750–65. </span>

<span class="csl-left-margin">35.
</span><span class="csl-right-inline">Iftikhar IH, Bittencourt L,
Youngstedt SD, Ayas N, Cistulli P, Schwab R, Durkin MW, Magalang UJ.
[Comparative efficacy of CPAP, MADs, exercise-training, and dietary
weight loss for sleep apnea: A network
meta-analysis](https://doi.org/10.1016/j.sleep.2016.06.001). Sleep
Medicine. 2017;30:7–14. </span>

<span class="csl-left-margin">36.
</span><span class="csl-right-inline">Iftikhar IH, Kline CE, Youngstedt
SD. [Effects of exercise training on sleep apnea: A
meta-analysis](https://doi.org/10.1007/s00408-013-9511-3). Lung.
2014;192(1):175–84. </span>

<span class="csl-left-margin">37.
</span><span class="csl-right-inline">Janssen X, Martin A, Hughes AR,
Hill CM, Kotronoulas G, Hesketh KR. [Associations of screen time,
sedentary time and physical activity with sleep in under 5s: A
systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2019.101226). Sleep
Medicine Reviews. 2020 Feb;49:101226. </span>

<span class="csl-left-margin">38.
</span><span class="csl-right-inline">Lederman O, Ward PB, Firth J,
Maloney C, Carney R, Vancampfort D, Stubbs B, Kalucy M, Rosenbaum S.
[Does exercise improve sleep quality in individuals with mental illness?
A systematic review and
meta-analysis](https://doi.org/10.1016/j.jpsychires.2018.11.004).
Journal of Psychiatric Research. 2019;109:96–106. </span>

<span class="csl-left-margin">39.
</span><span class="csl-right-inline">Lins-Filho OL, Pedrosa RP, Gomes
JML, Dantas Moraes SL, Vasconcelos BCE, Lemos CAA, Pellizzer EP. [Effect
of exercise training on subjective parameters in patients with
obstructive sleep apnea: A systematic review and
meta-analysis](https://doi.org/10.1016/j.sleep.2019.12.022). Sleep
Medicine. 2020;69:1–7. </span>

<span class="csl-left-margin">40.
</span><span class="csl-right-inline">Mendelson M, Bailly S, Marillier
M, Flore P, Borel JC, Vivodtzev I, Doutreleau S, Verges S, Tamisier R,
Pépin J-L. [Obstructive sleep apnea syndrome, objectively measured
physical activity and exercise training interventions: A systematic
review and meta-analysis](https://doi.org/10.3389/fneur.2018.00073).
Frontiers in Neurology. 2018;9(FEB). </span>

<span class="csl-left-margin">41.
</span><span class="csl-right-inline">Rubio-Arias JÁ, Marín-Cascales E,
Ramos-Campo DJ, Hernandez AV, Pérez-López FR. [Effect of exercise on
sleep quality and insomnia in middle-aged women: A systematic review and
meta-analysis of randomized controlled
trials](https://doi.org/10.1016/j.maturitas.2017.04.003). Maturitas.
2017;100:49–56. </span>

<span class="csl-left-margin">42.
</span><span class="csl-right-inline">Stutz J, Eiholzer R, Spengler CM.
[Effects of Evening Exercise on Sleep in Healthy Participants: A
Systematic Review and
Meta-Analysis](https://doi.org/10.1007/s40279-018-1015-0). Sports
Medicine. 2019;49(2):269–87. </span>

<span class="csl-left-margin">43.
</span><span class="csl-right-inline">Yang S-Y, Lan S-J, Yen Y-Y, Hsieh
Y-P, Kung P-T, Lan S-H. [Effects of Exercise on Sleep Quality in
Pregnant Women: A Systematic Review and Meta-analysis of Randomized
Controlled Trials](https://doi.org/10.1016/j.anr.2020.01.003). Asian
Nursing Research. 2020;14(1):1–10. </span>

<span class="csl-left-margin">44.
</span><span class="csl-right-inline">Yang P-Y, Ho K-H, Chen H-C, Chien
M-Y. [Exercise training improves sleep quality in middle-aged and older
adults with sleep problems: A systematic
review](https://doi.org/10.1016/S1836-9553(12)70106-6). Journal of
Physiotherapy. 2012;58(3):157–63. </span>

<span class="csl-left-margin">45.
</span><span class="csl-right-inline">Yang Y, Shin JC, Li D, An R.
[Sedentary Behavior and Sleep Problems: A Systematic Review and
Meta-Analysis](https://doi.org/10.1007/s12529-016-9609-0). International
Journal of Behavioral Medicine. 2017;24(4):481–92. </span>

<span class="csl-left-margin">46.
</span><span class="csl-right-inline">Riley RD, Dias S, Donegan S,
Tierney JF, Stewart LA, Efthimiou O, Phillippo DM. [Using individual
participant data to improve network meta-analysis
projects](https://doi.org/10.1136/bmjebm-2022-111931). BMJ
Evidence-Based Medicine. 2023 June;28(3):197–203. </span>

<span class="csl-left-margin">47.
</span><span class="csl-right-inline">Ioannidis J. [Next-generation
systematic reviews: Prospective meta-analysis, individual-level data,
networks and umbrella
reviews](https://doi.org/10.1136/bjsports-2017-097621). British Journal
of Sports Medicine. 2017 Oct;51(20):1456–8. </span>

<span class="csl-left-margin">48.
</span><span class="csl-right-inline">Hinde S, Spackman E.
[Bidirectional Citation Searching to Completion: An Exploration of
Literature Searching
Methods](https://doi.org/10.1007/s40273-014-0205-3). PharmacoEconomics.
2015 Jan;33(1):5–11. </span>

<span class="csl-left-margin">49.
</span><span class="csl-right-inline">Migueles JH, Rowlands AV, Huber F,
Sabia S, Van Hees VT. [GGIR: A Research Community–Driven Open Source R
Package for Generating Physical Activity and Sleep Outcomes From
Multi-Day Raw Accelerometer
Data](https://doi.org/10.1123/jmpb.2018-0063). Journal for the
Measurement of Physical Behaviour. 2019 Sept;2(3):188–96. </span>

<span class="csl-left-margin">50.
</span><span class="csl-right-inline">Van Hees VT, Sabia S, Jones SE,
Wood AR, Anderson KN, Kivimäki M, Frayling TM, Pack AI, Bucan M, Trenell
MI, Mazzotti DR, Gehrman PR, Singh-Manoux BA, Weedon MN. [Estimating
sleep parameters using an accelerometer without sleep
diary](https://doi.org/10.1038/s41598-018-31266-z). Scientific Reports.
2018 Aug;8(1). </span>

<span class="csl-left-margin">51.
</span><span class="csl-right-inline">Sanders T.
<span class="nocase">sleepIPD</span>: Opinionated accelerometer
reprocessing for the SleepIPD study \[Internet\]. 2023. Available from:
<https://motivation-and-behaviour.github.io/sleepIPD></span>

<span class="csl-left-margin">52.
</span><span class="csl-right-inline">Cepni AB, Burkart S, Zhu X, White
J, Finnegan O, Nelakuditi S, Beets M, Brown Iii D, Pate R, Welk G, De
Zambotti M, Ghosal R, Wang Y, Armstrong B, Adams E, Van Hees V, Glenn
Weaver R. [Evaluating the performance of open-source and proprietary
processing of actigraphy sleep estimation in children with suspected
sleep disorders: A comparison with
polysomnography](https://doi.org/10.1093/sleep/zsae267). SLEEP. 2025
Apr;48(4):zsae267. </span>

<span class="csl-left-margin">53.
</span><span class="csl-right-inline">Chen PW, Jansen EC, Cielo CM,
Williamson AA, Banker M, Kaye M, Song PXK, Peterson KE, Cantoral A,
Téllez-Rojo MM, Goldstein C, Zanna K, Sano A, Moreno JP, Kalkwarf H,
Zemel BS, Mitchell JA. [Performance of an automated sleep scoring
approach for actigraphy data in children and
adolescents](https://doi.org/10.1093/sleep/zsaf282). SLEEP. 2026
Feb;49(2):zsaf282. </span>

<span class="csl-left-margin">54.
</span><span class="csl-right-inline">Rowlands AV, Plekhanova T, Yates
T, Mirkes EM, Davies M, Khunti K, Edwardson CL. [Providing a Basis for
Harmonization of Accelerometer-Assessed Physical Activity Outcomes
Across Epidemiological
Datasets](https://doi.org/10.1123/jmpb.2018-0073). Journal for the
Measurement of Physical Behaviour. 2019 Sept;2(3):131–42. </span>

<span class="csl-left-margin">55.
</span><span class="csl-right-inline">Van Hees VT, Gorzelniak L, Dean
León EC, Eder M, Pias M, Taherian S, Ekelund U, Renström F, Franks PW,
Horsch A, Brage S. [Separating Movement and Gravity Components in an
Acceleration Signal and Implications for the Assessment of Human Daily
Physical Activity](https://doi.org/10.1371/journal.pone.0061691). Müller
M, editor. PLoS ONE. 2013 Apr;8(4):e61691. </span>

<span class="csl-left-margin">56.
</span><span class="csl-right-inline">Rowlands AV. [Moving Forward With
Accelerometer-Assessed Physical Activity: Two Strategies to Ensure
Meaningful, Interpretable, and Comparable
Measures](https://doi.org/10.1123/pes.2018-0201). Pediatric Exercise
Science. 2018 Nov;30(4):450–6. </span>

<span class="csl-left-margin">57.
</span><span class="csl-right-inline">Rowlands AV, Dawkins NP, Maylor B,
Edwardson CL, Fairclough SJ, Davies MJ, Harrington DM, Khunti K, Yates
T. [Enhancing the value of accelerometer-assessed physical activity:
Meaningful visual comparisons of data-driven translational accelerometer
metrics](https://doi.org/10.1186/s40798-019-0225-9). Sports Medicine -
Open. 2019 Dec;5(1). </span>

<span class="csl-left-margin">58.
</span><span class="csl-right-inline">Rowlands AV, Sherar LB, Fairclough
SJ, Yates T, Edwardson CL, Harrington DM, Davies MJ, Munir F, Khunti K,
Stiles VH. [A data-driven, meaningful, easy to interpret, standardised
accelerometer outcome variable for global
surveillance](https://doi.org/10.1016/j.jsams.2019.06.016). Journal of
Science and Medicine in Sport. 2019 Oct;22(10):1132–8. </span>

<span class="csl-left-margin">59.
</span><span class="csl-right-inline">Gába A, Dygrýn J, Štefelová N,
Rubín L, Hron K, Jakubec L, Pedišić Ž. [How do short sleepers use extra
waking hours? A compositional analysis of 24-h time-use patterns among
children and adolescents](https://doi.org/10.1186/s12966-020-01004-8).
International Journal of Behavioral Nutrition and Physical Activity.
2020 Dec;17(1). </span>

<span class="csl-left-margin">60.
</span><span class="csl-right-inline">Weston SJ, Ritchie SJ, Rohrer JM,
Przybylski AK. [Recommendations for Increasing the Transparency of
Analysis of Preexisting Data
Sets](https://doi.org/10.1177/2515245919848684). Advances in Methods and
Practices in Psychological Science. 2019 Sept;2(3):214–27. </span>

<span class="csl-left-margin">61.
</span><span class="csl-right-inline">MacCoun R, Perlmutter S. [Blind
analysis: Hide results to seek the
truth](https://doi.org/10.1038/526187a). Nature. 2015
Oct;526(7572):187–9. </span>

<span class="csl-left-margin">62.
</span><span class="csl-right-inline">Buuren SV, Groothuis-Oudshoorn K.
[**Mice** : Multivariate Imputation by Chained Equations in
*R*](https://doi.org/10.18637/jss.v045.i03). Journal of Statistical
Software. 2011;45(3). </span>

<span class="csl-left-margin">63.
</span><span class="csl-right-inline">Curran PJ, Hussong AM.
[Integrative data analysis: The simultaneous analysis of multiple data
sets.](https://doi.org/10.1037/a0015914) Psychological Methods.
2009;14(2):81–100. </span>

<span class="csl-left-margin">64.
</span><span class="csl-right-inline">Leota J, Presby DM, Le F, Czeisler
MÉ, Mascaro L, Capodilupo ER, Wiley JF, Drummond SPA, Rajaratnam SMW,
Facer-Childs ER. [Dose-response relationship between evening exercise
and sleep](https://doi.org/10.1038/s41467-025-58271-x). Nature
Communications. 2025 Apr;16(1):3297. </span>

<span class="csl-left-margin">65.
</span><span class="csl-right-inline">Souabni M, Del Sordo GC, Saurel M,
Peyrel P, Giacomoni M, Souabni MJ, Davenne D, Gabriel BM, St-Onge MP,
Saidi O. [Effects of morning versus evening exercise on sleep: A
systematic review of comparative studies with a meta-analysis of
randomized controlled
trials](https://doi.org/10.1016/j.smrv.2026.102358). Sleep Medicine
Reviews. 2026 Aug;90:102358. </span>

<span class="csl-left-margin">66.
</span><span class="csl-right-inline">Pham H, Svensson T, Chung U,
Svensson AK. [Bidirectional association between physical activity and
sleep in healthy Japanese super-seniors: The Japan Healthy Aging Study
(J-HAS)](https://doi.org/10.1038/s41514-025-00223-2). npj Aging. 2025
May;11(1):36. </span>

<span class="csl-left-margin">67.
</span><span class="csl-right-inline">Windred DP, Burns AC, Lane JM,
Saxena R, Rutter MK, Cain SW, Phillips AJK. [Sleep regularity is a
stronger predictor of mortality risk than sleep duration: A prospective
cohort study](https://doi.org/10.1093/sleep/zsad253). SLEEP. 2024
Jan;47(1):zsad253. </span>

<span class="csl-left-margin">68.
</span><span class="csl-right-inline">Chaput JP, Biswas RK, Ahmadi M,
Cistulli PA, Rajaratnam SMW, Bian W, St-Onge MP, Stamatakis E. [Sleep
regularity and major adverse cardiovascular events: A device-based
prospective study in 72 269 UK
adults](https://doi.org/10.1136/jech-2024-222795). Journal of
Epidemiology and Community Health. 2025 Apr;79(4):257–64. </span>

<span class="csl-left-margin">69.
</span><span class="csl-right-inline">Li DR, Li ZX, Li MH, Liu BQ, Fang
Q, Liu JC, Zheng WR, Gong TT, Gao SY, Wu QJ. [Regular sleep patterns,
not just duration, critical for mental health: Association of
accelerometer-derived sleep regularity with incident depression and
anxiety](https://doi.org/10.1017/S0033291725101281). Psychological
Medicine. 2025;55:e239. </span>

<span class="csl-left-margin">70.
</span><span class="csl-right-inline">Bian W, Ahmadi MN, Biswas RK,
Blodgett JM, Atkin AJ, Chan HW, Del Pozo Cruz B, Suorsa K, Bakker EA,
Pulsford RM, Mielke GI, Johansson PJ, Hettiarachchi P, Koemel NA,
Thijssen DHJ, Stenholm S, Mishra GD, Teixeira-Pinto A, Rangul V, Sherar
LB, Ekelund U, Hughes AD, Lee IM, Cistulli PA, Holtermann A, Koster A,
Hamer M, Stamatakis E, Savelberg H, De Galan B, Van De Kallen C,
Eijsvogels TMH. [Device-Measured Sleep Characteristics, Daily Step
Count, and Cardiometabolic Health Markers: Findings From the Prospective
Physical Activity, Sitting, and Sleep (ProPASS)
Consortium](https://doi.org/10.1161/CIRCOUTCOMES.124.011873).
Circulation: Cardiovascular Quality and Outcomes. 2025 Aug;18(8).
</span>

<span class="csl-left-margin">71.
</span><span class="csl-right-inline">Australian Government Department
of Health, Disability and Ageing. 24-hour movement guidelines for all
Australians \[Internet\]. Australian Government Department of Health,
Disability and Ageing; 2026 \[cited 2026 Oct 10\]. Available from:
<https://www.health.gov.au/topics/physical-activity/24-hour-movement-guidelines-for-all-australians></span>

<span class="csl-left-margin">72.
</span><span class="csl-right-inline">Zisou C, Maylor BD, Acquah A,
Hammami I, Reynolds AC, Maddison KJ, McVeigh JA, Eastwood P, Straker L,
Healy GN, Dunstan DW, Davies MJ, Yates T, Rowlands AV, Edwardson CL,
Smith-Byrne K, Hopewell JC, Doherty A. [Towards harmonised
accelerometer-derived physical activity: A comparability study across
wrist, thigh, and hip wear
locations](https://doi.org/10.1186/s12966-026-01975-0). International
Journal of Behavioral Nutrition and Physical Activity. 2026 Sept;
</span>

<span class="csl-left-margin">73.
</span><span class="csl-right-inline">Lok R, Suh S, Rue S, Weed L,
Zeitzer JM. [How many days are enough? Sleep–wake timing regularity and
fragmentation scores change with the number of days
included](https://doi.org/10.1111/jsr.14332). Journal of Sleep Research.
2024 Sept;e14332. </span>

<span class="csl-left-margin">74.
</span><span class="csl-right-inline">Leota J, Messman BA, Le F,
Jasinski S, Capodilupo ER, Facer-Childs ER, Wiley JF. [How many nights
are needed? The short-term stability of intraindividual variability in
sleep parameters derived from accelerometry data in a cohort of normal
sleepers](https://doi.org/10.1093/sleep/zsag040). SLEEP. 2026
June;49(6):zsag040. </span>

<span class="csl-left-margin">75.
</span><span class="csl-right-inline">Czeisler MÉ, Leota J, Le F,
Campbell-Brown B, Rajaratnam SMW, Pase MP, Kramer DB. [Comparison of
Sleep Regularity Index scores calculated by open-source packages and
implications for outcomes research: Rationale and design of the RIRI
statement (Reporting Items for Regularity
Indices)](https://doi.org/10.1093/sleep/zsaf299). SLEEP. 2026
Apr;49(4):zsaf299. </span>
