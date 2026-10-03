# Introduction

Sleep is an important determinant of population health across the
lifespan\[1\]. The consequences of poor sleep include increased
all-cause mortality \[2\], cardiovascular disease \[3\], cancers \[4\],
and poor mental health \[5\]. Poor sleep - characteristed by low sleep
quality, duration, or the presence of sleep disorders - is a frequent
health problem: 14-68% of children and adolescents \[6\] and 35% of
adults \[7\] report lower sleep duration than recommended \[8\].
Improving sleep should be a priority for public health policy, and more
safe and accessible strategies that promote sleep are needed \[9\]. In
adults, being physically active appears to improve both the quantity and
quality of sleep \[10\]. The World Health Organization recommends
physical activity as a method to improve sleep in adults
\[**bullWorldHealthOrganisation2020?**\]. In children, adolescents, and
older adults associations are unclear \[9,11,12\] and guidelines do not
currently support a role for physical activity to improve sleep.

Promoting physical activity may be an effective method to improve sleep
across the lifespan, but the published literature has at least two
limitations. First, most studies have relied on self-report measures of
sleep and physical activity \[9,11,12\], which could confound
associations and limit the precision of analyses designed to identify a
dose-response relationship between physical activity and sleep. Second,
no study has collected day-to-day data from a large and geographically
diversesample that included participants across the lifespan. Umbrella
reviews \[9\] and meta-analyses \[3,4,10–28\] have provided some
suggestive evidence about how physical activity may improve sleep across
the lifespan. However, their reliance on aggregated study-level effects
limits the quality and scope of data that can be used to analyse the
influence of age \[29\]. Device-measured individual participant data
analysis can overcome these limitations \[30\].

In this study we harmonised individual participant data from
accelerometer observations obtained from studies around the world. The
aim of this pooled study was to investigate the relationship between
device-measured physical activity and sleep across the lifespan. We
examined whether individuals who are more physically active have better
sleep, whether age, sex, and socio-cultural factors moderate the
association between physical activity and sleep, and whether there is a
bidirectional association between physical activity and sleep.

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
\[10–12\]. We then used forward searching on potentially eligible
studies \[31\]. We additionally performed a literature search to
identify accelerometry consortiums and individual studies, searched
trial registries (ANZCTR, ICTRP, ISRCTN) and the Open Science Framework
for unpublished results, and asked collaborators to help identify any
additional studies.

We contacted chief investigators or consortium leads by email to request
access to deidentified individual data. To make the data acquisition
manageable, we only invited studies with sample sizes larger than 400
participants. If required by the contributing research group, the
contributing investigators or their university signed a data transfer
agreement. All studies included in the pooled data meta-analysis had to
have been approved by their local or institutional ethical committees,
and all participants had to have provided written informed consent. The
lead university’s Research Ethics Committee granted permission for each
study’s data to be obtained, and investigators transferred their
deidentified data via a secure file-sharing system.

## Data harmonisation and outcomes

All accelerometer-derived data were harmonised by reprocessing raw files
using an open-source GGIR package in R to produce physical activity and
sleep outcomes \[32,33\]. By reprocessing all raw accelerometer data, we
were able to generate outcomes that could be compared across studies,
device wear locations, and populations \[34\]. Data were reprocessed by
the core research team or the data contributors themselves using the
GGIR package processing script provided by the core research team.
Reprocessed individual study datasets were then pooled into a single
dataset. A detailed description of the reprocessing method and decisions
for specific accelerometer devices is available in an online
supplementary file.

For sleep, GGIR produced the following metrics: total sleep time
(minutes), sleep efficiency after onset (percentage of time asleep
within the sleep period), sleep regularity (regularity of sleep and
awake patterns on a day-to-day basis calculated as the sleep regularity
index), and timing (time of sleep and awakening within the 24-hour time
cycle). For physical activity, we used volume and intensity metrics
proposed by Rowlands et al \[35\]. These measures are 1) average
acceleration (measure of physical activity volume) and 2) intensity
gradient (measure of intensity distribution where the natural logs of
time and intensity are used to produce a straight-line graph with the
slope of the graph being the intensity gradient). We used this approach
to model the entire continuum of physical activity intensity for our
primary analyses \[36\] and avoid collapsing physical activity data into
population-specific intensity categories, which occurs when intensity
cut-points are applied to accelerometer data \[37\].

To translate physical activity data for interpretation and public health
recommendations and for time-use based analyses we then determined; 1)
the number of minutes spent in acceleration intensity bands in 50*m**g*
increments \[35\], 2) the number of minutes spent in light and moderate
to vigorous physical activity using age appropriate cut-points applied
to the time in intensity bands data, and 3) the minimum acceleration
measured for a range of different durations, this is the *M**X* metric
where *X* refers to the duration. For example, *M*60 refers to the
minimum acceleration for the most active 60 minutes of the day \[36\].
We also categorise all awake time into bout lengths for sedentary,
light, and moderate to vigorous physical activity bouts using the
categories defined by Gába et al \[38\]. After extracting sleep and
physical activity, the remaining time within the 24-hour time cycle was
classified as sedentary behaviour. Non-wear time and abnormally high
acceleration values were detected using the data processing default
setting \[39\]. These data were classified as missing and imputed using
the accelerometer data from the same time interval on the remaining days
of the week.

## Covariates and moderators

The following individual participant data were obtained when available:
age, sex, height, weight, waist circumference, maturational status,
socioeconomic status (SES), ethnicity, screen time, sleep medications,
and medically diagnosed conditions known to affect sleep. The country,
city, time zone, daylight length, and season in which data were
collected were also obtained.

<!-- NEW PARAGRAPH, REVIEW -->

Body mass index was converted to an age- and sex-standardised z-score.
For participants under 18 years we used the CDC 2000 BMI-for-age growth
reference, applied with the *childsds* package. No age-based growth
reference applies from 18 years, so for adults we standardised body mass
index against the adults in this sample. The two halves are consequently
not the same quantity: below 18 the z-score is expressed relative to an
external growth reference, and at 18 and above relative to this sample.
We use the z-score in place of raw body mass index throughout, both as a
covariate and as a moderator, so that a given value carries a comparable
meaning at every age.

## Statistical analysis

To reduce the risk of bias in secondary data analysis of IPD, we used a
blind analysis approach \[40\]. We developed and tested models on a 10%
holdout subsample of the dataset. When the analysis code was ready, we
conducted the final analysis \[41\].

<!-- REVIEW THIS PARAGRAPH ===== -->

We screened all data for implausibly low or high measurements, as these
likely indicate measurement device failures. Screening had three stages.
First, we excluded values falling outside fixed physiological limits;
for average acceleration this was 200 mg, a value above any sustainable
24-hour average. Second, because a miscalibrated accelerometer corrupts
every variable derived from acceleration magnitude rather than isolated
days, we excluded all physical activity variables for files whose mean
average acceleration exceeded that limit (76 participants). Third, we
excluded remaining values falling outside ±4 SD of the mean **of their
own study**, rather than of the pooled sample, so that the threshold
reflected implausible measurement rather than between-study differences
in device, protocol and population. The fixed limits were applied before
the ±4 SD screen because the mean and standard deviation are themselves
distorted by the values they are used to detect. Values excluded at any
stage were treated as missing and imputed alongside genuinely missing
data.

<!-- ===== NEW PARAGRAPH, REVIEW ===== -->

A day contributed to the analysis if it recorded more than 10 hours of
wear time and, where sleep was detected, a sleep period longer than 200
minutes. Participants were additionally required to have at least 4 days
of valid wear time. 345 participants, contributing 465 person-days, did
not meet this criterion, leaving 24,264 participants and 148,749
person-days for analysis. Because the criterion is applied after
imputation, the primary analysis and a sensitivity analysis that retains
every eligible participant regardless of the number of valid wear days
share the same imputations and the same standardisation constants, and
are therefore directly comparable; the sensitivity analysis is reported
in the supplementary materials.

We assumed missing covariate and moderator data to be missing at random
or completely at random and imputed these missing values using multiple
imputations by chained equations with the *mice* package in R \[42\] to
provide 3 imputed datasets. Specifically, we used multilevel predictive
mean matching imputation clustered by participants. We used Rubin’s rule
to pool the results from analyses conducted using each imputed dataset
into one single set of results. We examined 95% confidence intervals for
all analyses and used P&lt;0.05 to assess for statistical significance.
<!-- =====  NEW SENTENCES, REVIEW  --> Both the intervals and the
p-values are computed on the pooled Barnard-Rubin degrees of freedom,
which account for the additional uncertainty introduced by imputation.
We fitted 16 models for each moderator and report all of them without
adjustment for multiple comparisons, so p-values close to 0.05 should be
interpreted with corresponding caution. We used R (version 4.5.3) for
all data cleaning and analysis. We report mean and 95% confidence
intervals. For categorical variables, percentages are reported. We
examined model diagnostics from all models (see supplementary
materials). <!-- NEW SENTENCES, REVIEW  --> Convergence and singularity
are reported separately. A singular fit is one in which the optimiser
succeeded but placed a variance component at zero, which indicates that
the data do not support that random effect rather than that estimation
failed.

To examine the relationship between sleep and physical activity
(Research Questions 1-2) <!-- REVIEW  --> we ran mixed-effects models
with random intercepts for study ID and for participant, accounting for
the nesting of repeated days within participants and of participants
within studies \[43\].

<!--REVIEW -->

We predicted each sleep indicator by each physical activity indicator
controlling for socioeconomic status, sex, and body mass index z-score.
Quadratic terms were introduced in each model to allow for curvilinear
relationships to be modelled. We interacted each predictor with age to
reveal how the relationship between physical activity and sleep changes
over the lifespan.

<!-- NEW PARAGRAPH, REVIEW  -->

Models predicting physical activity from the preceding night’s sleep
(Research Question 3) use each sleep indicator lagged by one day,
constructed within participants after eligibility screening so that the
lagged value is always an eligible measurement from the immediately
preceding calendar day. Person-days whose preceding day was absent from
the data or did not meet the eligibility criteria therefore have no
lagged value and do not contribute to these models, leaving 120,660 of
149,214 eligible person-days.

## Protocol deviations

This paper addresses Hypotheses 1 and 2 from our registered protocol. We
will address the remaining hypotheses in future research. To streamline
our analysis and reduce redundancy, we made some changes to our protocol
while working with the holdout data.

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

The aggregated dataset describes 220,760 observations of daily physical
activity and sleep from 28,823 unique participants. Of these, 71,546
days were excluded as they had insufficient wear time, which also
resulted in the exclusion of 4,214 participants.
<!-- NEW SENTENCE, REVIEW  --> A further 345 participants (465 days)
were excluded for having fewer than 4 days of valid wear time, leaving
24,264 participants and 148,749 person-days. Table
@ref(tab:demo-participants) shows demographic information for the
included participants. The majority of participants came from Europe,
Oceania, and South America, with limited data from other regions. There
was a near-even split by sex (50.4% female), but age skewed towards both
younger children (2-11 years, 44.9%) and older adults (66+ years,
25.1%).

Valid observations were not uniform across the days of the week
(*χ*<sub>(6)</sub><sup>2</sup> = 310.72, p = &lt; .001). Weekends were
over-represented (Saturday: z = 6.08; Sunday: z = 11.62), while
Wednesdays (z = -9.91) and Thursdays (z = -8.85) were under-represented.
A table of study characteristics can be found in supplementary
materials.

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
<td style="text-align: left;">10,857</td>
<td style="text-align: left;">1,921</td>
<td style="text-align: left;">308</td>
<td style="text-align: left;">5,040</td>
<td style="text-align: left;">6,066</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">9.73 (1.49)</td>
<td style="text-align: left;">15.74 (1.72)</td>
<td style="text-align: left;">29.16 (5.07)</td>
<td style="text-align: left;">57.68 (7.37)</td>
<td style="text-align: left;">72.72 (4.52)</td>
</tr>
<tr>
<td style="text-align: left;">   BMI</td>
<td style="text-align: left;">17.99 (3.19)</td>
<td style="text-align: left;">21.61 (3.76)</td>
<td style="text-align: left;">26.46 (6.42)</td>
<td style="text-align: left;">26.80 (4.79)</td>
<td style="text-align: left;">27.08 (4.31)</td>
</tr>
<tr>
<td style="text-align: left;">   BMI z-score</td>
<td style="text-align: left;">0.25 (1.07)</td>
<td style="text-align: left;">0.09 (1.02)</td>
<td style="text-align: left;">-0.09 (1.38)</td>
<td style="text-align: left;">-0.02 (1.03)</td>
<td style="text-align: left;">0.05 (0.93)</td>
</tr>
<tr>
<td style="text-align: left;">   Valid Weartime Hours</td>
<td style="text-align: left;">23.13 (1.60)</td>
<td style="text-align: left;">23.23 (1.54)</td>
<td style="text-align: left;">23.25 (1.61)</td>
<td style="text-align: left;">23.74 (0.82)</td>
<td style="text-align: left;">23.89 (0.40)</td>
</tr>
<tr>
<td style="text-align: left;">   Valid Weatime Days</td>
<td style="text-align: left;">5.62 (1.89)</td>
<td style="text-align: left;">4.97 (1.34)</td>
<td style="text-align: left;">4.79 (1.31)</td>
<td style="text-align: left;">7.34 (2.48)</td>
<td style="text-align: left;">6.48 (2.09)</td>
</tr>
<tr>
<td style="text-align: left;">   PA Intensity</td>
<td style="text-align: left;">-2.13 (0.19)</td>
<td style="text-align: left;">-2.23 (0.19)</td>
<td style="text-align: left;">-2.43 (0.19)</td>
<td style="text-align: left;">-2.50 (0.19)</td>
<td style="text-align: left;">-2.68 (0.22)</td>
</tr>
<tr>
<td style="text-align: left;">   PA Volume</td>
<td style="text-align: left;">42.53 (24.27)</td>
<td style="text-align: left;">46.73 (14.30)</td>
<td style="text-align: left;">39.53 (10.69)</td>
<td style="text-align: left;">38.82 (11.60)</td>
<td style="text-align: left;">30.86 (9.71)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Duration (min)</td>
<td style="text-align: left;">486.90 (52.97)</td>
<td style="text-align: left;">416.72 (55.29)</td>
<td style="text-align: left;">421.10 (62.52)</td>
<td style="text-align: left;">408.42 (55.81)</td>
<td style="text-align: left;">404.16 (63.32)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Efficiency (%)</td>
<td style="text-align: left;">80.75 (7.40)</td>
<td style="text-align: left;">87.13 (6.30)</td>
<td style="text-align: left;">87.28 (6.12)</td>
<td style="text-align: left;">87.79 (5.52)</td>
<td style="text-align: left;">86.50 (6.03)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Onset (clock time; SD in
h:mm)</td>
<td style="text-align: left;">20:55 (01:08)</td>
<td style="text-align: left;">23:30 (01:26)</td>
<td style="text-align: left;">23:40 (01:46)</td>
<td style="text-align: left;">23:41 (01:23)</td>
<td style="text-align: left;">00:23 (01:14)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Regularity</td>
<td style="text-align: left;">54.00 (12.73)</td>
<td style="text-align: left;">57.03 (12.15)</td>
<td style="text-align: left;">55.79 (12.76)</td>
<td style="text-align: left;">59.45 (11.35)</td>
<td style="text-align: left;">54.53 (12.49)</td>
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
<td style="text-align: left;">2,567 (23.64%)</td>
<td style="text-align: left;">1,883 (98.02%)</td>
<td style="text-align: left;">308 (100.00%)</td>
<td style="text-align: left;">5,040 (100.00%)</td>
<td style="text-align: left;">6,066 (100.00%)</td>
</tr>
<tr>
<td style="text-align: left;">   Hip</td>
<td style="text-align: left;">8,290 (76.36%)</td>
<td style="text-align: left;">   38 (1.98%)</td>
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
<td style="text-align: left;">1,505 (13.86%)</td>
<td style="text-align: left;">303 (15.77%)</td>
<td style="text-align: left;">108 (35.06%)</td>
<td style="text-align: left;">  186 (3.69%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Finland</td>
<td style="text-align: left;">  508 (4.68%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">  801 (15.89%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   New Zealand</td>
<td style="text-align: left;">  455 (4.19%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">37 (12.01%)</td>
<td style="text-align: left;">  151 (3.00%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Spain</td>
<td style="text-align: left;">  934 (8.60%)</td>
<td style="text-align: left;">  5 (0.26%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">   37 (0.73%)</td>
<td style="text-align: left;">2,386 (39.33%)</td>
</tr>
<tr>
<td style="text-align: left;">   Switzerland</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">1,904 (37.78%)</td>
<td style="text-align: left;">1,023 (16.86%)</td>
</tr>
<tr>
<td style="text-align: left;">   United Kingdom</td>
<td style="text-align: left;">  447 (4.12%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">144 (46.75%)</td>
<td style="text-align: left;">1,961 (38.91%)</td>
<td style="text-align: left;">2,657 (43.80%)</td>
</tr>
<tr>
<td style="text-align: left;">   Brazil</td>
<td style="text-align: left;">  486 (4.48%)</td>
<td style="text-align: left;">727 (37.84%)</td>
<td style="text-align: left;">10 (3.25%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Czech Republic</td>
<td style="text-align: left;">  281 (2.59%)</td>
<td style="text-align: left;">848 (44.14%)</td>
<td style="text-align: left;">  9 (2.92%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Canada</td>
<td style="text-align: left;">  522 (4.81%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   China</td>
<td style="text-align: left;">  124 (1.14%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Colombia</td>
<td style="text-align: left;">  889 (8.19%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Denmark</td>
<td style="text-align: left;">  689 (6.35%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Ecuador</td>
<td style="text-align: left;">  981 (9.04%)</td>
<td style="text-align: left;">38 (1.98%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   India</td>
<td style="text-align: left;">  574 (5.29%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Kenya</td>
<td style="text-align: left;">  525 (4.84%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Portugal</td>
<td style="text-align: left;">  702 (6.47%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   South Africa</td>
<td style="text-align: left;">  506 (4.66%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   United States</td>
<td style="text-align: left;">  729 (6.71%)</td>
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
<td style="text-align: left;">2,127 (19.59%)</td>
<td style="text-align: left;">  169 (8.80%)</td>
<td style="text-align: left;">14 (4.55%)</td>
<td style="text-align: left;">   94 (1.87%)</td>
<td style="text-align: left;">  240 (3.96%)</td>
</tr>
<tr>
<td style="text-align: left;">   Unclear</td>
<td style="text-align: left;">4,433 (40.83%)</td>
<td style="text-align: left;">1,192 (62.05%)</td>
<td style="text-align: left;">152 (49.35%)</td>
<td style="text-align: left;">1,337 (26.53%)</td>
<td style="text-align: left;">2,424 (39.96%)</td>
</tr>
<tr>
<td style="text-align: left;">   White</td>
<td style="text-align: left;">4,297 (39.58%)</td>
<td style="text-align: left;">  560 (29.15%)</td>
<td style="text-align: left;">142 (46.10%)</td>
<td style="text-align: left;">3,609 (71.61%)</td>
<td style="text-align: left;">3,402 (56.08%)</td>
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
<td style="text-align: left;">3,561 (32.80%)</td>
<td style="text-align: left;">853 (44.40%)</td>
<td style="text-align: left;">153 (49.68%)</td>
<td style="text-align: left;">4,703 (93.31%)</td>
<td style="text-align: left;">6,066 (100.00%)</td>
</tr>
<tr>
<td style="text-align: left;">   Oceania</td>
<td style="text-align: left;">1,960 (18.05%)</td>
<td style="text-align: left;">303 (15.77%)</td>
<td style="text-align: left;">145 (47.08%)</td>
<td style="text-align: left;">  337 (6.69%)</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   South America</td>
<td style="text-align: left;">2,356 (21.70%)</td>
<td style="text-align: left;">765 (39.82%)</td>
<td style="text-align: left;">10 (3.25%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Africa</td>
<td style="text-align: left;">1,031 (9.50%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Asia</td>
<td style="text-align: left;">  698 (6.43%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   North America</td>
<td style="text-align: left;">1,251 (11.52%)</td>
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
<td style="text-align: left;">3,387 (31.20%)</td>
<td style="text-align: left;">  446 (23.22%)</td>
<td style="text-align: left;">39 (12.66%)</td>
<td style="text-align: left;">1,241 (24.62%)</td>
<td style="text-align: left;">1,268 (20.90%)</td>
</tr>
<tr>
<td style="text-align: left;">   Spring</td>
<td style="text-align: left;">2,687 (24.75%)</td>
<td style="text-align: left;">1,228 (63.93%)</td>
<td style="text-align: left;">124 (40.26%)</td>
<td style="text-align: left;">1,224 (24.29%)</td>
<td style="text-align: left;">1,989 (32.79%)</td>
</tr>
<tr>
<td style="text-align: left;">   Summer</td>
<td style="text-align: left;">1,381 (12.72%)</td>
<td style="text-align: left;">   85 (4.42%)</td>
<td style="text-align: left;">85 (27.60%)</td>
<td style="text-align: left;">1,270 (25.20%)</td>
<td style="text-align: left;">1,250 (20.61%)</td>
</tr>
<tr>
<td style="text-align: left;">   Winter</td>
<td style="text-align: left;">3,402 (31.33%)</td>
<td style="text-align: left;">  162 (8.43%)</td>
<td style="text-align: left;">60 (19.48%)</td>
<td style="text-align: left;">1,305 (25.89%)</td>
<td style="text-align: left;">1,559 (25.70%)</td>
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
<td style="text-align: left;">5,693 (52.45%)</td>
<td style="text-align: left;">1,002 (52.16%)</td>
<td style="text-align: left;">222 (72.08%)</td>
<td style="text-align: left;">2,714 (53.85%)</td>
<td style="text-align: left;">2,551 (42.05%)</td>
</tr>
<tr>
<td style="text-align: left;">   Male</td>
<td style="text-align: left;">5,162 (47.55%)</td>
<td style="text-align: left;">  919 (47.84%)</td>
<td style="text-align: left;">86 (27.92%)</td>
<td style="text-align: left;">2,326 (46.15%)</td>
<td style="text-align: left;">3,515 (57.95%)</td>
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
<td style="text-align: left;">   625 (5.76%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">3,511 (69.66%)</td>
<td style="text-align: left;">5,140 (84.73%)</td>
</tr>
<tr>
<td style="text-align: left;">   Yes</td>
<td style="text-align: left;">    50 (0.46%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">  310 (6.15%)</td>
<td style="text-align: left;">  626 (10.32%)</td>
</tr>
<tr>
<td style="text-align: left;">   Not Reported</td>
<td style="text-align: left;">10,182 (93.78%)</td>
<td style="text-align: left;">1,921 (100.00%)</td>
<td style="text-align: left;">308 (100.00%)</td>
<td style="text-align: left;">1,219 (24.19%)</td>
<td style="text-align: left;">  300 (4.95%)</td>
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
<td style="text-align: left;">2,306 (25.22%)</td>
<td style="text-align: left;">208 (11.56%)</td>
<td style="text-align: left;">  2 (0.74%)</td>
<td style="text-align: left;">1,074 (21.98%)</td>
<td style="text-align: left;">2,388 (39.38%)</td>
</tr>
<tr>
<td style="text-align: left;">   Medium</td>
<td style="text-align: left;">2,920 (31.94%)</td>
<td style="text-align: left;">884 (49.14%)</td>
<td style="text-align: left;">191 (70.74%)</td>
<td style="text-align: left;">2,407 (49.26%)</td>
<td style="text-align: left;">2,324 (38.32%)</td>
</tr>
<tr>
<td style="text-align: left;">   High</td>
<td style="text-align: left;">3,916 (42.84%)</td>
<td style="text-align: left;">707 (39.30%)</td>
<td style="text-align: left;">77 (28.52%)</td>
<td style="text-align: left;">1,405 (28.76%)</td>
<td style="text-align: left;">1,352 (22.30%)</td>
</tr>
</tbody>
</table>

*Note.* For categorical variables the value is the count, and
percentage. For numeric variables the value is the Mean and SD. Sleep
onset is a clock time; its SD is a duration in hours:minutes. Total N =
24,264. 72 participants had missing age data and are counted in the
total but not in any age band.

 

## The effects of physical activity volume on sleep

We estimated the effects of physical activity on sleep (RQ1) using
mixed-effects models. The effect of physical activity volume on sleep by
age are presented in Table @ref(tab:sleep-outcomes) and Figure
@ref(fig:sleep-by-volume-fig). Higher physical activity volume was
associated with longer sleep duration, higher sleep efficiency, earlier
sleep onset, and more regular sleep. While we observed statistically
significant curvilinear relationships, these did not appear to
meaningfully change the effect. The relationship between physical
activity volume and sleep duration, efficiency, and onset was consistent
across the age groups, with some negligible evidence that the
relationship with sleep regularity grew stronger with age.

<table>
<caption>(#tab:sleep-outcomes) Physical activity predicting sleep
controlling for SES, sex, and BMI z-score.</caption>
<thead>
<tr>
<th style="text-align: left;">Term</th>
<th style="text-align: left;"><span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">SE</th>
<th style="text-align: left;">t</th>
<th style="text-align: left;">p</th>
<th style="text-align: left;"><span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">SE</th>
<th style="text-align: left;">t</th>
<th style="text-align: left;">p</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Sleep duration (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.08 [-0.09, 0.25]</td>
<td style="text-align: left;">0.08</td>
<td style="text-align: left;">0.95</td>
<td style="text-align: left;">.342</td>
<td style="text-align: left;">0.08 [-0.08, 0.24]</td>
<td style="text-align: left;">0.08</td>
<td style="text-align: left;">1.00</td>
<td style="text-align: left;">.315</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.10 [0.09, 0.12]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">13.00</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.09 [0.06, 0.12]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">7.59</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.34</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.42</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.64</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.01 [0.00, 0.02]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">1.59</td>
<td style="text-align: left;">.130</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-4.25</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-5.32</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.57</td>
<td style="text-align: left;">.135</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.41</td>
<td style="text-align: left;">.003</td>
</tr>
<tr>
<td style="text-align: left;">Sleep efficiency (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.16 [0.00, 0.32]</td>
<td style="text-align: left;">0.08</td>
<td style="text-align: left;">2.00</td>
<td style="text-align: left;">.046</td>
<td style="text-align: left;">0.19 [0.01, 0.36]</td>
<td style="text-align: left;">0.09</td>
<td style="text-align: left;">2.11</td>
<td style="text-align: left;">.035</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.11 [0.09, 0.13]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">13.39</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.07 [0.05, 0.10]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">7.52</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.61</td>
<td style="text-align: left;">.545</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.53</td>
<td style="text-align: left;">.127</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-12.56</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.04 [-0.05, -0.03]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-7.47</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-11.89</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-6.78</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">2.59</td>
<td style="text-align: left;">.010</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">6.40</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">Sleep onset (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.10 [-0.18, 0.37]</td>
<td style="text-align: left;">0.14</td>
<td style="text-align: left;">0.71</td>
<td style="text-align: left;">.479</td>
<td style="text-align: left;">0.10 [-0.17, 0.36]</td>
<td style="text-align: left;">0.13</td>
<td style="text-align: left;">0.72</td>
<td style="text-align: left;">.472</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">-0.02 [-0.03, -0.01]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-4.22</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.06 [-0.08, -0.05]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-8.98</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.61</td>
<td style="text-align: left;">.111</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.69</td>
<td style="text-align: left;">.493</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.01 [-0.01, -0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-7.33</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.04, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-7.50</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-15.31</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.85</td>
<td style="text-align: left;">.068</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">9.23</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">10.73</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">Sleep regularity (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">0.15 [-0.01, 0.32]</td>
<td style="text-align: left;">0.08</td>
<td style="text-align: left;">1.95</td>
<td style="text-align: left;">.067</td>
<td style="text-align: left;">0.31 [0.13, 0.49]</td>
<td style="text-align: left;">0.09</td>
<td style="text-align: left;">3.40</td>
<td style="text-align: left;">.001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.25 [0.21, 0.29]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">17.91</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.29 [0.26, 0.32]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">22.24</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [-0.01, 0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.14</td>
<td style="text-align: left;">.900</td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.62</td>
<td style="text-align: left;">.036</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.03, -0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-6.60</td>
<td style="text-align: left;">.002</td>
<td style="text-align: left;">-0.11 [-0.12, -0.10]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-19.48</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.49</td>
<td style="text-align: left;">.211</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-19.73</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-15.25</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">9.81</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
</tbody>
</table>

*Note.* Adjusted for SES, sex, and BMI z-score. Outcomes variables are
listed in the column headers.

 

<img src="../Figures/main/Sleep on scale_pa_volume by Age_nolog.jpg" alt="Sleep metrics predicted by physical activity volume. The panels on the left show the curvilinear relationship between PA volume and each of the sleep outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero. These plots demonstrate exact turning points where the effects of PA volume change by age." width="110%" />
<p class="caption">
Sleep metrics predicted by physical activity volume. The panels on the
left show the curvilinear relationship between PA volume and each of the
sleep outcomes at the ages indicated in each column. The panels on the
right show the same relationships but by age continuously. The white
band indicates predictions which were not significantly different from
zero. These plots demonstrate exact turning points where the effects of
PA volume change by age.
</p>

## The effects of physical activity intensity on sleep

We estimated how physical activity intensity affects sleep across
different age groups and present the results controlling for sex, SES,
and BMI, in Table @ref(tab:sleep-outcomes) and Figure
@ref(fig:sleep-by-intensity-fig). We observed weak associations between
physical activity intensity and sleep duration, sleep efficiency, and
sleep onset, with a stronger relationship observed with sleep
regularity. As with physical activity volume, we observed statistically
significant curvilinear relationships, but these were only meaningful
for the impact of physical activity intensity on sleep regularity, in
which very low levels of intensity were strongly associated with poor
sleep regularity and the benefits of increasing intensity diminished
past one standard deviation above the mean. While age statistically
significantly modified the relationship between physical activity
intensity and sleep duration, efficiency, and regularity, the effect was
negligible.

<img src="../Figures/main/Sleep on scale_pa_intensity by Age_nolog.jpg" alt="Sleep metrics predicted by physical activity intensity. The panels on the left show the curvilinear relationship between PA intensity and each of the sleep outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Sleep metrics predicted by physical activity intensity. The panels on
the left show the curvilinear relationship between PA intensity and each
of the sleep outcomes at the ages indicated in each column. The panels
on the right show the same relationships but by age continuously. The
white band indicates predictions which were not significantly different
from zero.
</p>

## The effects of sleep duration on physical activity

We estimated the effect of sleep duration on physical activity by age.
Results, controlling for sex, SES, and BMI are presented in Table
@ref(tab:pa-outcomes) and Figure @ref(fig:PA-by-sleep-duration-fig). As
age increases, both physical activity volume and intensity decrease. We
found no evidence for an association between average sleep duration and
physical activity volume or intensity.

<table>
<caption>(#tab:pa-outcomes) Sleep predicting physical activity
controlling for SES, sex, and BMI z-score</caption>
<thead>
<tr>
<th style="text-align: left;">Term</th>
<th style="text-align: left;"><span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">SE</th>
<th style="text-align: left;">t</th>
<th style="text-align: left;">p</th>
<th style="text-align: left;"><span
class="math inline"><em>β</em></span> [95% CI]</th>
<th style="text-align: left;">SE</th>
<th style="text-align: left;">t</th>
<th style="text-align: left;">p</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Sleep duration (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">1.18 [0.83, 1.52]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">6.63</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.01 [0.88, 1.14]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">15.09</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration</td>
<td style="text-align: left;">0.05 [0.04, 0.06]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">10.43</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.04 [0.03, 0.05]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">8.38</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-38.85</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-48.18</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-9.32</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.02 [-0.02, -0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-5.72</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-12.00</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.04</td>
<td style="text-align: left;">.002</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep duration<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">2.60</td>
<td style="text-align: left;">.010</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">2.33</td>
<td style="text-align: left;">.022</td>
</tr>
<tr>
<td style="text-align: left;">Sleep efficiency (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">1.16 [0.82, 1.51]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">6.59</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.00 [0.87, 1.13]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">15.02</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency</td>
<td style="text-align: left;">0.00 [-0.01, 0.01]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-0.13</td>
<td style="text-align: left;">.900</td>
<td style="text-align: left;">0.01 [0.00, 0.02]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">2.33</td>
<td style="text-align: left;">.023</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-38.47</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-47.92</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.01 [-0.02, -0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-6.73</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-4.49</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">2.19</td>
<td style="text-align: left;">.029</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.59</td>
<td style="text-align: left;">.114</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep efficiency<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">2.77</td>
<td style="text-align: left;">.006</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.03</td>
<td style="text-align: left;">.304</td>
</tr>
<tr>
<td style="text-align: left;">Sleep onset (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">1.18 [0.82, 1.54]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">6.42</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.00 [0.86, 1.13]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">14.47</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset</td>
<td style="text-align: left;">-0.14 [-0.15, -0.12]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-20.95</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.07 [-0.08, -0.06]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-11.53</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-39.55</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-48.40</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.03 [-0.04, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-7.92</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.67</td>
<td style="text-align: left;">.506</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">13.34</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">9.48</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep onset<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">4.11</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.03</td>
<td style="text-align: left;">.975</td>
</tr>
<tr>
<td style="text-align: left;">Sleep regularity (z)</td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;">   (Intercept)</td>
<td style="text-align: left;">1.11 [0.77, 1.44]</td>
<td style="text-align: left;">0.17</td>
<td style="text-align: left;">6.50</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.97 [0.85, 1.09]</td>
<td style="text-align: left;">0.06</td>
<td style="text-align: left;">15.33</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity</td>
<td style="text-align: left;">0.16 [0.15, 0.17]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">36.63</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.12 [0.11, 0.13]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">29.40</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.02 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-36.44</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-47.16</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.86</td>
<td style="text-align: left;">.070</td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.44</td>
<td style="text-align: left;">.659</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity <span
class="math inline">×</span> Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-19.43</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-17.15</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep regularity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.04</td>
<td style="text-align: left;">.316</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.16</td>
<td style="text-align: left;">.871</td>
</tr>
</tbody>
</table>

*Note.* Adjusted for SES, sex, and BMI z-score. Outcomes variables are
listed in the row headers.

 

<img src="../Figures/main/PA on scale_sleep_duration_lag by Age_nolog.jpg" alt="Physical activity predicted by previous night sleep duration. The panels on the left show the curvilinear relationship between sleep duration and each of the physical activity outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Physical activity predicted by previous night sleep duration. The panels
on the left show the curvilinear relationship between sleep duration and
each of the physical activity outcomes at the ages indicated in each
column. The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.
</p>

## The effects of sleep efficiency on physical activity

We estimated the effect of sleep efficiency on physical activity by age.
Results, controlling for sex, SES, and BMI are presented in Table
@ref(tab:pa-outcomes) and Figure @ref(fig:PA-by-sleep-efficiency-fig).
There was a negligible relationship between sleep efficiency and
physical activity volume and a weak linear association between sleep
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

## The effects of sleep onset on physical activity

We estimated the effect of sleep onset on physical activity by age.
Results, controlling for sex, SES, and BMI are presented in Table
@ref(tab:pa-outcomes) and Figure @ref(fig:PA-by-sleep-onset-fig). There
were weak curvilinear relationships where average sleep onset was linked
to the highest levels of physical activity volume, but not intensity.

<img src="../Figures/main/PA on scale_sleep_onset_lag by Age_nolog.jpg" alt="Physical activity predicted by previous night sleep onset. The panels on the left show the curvilinear relationship between sleep onset and each of the physical activity outcomes at the ages indicated in each column. The panels on the right show the same relationships but by age continuously. The white band indicates predictions which were not significantly different from zero." width="110%" />
<p class="caption">
Physical activity predicted by previous night sleep onset. The panels on
the left show the curvilinear relationship between sleep onset and each
of the physical activity outcomes at the ages indicated in each column.
The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.
</p>

## The effects of sleep regularity on physical activity

We estimated the effect of sleep regularity on physical activity by age.
Results, controlling for sex, SES, and BMI are presented in Table
@ref(tab:pa-outcomes) and Figure @ref(fig:PA-by-sleep-regularity-fig).
There was a positive linear relationship between sleep regularity and
both physical activity volume and intensity. These relationships were
slightly attenuated with age, such that for older adults the effect was
negligible.

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

In this study, we collated and harmonised device-measured physical
activity and sleep data from 20 studies around the world, covering
participants across the lifespan. We found that both the volume and
intensity of physical activity was associated with longer sleep
duration, higher sleep efficiency, earlier sleep onset, and more regular
sleep. These relationships were the strongest for sleep regularity,
although the the relationship between physical activity intensity
weakened with age.

When examining the reverse direction, we observed those who fell asleep
earlier and at a more regular time engaged in more total physical
activity, with an additional negligible benefit for those who slept more
efficiently. Similarly, those who slept at a more regular time and with
more efficiency engaged in more intense physical activity. These
findings were largely robust to the decisions made during analysis (see
supplementary materials for all analyses).

Our findings broadly align with meta-analyses of experimental research,
although the strengths of the relationships we observed were much weaker
\[10\]. For example, \[10\] found small (0.22 &lt; *d* &lt; 0.30) but
significant relationships between physical activity and sleep duration
and efficiency in adults using experimental studies for both acute and
regular physical activity. It is difficult to directly compare these
effect sizes, however, as the experimental studies directly manipulated
physical activity, whereas our study examined the relationship between
habitual physical activity and sleep. It may be that the routine nature
of individuals’ physical activity makes associations more difficult to
detect. Similarly, most of the previous studies which have examined
acute changes to physical activity were conducted in laboratory
settings. These studies, while useful for understanding the potential
mechanisms, are not ecologically valid and may not reflect the
relationship between physical activity and sleep in the real world.
Laboratory studies also tend to use more sensitive measures of sleep and
physical activity, which may generate less statistical noise and make
relationships easier to detect. It could also be that duration of sleep
is set more by parameters external to the individual, such as work
schedules or parenting duties, than by individual health behaviours.

We found the most evidence for relationships between sleep regularity
with physical activity volume and intensity. This relationship appeared
to be bi-directional, with sleep onset and regularity influencing
physical activity the following day, and physical activity influencing
sleep onset and regularity that night. This is consistent with the idea
that sleep and physical activity are mutually reinforcing behaviours
\[9\], or that they are habitual and linked to factors such as the value
placed on health. Sleep regularity is a stronger predictor of mortality
than sleep duration \[44\], and so the finding that physical activity is
associated with sleep regularity may be particularly important.

Contrary to expectations, we found little evidence to suggest that the
associations between physical activity and sleep differed by age. While
several of the analyses suggested statistically significant interactions
between age and physical activity, these were too small to be clinically
meaningful. This suggests that the benefits of physical activity and
sleep are consistent across the lifespan, which is inconsistent with
previous meta-analyses conducted on different ages. More specifically,
meta-analyses in adult populations have generally found associations
\[10\] while those conducted in children and adolescents are less clear
\[9,11,12\]. This would seem to suggest that the differences in findings
across these meta-analyses are due to differences beyond just the age of
the populations, such as the type of study designs used or the measures
of physical activity and sleep. It also demonstrates the importance of
using individual participant data meta-analyses to examine these types
of questions.

These findings have implications for public health recommendations. Our
results support encouraging physical activity for maintaining regular
sleep patterns across the lifespan. Sleep health may contribute to
overall better health outcomes \[2,3\], and so promoting physical
activity may be a useful strategy for improving population health.
Public health messages should, therefore, emphasise the role of physical
activity in promoting stable sleep schedules rather than framing
physical activity as a universal solution for improving sleep. We note,
however, that we did not examine the these relationships in individuals
with sleep disorders or other health conditions. Physical activity
interventions may still be a useful treatment for these conditions.
Promoting more regular sleep may also be a useful strategy for
increasing physical activity, although our results suggest a limited
benefit from targeting other aspects of sleep quality.

Our study has several strengths, including the use of device-based
measures, which reduce the bias associated with self-reported sleep and
activity data, and the harmonisation of individual data from multiple
studies, conducted in geographically diverse regions and among multiple
age groups, allowing for robust and generalisable findings. We also used
modern methods for processing and estimating physical activity,
including using the full 24 hours of data, and avoiding the use of
intensity cut-points. However, the study is not without limitations.
Despite our efforts to harmonise data, variability in accelerometer wear
locations and slight differences in data collection protocols across
studies may have introduced measurement inconsistencies. We also chose
to maximise the data available for analysis, at the expense of some
precision in the estimates. Specifically, we included participants with
four or more nights of valid sleep, while some recommend at least seven
nights to estimate metrics such as sleep regularity \[45\]. However, the
processing method we used specifically allows for comparisons to
day-pairs and this methodology has been used by others \[e.g., 44\].
Additionally, although we controlled for several potential confounders,
there may still be unmeasured factors affecting the observed
associations. <!-- NEW SENTENCES, REVIEW  --> Our imputation model
included study, but not the remaining moderators or their interactions
with physical activity and sleep. It is therefore uncongenial with the
analysis models, and moderator effects may be attenuated towards the
null as a result. Finally, while our study captured data from across the
lifespan, the limited representation of certain age groups, especially
adolescents and young adults, restricts the generalisability of our
conclusions to these populations. The limited research on those aged
18-35 is particularly concerning given the high prevalence of poor sleep
in this age group \[7\].

# Conclusion

Our findings contribute to the growing body of literature that
highlights the complex relationship between physical activity and sleep.
While physical activity appears to benefit sleep regularity, its effects
on other sleep metrics are less clear, particularly when considering age
differences. Future research should aim to explore mechanisms underlying
these associations and identify specific intervention strategies that
can effectively enhance both sleep and physical activity behaviours for
diverse populations.

# References

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Luyster FS, Strollo PJ, Zee PC,
Walsh JK. [Sleep: A Health
Imperative](https://doi.org/10.5665/sleep.1846). Sleep. 2012
June;35(6):727–34. </span>

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Huang BH, Duncan MJ, Cistulli PA,
Nassar N, Hamer M, Stamatakis E. [Sleep and physical activity in
relation to all-cause, cardiovascular disease and cancer mortality
risk](https://doi.org/10.1136/bjsports-2021-104046). British Journal of
Sports Medicine. 2022 July;56(13):718–24. </span>

<span class="csl-left-margin">3.
</span><span class="csl-right-inline">Kwok CS, Kontopantelis E,
Kuligowski G, Gray M, Muhyaldeen A, Gale CP, Peat GM, Cleator J,
Chew-Graham C, Loke YK, Mamas MA. [Self-Reported Sleep Duration and
Quality and Cardiovascular Disease and Mortality: A Dose-Response
Meta-Analysis](https://doi.org/10.1161/jaha.118.008552). Journal of the
American Heart Association. 2018 Aug;7(15). </span>

<span class="csl-left-margin">4.
</span><span class="csl-right-inline">Ma QQ, Yao Q, Lin L, Chen GC, Yu
JB. [Sleep duration and total cancer mortality: A meta-analysis of
prospective studies](https://doi.org/10.1016/j.sleep.2016.06.036). Sleep
Medicine. 2016 Nov;27–28:39–44. </span>

<span class="csl-left-margin">5.
</span><span class="csl-right-inline">Wakefield JRH, Bowe M, Kellezi B,
Butcher A, Groeger JA. [Longitudinal associations between family
identification, loneliness, depression, and sleep
quality](https://doi.org/10.1111/bjhp.12391). British Journal of Health
Psychology. 2020 Feb;25(1):1–16. </span>

<span class="csl-left-margin">6.
</span><span class="csl-right-inline">Gariepy G, Danna S, Gobiņa I,
Rasmussen M, Gaspar De Matos M, Tynjälä J, Janssen I, Kalman M,
Villeruša A, Husarova D, Brooks F, Elgar FJ, Klavina-Makrecka S,
Šmigelskas K, Gaspar T, Schnohr C. [How Are Adolescents Sleeping?
Adolescent Sleep Patterns and Sociodemographic Differences in 24
European and North American
Countries](https://doi.org/10.1016/j.jadohealth.2020.03.013). Journal of
Adolescent Health. 2020 June;66(6):S81–8. </span>

<span class="csl-left-margin">7.
</span><span class="csl-right-inline">Liu Y, Wheaton AG, Chapman DP,
Cunningham TJ, Lu H, Croft JB. [Prevalence of Healthy Sleep Duration
among Adults–United States,
2014](https://doi.org/10.15585/mmwr.mm6506a1). MMWR Morbidity and
mortality weekly report. 2016 Feb;65(6):137–41. </span>

<span class="csl-left-margin">8.
</span><span class="csl-right-inline">Hirshkowitz M, Whiton K, Albert
SM, Alessi C, Bruni O, DonCarlos L, Hazen N, Herman J, Adams Hillard PJ,
Katz ES, Kheirandish-Gozal L, Neubauer DN, O’Donnell AE, Ohayon M,
Peever J, Rawding R, Sachdeva RC, Setters B, Vitiello MV, Ware JC.
[National Sleep Foundation’s updated sleep duration recommendations:
Final report](https://doi.org/10.1016/j.sleh.2015.10.004). Sleep Health.
2015 Dec;1(4):233–43. </span>

<span class="csl-left-margin">9.
</span><span class="csl-right-inline">Kline CE, Hillman CH, Bloodgood
Sheppard B, Tennant B, Conroy DE, Macko RF, Marquez DX, Petruzzello SJ,
Powell KE, Erickson KI. [Physical activity and sleep: An updated
umbrella review of the 2018 Physical Activity Guidelines Advisory
Committee report](https://doi.org/10.1016/j.smrv.2021.101489). Sleep
Medicine Reviews. 2021 Aug;58:101489. </span>

<span class="csl-left-margin">10.
</span><span class="csl-right-inline">Kredlow MA, Capozzoli MC, Hearon
BA, Calkins AW, Otto MW. [The effects of physical activity on sleep: A
meta-analytic review](https://doi.org/10.1007/s10865-015-9617-6).
Journal of Behavioral Medicine. 2015 June;38(3):427–49. </span>

<span class="csl-left-margin">11.
</span><span class="csl-right-inline"><span class="nocase">Antczak D,
Lonsdale C, Lee J, Hilland T, Duncan MJ, del Pozo Cruz B, Hulteen RM,
Parker PD, Sanders T</span>. [Physical activity and sleep are
inconsistently related in healthy children: A systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2020.101278). Sleep
Medicine Reviews. 2020 June;51:101278. </span>

<span class="csl-left-margin">12.
</span><span class="csl-right-inline">Lang C, Kalak N, Brand S,
Holsboer-Trachsler E, Pühse U, Gerber M. [The relationship between
physical activity and sleep from mid adolescence to early adulthood. A
systematic review of methodological approaches and
meta-analysis](https://doi.org/10.1016/j.smrv.2015.07.004). Sleep
Medicine Reviews. 2016 Aug;28:32–45. </span>

<span class="csl-left-margin">13.
</span><span class="csl-right-inline">Aiello KD, Caughey WG, Nelluri B,
Sharma A, Mookadam F, Mookadam M. [Effect of exercise training on sleep
apnea: A systematic review and
meta-analysis](https://doi.org/10.1016/j.rmed.2016.05.015). Respiratory
Medicine. 2016;116:85–92. </span>

<span class="csl-left-margin">14.
</span><span class="csl-right-inline">Banno M, Harada Y, Taniguchi M,
Tobita R, Tsujimoto H, Tsujimoto Y, Kataoka Y, Noda A. [Exercise can
improve sleep quality: A systematic review and
meta-analysis](https://doi.org/10.7717/peerj.5172). PeerJ. 2018;2018(7).
</span>

<span class="csl-left-margin">15.
</span><span class="csl-right-inline">Bartel KA, Gradisar M, Williamson
P. [Protective and risk factors for adolescent sleep: A meta-analytic
review](https://doi.org/10.1016/j.smrv.2014.08.002). Sleep Medicine
Reviews. 2015 June;21:72–85. </span>

<span class="csl-left-margin">16.
</span><span class="csl-right-inline">Edwards BA, Bristow C, O’Driscoll
DM, Wong A-M, Ghazi L, Davidson ZE, Young A, Truby H, Haines TP,
Hamilton GS. [Assessing the impact of diet, exercise and the combination
of the two as a treatment for OSA: A systematic review and
meta-analysis](https://doi.org/10.1111/resp.13580). Respirology.
2019;24(8):740–51. </span>

<span class="csl-left-margin">17.
</span><span class="csl-right-inline">Gao Y-N, Wu Y-C, Lin S-Y, Chang
JZ-C, Tu Y-K. [Short-term efficacy of minimally invasive treatments for
adult obstructive sleep apnea: A systematic review and network
meta-analysis of randomized controlled
trials](https://doi.org/10.1016/j.jfma.2018.02.008). Journal of the
Formosan Medical Association. 2019;118(4):750–65. </span>

<span class="csl-left-margin">18.
</span><span class="csl-right-inline">Iftikhar IH, Bittencourt L,
Youngstedt SD, Ayas N, Cistulli P, Schwab R, Durkin MW, Magalang UJ.
[Comparative efficacy of CPAP, MADs, exercise-training, and dietary
weight loss for sleep apnea: A network
meta-analysis](https://doi.org/10.1016/j.sleep.2016.06.001). Sleep
Medicine. 2017;30:7–14. </span>

<span class="csl-left-margin">19.
</span><span class="csl-right-inline">Iftikhar IH, Kline CE, Youngstedt
SD. [Effects of exercise training on sleep apnea: A
meta-analysis](https://doi.org/10.1007/s00408-013-9511-3). Lung.
2014;192(1):175–84. </span>

<span class="csl-left-margin">20.
</span><span class="csl-right-inline">Janssen X, Martin A, Hughes AR,
Hill CM, Kotronoulas G, Hesketh KR. [Associations of screen time,
sedentary time and physical activity with sleep in under 5s: A
systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2019.101226). Sleep
Medicine Reviews. 2020 Feb;49:101226. </span>

<span class="csl-left-margin">21.
</span><span class="csl-right-inline">Lederman O, Ward PB, Firth J,
Maloney C, Carney R, Vancampfort D, Stubbs B, Kalucy M, Rosenbaum S.
[Does exercise improve sleep quality in individuals with mental illness?
A systematic review and
meta-analysis](https://doi.org/10.1016/j.jpsychires.2018.11.004).
Journal of Psychiatric Research. 2019;109:96–106. </span>

<span class="csl-left-margin">22.
</span><span class="csl-right-inline">Lins-Filho OL, Pedrosa RP, Gomes
JML, Dantas Moraes SL, Vasconcelos BCE, Lemos CAA, Pellizzer EP. [Effect
of exercise training on subjective parameters in patients with
obstructive sleep apnea: A systematic review and
meta-analysis](https://doi.org/10.1016/j.sleep.2019.12.022). Sleep
Medicine. 2020;69:1–7. </span>

<span class="csl-left-margin">23.
</span><span class="csl-right-inline">Mendelson M, Bailly S, Marillier
M, Flore P, Borel JC, Vivodtzev I, Doutreleau S, Verges S, Tamisier R,
Pépin J-L. [Obstructive sleep apnea syndrome, objectively measured
physical activity and exercise training interventions: A systematic
review and meta-analysis](https://doi.org/10.3389/fneur.2018.00073).
Frontiers in Neurology. 2018;9(FEB). </span>

<span class="csl-left-margin">24.
</span><span class="csl-right-inline">Rubio-Arias JÁ, Marín-Cascales E,
Ramos-Campo DJ, Hernandez AV, Pérez-López FR. [Effect of exercise on
sleep quality and insomnia in middle-aged women: A systematic review and
meta-analysis of randomized controlled
trials](https://doi.org/10.1016/j.maturitas.2017.04.003). Maturitas.
2017;100:49–56. </span>

<span class="csl-left-margin">25.
</span><span class="csl-right-inline">Stutz J, Eiholzer R, Spengler CM.
[Effects of Evening Exercise on Sleep in Healthy Participants: A
Systematic Review and
Meta-Analysis](https://doi.org/10.1007/s40279-018-1015-0). Sports
Medicine. 2019;49(2):269–87. </span>

<span class="csl-left-margin">26.
</span><span class="csl-right-inline">Yang S-Y, Lan S-J, Yen Y-Y, Hsieh
Y-P, Kung P-T, Lan S-H. [Effects of Exercise on Sleep Quality in
Pregnant Women: A Systematic Review and
<span class="nocase">Meta-analysis</span> of Randomized Controlled
Trials](https://doi.org/10.1016/j.anr.2020.01.003). Asian Nursing
Research. 2020;14(1):1–10. </span>

<span class="csl-left-margin">27.
</span><span class="csl-right-inline">Yang P-Y, Ho K-H, Chen H-C, Chien
M-Y. [Exercise training improves sleep quality in middle-aged and older
adults with sleep problems: A systematic
review](https://doi.org/10.1016/S1836-9553(12)70106-6). Journal of
Physiotherapy. 2012;58(3):157–63. </span>

<span class="csl-left-margin">28.
</span><span class="csl-right-inline">Yang Y, Shin JC, Li D, An R.
[Sedentary Behavior and Sleep Problems: A Systematic Review and
Meta-Analysis](https://doi.org/10.1007/s12529-016-9609-0). International
Journal of Behavioral Medicine. 2017;24(4):481–92. </span>

<span class="csl-left-margin">29.
</span><span class="csl-right-inline">Riley RD, Dias S, Donegan S,
Tierney JF, Stewart LA, Efthimiou O, Phillippo DM. [Using individual
participant data to improve network meta-analysis
projects](https://doi.org/10.1136/bmjebm-2022-111931). BMJ
Evidence-Based Medicine. 2023 June;28(3):197–203. </span>

<span class="csl-left-margin">30.
</span><span class="csl-right-inline">Ioannidis J. [Next-generation
systematic reviews: Prospective meta-analysis, individual-level data,
networks and umbrella
reviews](https://doi.org/10.1136/bjsports-2017-097621). British Journal
of Sports Medicine. 2017 Oct;51(20):1456–8. </span>

<span class="csl-left-margin">31.
</span><span class="csl-right-inline">Hinde S, Spackman E.
[Bidirectional Citation Searching to Completion: An Exploration of
Literature Searching
Methods](https://doi.org/10.1007/s40273-014-0205-3). PharmacoEconomics.
2015 Jan;33(1):5–11. </span>

<span class="csl-left-margin">32.
</span><span class="csl-right-inline">Migueles JH, Rowlands AV, Huber F,
Sabia S, Van Hees VT. [GGIR: A Research Community–Driven Open Source R
Package for Generating Physical Activity and Sleep Outcomes From
Multi-Day Raw Accelerometer
Data](https://doi.org/10.1123/jmpb.2018-0063). Journal for the
Measurement of Physical Behaviour. 2019 Sept;2(3):188–96. </span>

<span class="csl-left-margin">33.
</span><span class="csl-right-inline">Van Hees VT, Sabia S, Jones SE,
Wood AR, Anderson KN, Kivimäki M, Frayling TM, Pack AI, Bucan M, Trenell
MI, Mazzotti DR, Gehrman PR, Singh-Manoux BA, Weedon MN. [Estimating
sleep parameters using an accelerometer without sleep
diary](https://doi.org/10.1038/s41598-018-31266-z). Scientific Reports.
2018 Aug;8(1). </span>

<span class="csl-left-margin">34.
</span><span class="csl-right-inline">Rowlands AV, Plekhanova T, Yates
T, Mirkes EM, Davies M, Khunti K, Edwardson CL. [Providing a Basis for
Harmonization of Accelerometer-Assessed Physical Activity Outcomes
Across Epidemiological
Datasets](https://doi.org/10.1123/jmpb.2018-0073). Journal for the
Measurement of Physical Behaviour. 2019 Sept;2(3):131–42. </span>

<span class="csl-left-margin">35.
</span><span class="csl-right-inline">Rowlands AV. [Moving Forward With
Accelerometer-Assessed Physical Activity: Two Strategies to Ensure
Meaningful, Interpretable, and Comparable
Measures](https://doi.org/10.1123/pes.2018-0201). Pediatric Exercise
Science. 2018 Nov;30(4):450–6. </span>

<span class="csl-left-margin">36.
</span><span class="csl-right-inline">Rowlands AV, Dawkins NP, Maylor B,
Edwardson CL, Fairclough SJ, Davies MJ, Harrington DM, Khunti K, Yates
T. [Enhancing the value of accelerometer-assessed physical activity:
Meaningful visual comparisons of data-driven translational accelerometer
metrics](https://doi.org/10.1186/s40798-019-0225-9). Sports Medicine -
Open. 2019 Dec;5(1). </span>

<span class="csl-left-margin">37.
</span><span class="csl-right-inline">Rowlands AV, Sherar LB, Fairclough
SJ, Yates T, Edwardson CL, Harrington DM, Davies MJ, Munir F, Khunti K,
Stiles VH. [A data-driven, meaningful, easy to interpret, standardised
accelerometer outcome variable for global
surveillance](https://doi.org/10.1016/j.jsams.2019.06.016). Journal of
Science and Medicine in Sport. 2019 Oct;22(10):1132–8. </span>

<span class="csl-left-margin">38.
</span><span class="csl-right-inline">Gába A, Dygrýn J, Štefelová N,
Rubín L, Hron K, Jakubec L, Pedišić Ž. [How do short sleepers use extra
waking hours? A compositional analysis of 24-h time-use patterns among
children and adolescents](https://doi.org/10.1186/s12966-020-01004-8).
International Journal of Behavioral Nutrition and Physical Activity.
2020 Dec;17(1). </span>

<span class="csl-left-margin">39.
</span><span class="csl-right-inline">Van Hees VT, Gorzelniak L, Dean
León EC, Eder M, Pias M, Taherian S, Ekelund U, Renström F, Franks PW,
Horsch A, Brage S. [Separating Movement and Gravity Components in an
Acceleration Signal and Implications for the Assessment of Human Daily
Physical Activity](https://doi.org/10.1371/journal.pone.0061691). Müller
M, editor. PLoS ONE. 2013 Apr;8(4):e61691. </span>

<span class="csl-left-margin">40.
</span><span class="csl-right-inline">Weston SJ, Ritchie SJ, Rohrer JM,
Przybylski AK. [Recommendations for Increasing the Transparency of
Analysis of Preexisting Data
Sets](https://doi.org/10.1177/2515245919848684). Advances in Methods and
Practices in Psychological Science. 2019 Sept;2(3):214–27. </span>

<span class="csl-left-margin">41.
</span><span class="csl-right-inline">MacCoun R, Perlmutter S. [Blind
analysis: Hide results to seek the
truth](https://doi.org/10.1038/526187a). Nature. 2015
Oct;526(7572):187–9. </span>

<span class="csl-left-margin">42.
</span><span class="csl-right-inline">Buuren SV, Groothuis-Oudshoorn K.
[**Mice** : Multivariate Imputation by Chained Equations in
*R*](https://doi.org/10.18637/jss.v045.i03). Journal of Statistical
Software. 2011;45(3). </span>

<span class="csl-left-margin">43.
</span><span class="csl-right-inline">Curran PJ, Hussong AM.
[Integrative data analysis: The simultaneous analysis of multiple data
sets.](https://doi.org/10.1037/a0015914) Psychological Methods.
2009;14(2):81–100. </span>

<span class="csl-left-margin">44.
</span><span class="csl-right-inline">Windred DP, Burns AC, Lane JM,
Saxena R, Rutter MK, Cain SW, Phillips AJK. [Sleep regularity is a
stronger predictor of mortality risk than sleep duration: A prospective
cohort study](https://doi.org/10.1093/sleep/zsad253). SLEEP. 2024
Jan;47(1):zsad253. </span>

<span class="csl-left-margin">45.
</span><span class="csl-right-inline">Lok R, Suh S, Rue S, Weed L,
Zeitzer JM. [How many days are enough? Sleep–wake timing regularity and
fragmentation scores change with the number of days
included](https://doi.org/10.1111/jsr.14332). Journal of Sleep Research.
2024 Sept;e14332. </span>
