# Introduction

Sleep is an important determinant of population health \[1\]. The
consequences of poor sleep are profound and include increased all-cause
mortality \[2\], cardiovascular disease \[3\], cancers \[4\], and poor
mental health \[5\]. Poor sleep is common: over 50% of children and
adolescents \[6\] and 35% of adults \[7\] report lower sleep duration
than recommended. Improving sleep should be a priority for public health
policy and more safe and accessible strategies that promote sleep are
needed \[8\]. In adults, being physically active appears to improve both
the quantity and quality of sleep \[9\] and the World Health
Organisation recommends physical activity as a method to improve sleep
in adults \[10\]. In children, adolescents, and older adults
associations are unclear \[8,11,12\] and guidelines do not currently
support a role for physical activity to improve sleep.

Promoting physical activity may be an effective method to improve sleep
across the lifespan but the published literature to date has at least
two limitations that prevent firm conclusions to support health
guidelines. First, most studies have relied on self-report measures of
sleep and physical activity \[8,11,12\], which could confound
associations and limit the precision of analyses designed to identify a
dose-response relationship between physical activity and sleep. Second,
no study has collected data from a large sample that included
participants across the lifespan. Umbrella reviews \[8\] and
meta-analyses \[3,4,9,11–28\] have provided some suggestive evidence
about how physical activity as a means of improving sleep may vary
across the lifespan. However, their reliance on aggregated study-level
effects limits the quality and scope of data that can be used to analyse
the influence of age \[29\]. Individual participant data (IPD) analysis
can overcome these limitations \[30\].

In this study we harmonised individual participant data (IPD) from
activity monitor observations obtained from studies around the world.
The objective of this pooled IPD study was to investigate the
relationship between device-measured physical activity and sleep across
the lifespan. We examined whether individuals who are more physically
active have better sleep, whether age, sex, and socio-cultural factors
moderate the association between physical activity and sleep, and
whether there is a bidirectional association between physical activity
and sleep.

# Methods

We pre-registered all methods and analyses on the Open Science Framework
(OSF) before data acquisition began (<https://osf.io/ahd96>).

## Data sources and data acquisition

We included data from studies, with any design, that used activity
monitors and a 24-hour wear time protocol over a minimum of 4 days,
including 1 weekend day. Studies needed to have used research-grade
accelerometers from which raw accelerations (*m*/*s*<sup>2</sup> or
*m**g*) could be obtained. Studies could use any wear location for the
accelerometer (hip, wrist, or thigh).

We identified potentially relevant studies by examining the reference
lists of recent systematic reviews that explored the relationship
between sleep and physical activity in children, adolescents, and adults
\[9,11,12\]. We then used forward searching on potentially eligible
studies \[31\]. We additionally performed a literature search to
identify accelerometry consortiums and individual studies, searched
trial registries (ANZCTR, ICTRP, ISRCTN) and the Open Science Framework
for unpublished results, and we asked collaborators to help identify any
additional studies.

We contacted chief investigators or consortium leads by email to request
access to deidentified individual data. To make the data acquisition
manageable, we only invited studies with sample sizes larger than 400
participants. If required by the contributing research group, the
contributing investigators or their university signed a data transfer
agreement. All studies included in the pooled data meta-analysis had to
have been approved by their local institutional ethical committees, and
all participants had to have provided written informed consent. The lead
university’s Research Ethics Committee granted permission for each
study’s data to be obtained, and investigators transferred their
deidentified data via a secure file-sharing system.

## Data harmonisation and outcomes

All activity monitor data were harmonised by reprocessing raw
accelerometer files using an open-source GGIR package in R to produce
physical activity and sleep outcomes \[32,33\]. By reprocessing all raw
accelerometer data, we were able to generate outcomes that could be
compared across studies, devices wear location, and populations \[34\].
Data were reprocessed by the core research team, or by data contributors
themselves using the GGIR package processing script provided by the core
research team. Reprocessed individual study datasets were then pooled
into a single dataset. A detailed description of the reprocessing method
and decisions for specific accelerometer devices is available in an
online supplementary file.

For sleep, GGIR produced the following metrics; total sleep time
(minutes), sleep efficiency (percentage of time asleep within the sleep
period), sleep regularity (regularity of sleep and awake patterns on a
day-to-day basis calculated as the sleep regularity index), and timing
(time of sleep and awakening within the 24-hour time cycle). For
physical activity, we used volume and intensity metrics proposed by
Rowlands et al \[35\]. These measures are 1) average acceleration
(measure of physical activity volume) and 2) intensity gradient (measure
of intensity distribution where the natural logs of time and intensity
are used to produce a straight-line graph with the slope of the graph
being the intensity gradient). We used this approach to model the full
continuum of physical activity intensity for our main analyses \[36\]
and avoid collapsing physical activity data into population-specific
intensity categories which occurs when intensity cut-points are applied
to accelerometer data \[37\].

For the purposes of translating physical activity data for
interpretation and public health recommendations and for time-use based
analyses we then determined; 1) the number of minutes spent in
acceleration intensity bands in 50*m**g* increments \[35\], 2) the
number of minutes spent in light and moderate to vigorous physical
activity using age appropriate cut-points applied to the time in
intensity bands data, and 3) the minimum acceleration measured for a
range of different durations, this is the *M**X* metric where *X* refers
to the duration. For example, *M*60 refers to the minimum acceleration
for the most active 60 minutes of the day \[36\]. We also categorise all
awake time into bout lengths for sedentary, light, and moderate to
vigorous physical activity bouts using the categories defined by Gába et
al \[38\]. After extracting sleep and physical activity, the remaining
time within the 24-hour time cycle was classified as sedentary
behaviour. Non-wear time and abnormally high acceleration values were
detected using the data processing default setting \[39\]. These data
were classified as missing and imputed using the accelerometer data from
the same time interval on the remaining days of the week.

## Covariates and moderators

The following individual participant data were obtained when available;
age, sex, height, weight, waist circumference, maturational status,
socioeconomic status (SES), ethnicity, screen time, sleep medications,
and medically diagnosed conditions known to affect sleep. The country,
city, time zone, daylight length, and season in which data were
collected were also obtained.

## Statistical analysis

To reduce the risk of bias in secondary data analysis of IPD, we used a
blind analysis approach \[40\]. We developed and tested models on a 10%
holdout subsample of the dataset. When the analysis code was ready, we
conducted the final analysis \[41\].

We checked all data for implausibly low or high measurements which were
excluded if they fell outside of ±4SD of the mean, as these likely
indicate measurement device failures. We assumed missing covariate and
moderator data to be missing at random or completely at random and
imputed these missing values using multiple imputation by chained
equations with the *mice* package in R \[42\] to provide 50 imputed
datasets. Specifically, we used multilevel predictive mean matching
imputation clustered by participants. We used Rubin’s rule to pool the
results from analyses conducted using each imputed dataset into one
single set of results. For all analyses, we examined 95% confidence
intervals and used P &lt; 0.05 to assess for statistical significance.
We used R (version 4.3.2) for all data cleaning and analysis. We report
mean and 95% confidence intervals. For categorical variables,
percentages are reported. We examined model diagnostics from all models
(see supplementary materials).

To examine the relationship between sleep and physical activity
(Research Questions 1-2) we fit mixed-effects models with random
intercepts for study ID to account for the nesting of participants in
studies \[43\]. We predict each sleep indicator by each physical
activity indicator controlling for sex, body mass index (BMI) and
region. Quadratic terms were introduced in each model to allow for
curvilinear relationships to be modelled. We interacted each predictor
with age to reveal how the relationship between physical activity and
sleep changes over the lifespan.

## Protocol deviations

This paper addresses Hypotheses 1 and 2 from our registered protocol. We
will address remaining hypotheses in future research. To streamline our
analysis and reduce redundancy, we made some changes to our protocol
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
the protocol) can be found in the supplementary materials, and are
qualitatively similar to the main results.

### Sleep indicator variable

Our research examines various metrics of sleep: onset, duration,
regularity, and efficiency. We initially planned to merge these
variables into a single indicator to simplify analysis. However, no
validated indicator existed which could be estimated from our measured
variables. We decided against creating one, as we could not validate it
with our data. Consequently, our models illustrate the relationship
between each aspect of sleep and physical activity, instead of
describing a straightforward link between overall sleep quality and
physical activity.

### Log transformation

We aimed to improve model diagnostics using log transformations of
skewed variables. Pilot analysis using holdout data revealed that while
diagnostics improved on the log scale, model predictions followed
extreme exponential patterns near the edge of the data range when
back-transformed. Accordingly, we do not use log transformations in the
main paper, however these models can be found in supplementary
materials.

## Patient and public involvement

Patients and members of the public were not involved in the design,
analysis or interpretation of this IPD study using secondary data.
Results of this study have broad public health interest, particularly
policymakers. Findings from this study will be disseminated through
institutional websites, press and media platform releases, and tailored
messages to public health organisations and governing bodies.

# Results

The aggregated dataset describes 220,612 observations of daily physical
activity and sleep from 28,823 unique participants. Of these, 71,451
days were excluded as they had insufficient wear time, which also
resulted in the exclusion of 4,214 participants. Table
@ref(tab:demo-participants) shows demographic information for the
included participants. The majority of participants came from Europe,
Oceania, and South America, with limited data from other regions. There
was a near-even split by sex (50.5% female), but age skewed towards both
younger children (2-11 years, 39.8%) and older adults (66+ years,
26.8%).

Valid observations were not uniform across the days of the week
(*χ*<sub>(6)</sub><sup>2</sup> = 298.93, p = &lt; .001). Weekends were
over-represented (Saturday: z = 6.09; Sunday: z = 11.44), while
Wednesdays (z = -9.77) and Thursdays (z = -8.54) were under-represented.
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
<td style="text-align: left;">9,758</td>
<td style="text-align: left;">3,062</td>
<td style="text-align: left;">473</td>
<td style="text-align: left;">4,675</td>
<td style="text-align: left;">6,563</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">9.49 (1.51)</td>
<td style="text-align: left;">13.71 (2.39)</td>
<td style="text-align: left;">23.90 (6.13)</td>
<td style="text-align: left;">56.72 (7.55)</td>
<td style="text-align: left;">72.21 (4.73)</td>
</tr>
<tr>
<td style="text-align: left;">   BMI</td>
<td style="text-align: left;">17.95 (3.30)</td>
<td style="text-align: left;">20.39 (4.00)</td>
<td style="text-align: left;">24.71 (5.88)</td>
<td style="text-align: left;">26.87 (4.95)</td>
<td style="text-align: left;">27.08 (4.42)</td>
</tr>
<tr>
<td style="text-align: left;">   Valid Weartime Hours</td>
<td style="text-align: left;">23.11 (1.67)</td>
<td style="text-align: left;">23.07 (1.75)</td>
<td style="text-align: left;">23.35 (1.53)</td>
<td style="text-align: left;">23.72 (0.87)</td>
<td style="text-align: left;">23.87 (0.60)</td>
</tr>
<tr>
<td style="text-align: left;">   Valid Weatime Days</td>
<td style="text-align: left;">5.56 (1.93)</td>
<td style="text-align: left;">5.14 (1.78)</td>
<td style="text-align: left;">4.64 (1.36)</td>
<td style="text-align: left;">7.27 (2.60)</td>
<td style="text-align: left;">6.49 (2.12)</td>
</tr>
<tr>
<td style="text-align: left;">   PA Intensity</td>
<td style="text-align: left;">-2.12 (0.19)</td>
<td style="text-align: left;">-2.20 (0.19)</td>
<td style="text-align: left;">-2.36 (0.20)</td>
<td style="text-align: left;">-2.49 (0.20)</td>
<td style="text-align: left;">-2.67 (0.22)</td>
</tr>
<tr>
<td style="text-align: left;">   PA Volume</td>
<td style="text-align: left;">43.79 (25.80)</td>
<td style="text-align: left;">43.13 (17.26)</td>
<td style="text-align: left;">41.12 (11.59)</td>
<td style="text-align: left;">39.60 (13.14)</td>
<td style="text-align: left;">31.30 (10.41)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Duration (min)</td>
<td style="text-align: left;">485.41 (54.44)</td>
<td style="text-align: left;">449.68 (66.67)</td>
<td style="text-align: left;">412.24 (57.99)</td>
<td style="text-align: left;">408.17 (56.56)</td>
<td style="text-align: left;">405.08 (63.91)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Efficiency (%)</td>
<td style="text-align: left;">80.88 (7.56)</td>
<td style="text-align: left;">83.92 (7.90)</td>
<td style="text-align: left;">87.47 (6.26)</td>
<td style="text-align: left;">87.81 (5.55)</td>
<td style="text-align: left;">86.46 (6.31)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Onset (HH:MM clock time)</td>
<td style="text-align: left;">20:59 (01:12)</td>
<td style="text-align: left;">22:13 (01:52)</td>
<td style="text-align: left;">23:50 (01:25)</td>
<td style="text-align: left;">23:37 (01:21)</td>
<td style="text-align: left;">24:22 (01:17)</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep Regularity</td>
<td style="text-align: left;">54.53 (12.67)</td>
<td style="text-align: left;">54.22 (13.10)</td>
<td style="text-align: left;">54.98 (12.51)</td>
<td style="text-align: left;">59.50 (11.44)</td>
<td style="text-align: left;">54.83 (12.53)</td>
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
<td style="text-align: left;">2,479 (25.40%)</td>
<td style="text-align: left;">1,928 (62.97%)</td>
<td style="text-align: left;">473 (100.00%)</td>
<td style="text-align: left;">4,675 (100.00%)</td>
<td style="text-align: left;">6,563 (100.00%)</td>
</tr>
<tr>
<td style="text-align: left;">   Hip</td>
<td style="text-align: left;">7,279 (74.60%)</td>
<td style="text-align: left;">1,134 (37.03%)</td>
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
<td style="text-align: left;">1,830 (18.75%)</td>
<td style="text-align: left;">  469 (15.32%)</td>
<td style="text-align: left;">34 (7.19%)</td>
<td style="text-align: left;">   87 (1.86%)</td>
<td style="text-align: left;">  263 (4.01%)</td>
</tr>
<tr>
<td style="text-align: left;">   Unclear</td>
<td style="text-align: left;">4,016 (41.16%)</td>
<td style="text-align: left;">1,620 (52.91%)</td>
<td style="text-align: left;">235 (49.68%)</td>
<td style="text-align: left;">1,317 (28.17%)</td>
<td style="text-align: left;">2,475 (37.71%)</td>
</tr>
<tr>
<td style="text-align: left;">   White</td>
<td style="text-align: left;">3,912 (40.09%)</td>
<td style="text-align: left;">  973 (31.78%)</td>
<td style="text-align: left;">204 (43.13%)</td>
<td style="text-align: left;">3,271 (69.97%)</td>
<td style="text-align: left;">3,825 (58.28%)</td>
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
<td style="text-align: left;">3,203 (32.82%)</td>
<td style="text-align: left;">1,189 (38.83%)</td>
<td style="text-align: left;">234 (49.47%)</td>
<td style="text-align: left;">4,305 (92.09%)</td>
<td style="text-align: left;">6,560 (99.95%)</td>
</tr>
<tr>
<td style="text-align: left;">   Oceania</td>
<td style="text-align: left;">1,761 (18.05%)</td>
<td style="text-align: left;">  552 (18.03%)</td>
<td style="text-align: left;">123 (26.00%)</td>
<td style="text-align: left;">  370 (7.91%)</td>
<td style="text-align: left;">    3 (0.05%)</td>
</tr>
<tr>
<td style="text-align: left;">   South America</td>
<td style="text-align: left;">2,074 (21.25%)</td>
<td style="text-align: left;">  984 (32.14%)</td>
<td style="text-align: left;">116 (24.52%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Africa</td>
<td style="text-align: left;">  876 (8.98%)</td>
<td style="text-align: left;">  167 (5.45%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   Asia</td>
<td style="text-align: left;">  610 (6.25%)</td>
<td style="text-align: left;">   90 (2.94%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr>
<td style="text-align: left;">   North America</td>
<td style="text-align: left;">1,234 (12.65%)</td>
<td style="text-align: left;">   80 (2.61%)</td>
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
<td style="text-align: left;">3,213 (32.93%)</td>
<td style="text-align: left;">  668 (21.82%)</td>
<td style="text-align: left;">55 (11.63%)</td>
<td style="text-align: left;">1,154 (24.68%)</td>
<td style="text-align: left;">1,380 (21.03%)</td>
</tr>
<tr>
<td style="text-align: left;">   Spring</td>
<td style="text-align: left;">2,268 (23.24%)</td>
<td style="text-align: left;">1,588 (51.86%)</td>
<td style="text-align: left;">265 (56.03%)</td>
<td style="text-align: left;">1,152 (24.64%)</td>
<td style="text-align: left;">2,107 (32.10%)</td>
</tr>
<tr>
<td style="text-align: left;">   Summer</td>
<td style="text-align: left;">1,169 (11.98%)</td>
<td style="text-align: left;">  311 (10.16%)</td>
<td style="text-align: left;">84 (17.76%)</td>
<td style="text-align: left;">1,172 (25.07%)</td>
<td style="text-align: left;">1,378 (21.00%)</td>
</tr>
<tr>
<td style="text-align: left;">   Winter</td>
<td style="text-align: left;">3,108 (31.85%)</td>
<td style="text-align: left;">  495 (16.17%)</td>
<td style="text-align: left;">69 (14.59%)</td>
<td style="text-align: left;">1,197 (25.60%)</td>
<td style="text-align: left;">1,698 (25.87%)</td>
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
<td style="text-align: left;">5,118 (52.45%)</td>
<td style="text-align: left;">1,593 (52.02%)</td>
<td style="text-align: left;">306 (64.69%)</td>
<td style="text-align: left;">2,619 (56.02%)</td>
<td style="text-align: left;">2,725 (41.52%)</td>
</tr>
<tr>
<td style="text-align: left;">   Male</td>
<td style="text-align: left;">4,640 (47.55%)</td>
<td style="text-align: left;">1,469 (47.98%)</td>
<td style="text-align: left;">167 (35.31%)</td>
<td style="text-align: left;">2,056 (43.98%)</td>
<td style="text-align: left;">3,838 (58.48%)</td>
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
<td style="text-align: left;">   Yes</td>
<td style="text-align: left;">   49 (0.50%)</td>
<td style="text-align: left;">    1 (0.03%)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">  261 (5.58%)</td>
<td style="text-align: left;">  682 (10.39%)</td>
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
<td style="text-align: left;">3,607 (36.96%)</td>
<td style="text-align: left;">  819 (26.75%)</td>
<td style="text-align: left;">65 (13.74%)</td>
<td style="text-align: left;">1,162 (24.86%)</td>
<td style="text-align: left;">2,490 (37.94%)</td>
</tr>
<tr>
<td style="text-align: left;">   Medium</td>
<td style="text-align: left;">2,670 (27.36%)</td>
<td style="text-align: left;">1,105 (36.09%)</td>
<td style="text-align: left;">265 (56.03%)</td>
<td style="text-align: left;">2,221 (47.51%)</td>
<td style="text-align: left;">2,569 (39.14%)</td>
</tr>
<tr>
<td style="text-align: left;">   High</td>
<td style="text-align: left;">3,481 (35.67%)</td>
<td style="text-align: left;">1,138 (37.17%)</td>
<td style="text-align: left;">143 (30.23%)</td>
<td style="text-align: left;">1,292 (27.64%)</td>
<td style="text-align: left;">1,504 (22.92%)</td>
</tr>
</tbody>
</table>

*Note.* 78 participants had missing age data. For categorical variables
the value is the count, and percentage. For numeric variables the value
is the Mean and SD. Total N = 24,609

 

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
controlling for SES, sex, and BMI.</caption>
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
<td style="text-align: left;">0.24 [0.06, 0.41]</td>
<td style="text-align: left;">0.09</td>
<td style="text-align: left;">2.69</td>
<td style="text-align: left;">.007</td>
<td style="text-align: left;">0.24 [0.07, 0.40]</td>
<td style="text-align: left;">0.08</td>
<td style="text-align: left;">2.85</td>
<td style="text-align: left;">.004</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.10 [0.08, 0.12]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">10.88</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.08 [0.07, 0.10]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">10.50</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.00</td>
<td style="text-align: left;">.003</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-2.92</td>
<td style="text-align: left;">.004</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-4.94</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.01 [0.00, 0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">2.76</td>
<td style="text-align: left;">.006</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.57</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-5.73</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.05</td>
<td style="text-align: left;">.959</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-4.89</td>
<td style="text-align: left;">&lt; .001</td>
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
<td style="text-align: left;">0.26 [0.09, 0.44]</td>
<td style="text-align: left;">0.09</td>
<td style="text-align: left;">2.92</td>
<td style="text-align: left;">.004</td>
<td style="text-align: left;">0.29 [0.10, 0.47]</td>
<td style="text-align: left;">0.10</td>
<td style="text-align: left;">2.99</td>
<td style="text-align: left;">.003</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.10 [0.09, 0.12]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">11.30</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.06 [0.05, 0.08]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">7.97</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.18</td>
<td style="text-align: left;">.855</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.86</td>
<td style="text-align: left;">.391</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.02, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-11.97</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.04, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-6.69</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-10.55</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-7.17</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">4.79</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">5.40</td>
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
<td style="text-align: left;">0.00 [-0.27, 0.27]</td>
<td style="text-align: left;">0.14</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">.995</td>
<td style="text-align: left;">0.00 [-0.26, 0.26]</td>
<td style="text-align: left;">0.13</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">.988</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">-0.03 [-0.04, -0.01]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-3.61</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.06 [-0.07, -0.05]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-10.30</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.73</td>
<td style="text-align: left;">.083</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.22</td>
<td style="text-align: left;">.222</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.01 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-4.30</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.04, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-9.25</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-11.48</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.35</td>
<td style="text-align: left;">.178</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">8.83</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">11.29</td>
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
<td style="text-align: left;">0.53 [0.38, 0.67]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">7.18</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.71 [0.53, 0.89]</td>
<td style="text-align: left;">0.09</td>
<td style="text-align: left;">7.60</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity</td>
<td style="text-align: left;">0.24 [0.22, 0.25]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">25.42</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.27 [0.25, 0.29]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">29.15</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.06</td>
<td style="text-align: left;">.293</td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.08</td>
<td style="text-align: left;">.003</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-15.45</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.10 [-0.11, -0.09]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-18.83</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Physical activity <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.48</td>
<td style="text-align: left;">.141</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-21.58</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Physical activity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-5.89</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">9.47</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
</tbody>
</table>

*Note.* Adjusted for SES, sex, and BMI. Outcomes variables are listed in
the column headers.

 

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
controlling for SES, sex, and BMI</caption>
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
<td style="text-align: left;">1.64 [1.29, 1.99]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">9.17</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.46 [1.32, 1.61]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">20.34</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration</td>
<td style="text-align: left;">0.00 [-0.02, 0.03]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">0.24</td>
<td style="text-align: left;">.813</td>
<td style="text-align: left;">0.00 [-0.01, 0.02]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">0.43</td>
<td style="text-align: left;">.667</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-38.29</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-48.38</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.41</td>
<td style="text-align: left;">.161</td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.70</td>
<td style="text-align: left;">.486</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep duration <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.34</td>
<td style="text-align: left;">.184</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.96</td>
<td style="text-align: left;">.052</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep duration<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-3.23</td>
<td style="text-align: left;">.002</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-2.15</td>
<td style="text-align: left;">.032</td>
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
<td style="text-align: left;">1.63 [1.28, 1.98]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">9.13</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.46 [1.32, 1.61]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">20.40</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency</td>
<td style="text-align: left;">0.02 [0.00, 0.03]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">1.98</td>
<td style="text-align: left;">.051</td>
<td style="text-align: left;">0.03 [0.02, 0.04]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">4.45</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-38.52</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-48.47</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.86</td>
<td style="text-align: left;">.065</td>
<td style="text-align: left;">0.00 [0.00, 0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">4.28</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep efficiency <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.89</td>
<td style="text-align: left;">.373</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-4.13</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep efficiency<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-1.32</td>
<td style="text-align: left;">.188</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-2.79</td>
<td style="text-align: left;">.005</td>
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
<td style="text-align: left;">1.65 [1.29, 2.00]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">9.12</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.47 [1.33, 1.61]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">20.28</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset</td>
<td style="text-align: left;">-0.03 [-0.08, 0.01]</td>
<td style="text-align: left;">0.02</td>
<td style="text-align: left;">-1.41</td>
<td style="text-align: left;">.165</td>
<td style="text-align: left;">-0.01 [-0.04, 0.02]</td>
<td style="text-align: left;">0.02</td>
<td style="text-align: left;">-0.48</td>
<td style="text-align: left;">.630</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.03 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-38.31</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-48.15</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">-0.02 [-0.03, 0.00]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">-2.61</td>
<td style="text-align: left;">.011</td>
<td style="text-align: left;">0.00 [-0.01, 0.01]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">.992</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep onset <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.16</td>
<td style="text-align: left;">.249</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.82</td>
<td style="text-align: left;">.414</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep onset<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">1.34</td>
<td style="text-align: left;">.182</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.78</td>
<td style="text-align: left;">.435</td>
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
<td style="text-align: left;">1.59 [1.25, 1.94]</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">9.08</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">1.45 [1.31, 1.58]</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">20.69</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity</td>
<td style="text-align: left;">0.11 [0.09, 0.12]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">12.20</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.08 [0.07, 0.09]</td>
<td style="text-align: left;">0.01</td>
<td style="text-align: left;">13.75</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age</td>
<td style="text-align: left;">-0.02 [-0.03, -0.02]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-38.26</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">-0.03 [-0.03, -0.03]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-48.34</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity<span
class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.43</td>
<td style="text-align: left;">.668</td>
<td style="text-align: left;">0.00 [-0.01, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-2.11</td>
<td style="text-align: left;">.035</td>
</tr>
<tr>
<td style="text-align: left;">   Sleep regularity <span
class="math inline">×</span> age</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-10.39</td>
<td style="text-align: left;">&lt; .001</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-12.13</td>
<td style="text-align: left;">&lt; .001</td>
</tr>
<tr>
<td style="text-align: left;">   Age <span class="math inline">×</span>
Sleep regularity<span class="math inline"><sup>2</sup></span></td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">-0.05</td>
<td style="text-align: left;">.959</td>
<td style="text-align: left;">0.00 [0.00, 0.00]</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.53</td>
<td style="text-align: left;">.598</td>
</tr>
</tbody>
</table>

*Note.* Adjusted for SES, sex, and BMI. Outcomes variables are listed in
the row headers.

 

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
\[9\]. For example, \[9\] found small (0.22 &lt; *d* &lt; 0.30) but
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
\[8\], or that they are habitual and linked to factors such as the value
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
\[9\] while those conducted in children and adolescents are less clear
\[8,11,12\]. This would seem to suggest that the differences in findings
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
interventions may still be a useful treatment for these conditions
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
associations. Finally, while our study captured data from across the
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
Jun;35(6):727–34. </span>

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Huang BH, Duncan MJ, Cistulli PA,
Nassar N, Hamer M, Stamatakis E. [Sleep and physical activity in
relation to all-cause, cardiovascular disease and cancer mortality
risk](https://doi.org/10.1136/bjsports-2021-104046). British Journal of
Sports Medicine. 2022 Jul;56(13):718–24. </span>

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
</span><span class="csl-right-inline">Wheaton AG, Jones SE, Cooper AC,
Croft JB. [Short Sleep Duration Among Middle School and High School
Students - United States, 2015](https://doi.org/10.15585/mmwr.mm6703a1).
MMWR Morbidity and mortality weekly report. 2018 Jan;67(3):85–90.
</span>

<span class="csl-left-margin">7.
</span><span class="csl-right-inline">Liu Y, Wheaton AG, Chapman DP,
Cunningham TJ, Lu H, Croft JB. [Prevalence of Healthy Sleep Duration
among Adults–United States,
2014](https://doi.org/10.15585/mmwr.mm6506a1). MMWR Morbidity and
mortality weekly report. 2016 Feb;65(6):137–41. </span>

<span class="csl-left-margin">8.
</span><span class="csl-right-inline">Kline CE, Hillman CH, Bloodgood
Sheppard B, Tennant B, Conroy DE, Macko RF, Marquez DX, Petruzzello SJ,
Powell KE, Erickson KI. [Physical activity and sleep: An updated
umbrella review of the 2018 Physical Activity Guidelines Advisory
Committee report](https://doi.org/10.1016/j.smrv.2021.101489). Sleep
Medicine Reviews. 2021 Aug;58:101489. </span>

<span class="csl-left-margin">9.
</span><span class="csl-right-inline">Kredlow MA, Capozzoli MC, Hearon
BA, Calkins AW, Otto MW. [The effects of physical activity on sleep: A
meta-analytic review](https://doi.org/10.1007/s10865-015-9617-6).
Journal of Behavioral Medicine. 2015 Jun;38(3):427–49. </span>

<span class="csl-left-margin">10.
</span><span class="csl-right-inline">Bull FC, Al-Ansari SS, Biddle S,
Borodulin K, Buman MP, Cardon G, Carty C, Chaput JP, Chastin S, Chou R,
Dempsey PC, DiPietro L, Ekelund U, Firth J, Friedenreich CM, Garcia L,
Gichu M, Jago R, Katzmarzyk PT, Lambert E, Leitzmann M, Milton K, Ortega
FB, Ranasinghe C, Stamatakis E, Tiedemann A, Troiano RP, Van Der Ploeg
HP, Wari V, Willumsen JF. [World Health Organization 2020 guidelines on
physical activity and sedentary
behaviour](https://doi.org/10.1136/bjsports-2020-102955). British
Journal of Sports Medicine. 2020 Dec;54(24):1451–62. </span>

<span class="csl-left-margin">11.
</span><span class="csl-right-inline">Antczak D, Lonsdale C, Lee J,
Hilland T, Duncan MJ, del Pozo Cruz B, Hulteen RM, Parker PD, Sanders T.
[Physical activity and sleep are inconsistently related in healthy
children: A systematic review and
meta-analysis](https://doi.org/10.1016/j.smrv.2020.101278). Sleep
Medicine Reviews. 2020 Jun;51:101278. </span>

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
Reviews. 2015 Jun;21:72–85. </span>

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
Evidence-Based Medicine. 2023 Jun;28(3):197–203. </span>

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
Measurement of Physical Behaviour. 2019 Sep;2(3):188–96. </span>

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
Measurement of Physical Behaviour. 2019 Sep;2(3):131–42. </span>

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
Practices in Psychological Science. 2019 Sep;2(3):214–27. </span>

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
2024 Sep;e14332. </span>
