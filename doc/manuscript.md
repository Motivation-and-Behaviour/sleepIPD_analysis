# Examining the bidirectional relationship between sleep and physical activity using pooled individual data


<div class="no-count">

Taren Sanders PhD<sup>1</sup>, Timothy Hartwig PhD<sup>2</sup>, James H.
Conigrave PhD<sup>3</sup>, Borja del Pozo Cruz PhD<sup>4</sup>, Philip
Parker PhD<sup>5</sup>, Michael Noetel PhD<sup>6</sup>, Devan Antczak
PhD<sup>7</sup>, Stuart J. Fairclough PhD<sup>8</sup>, Shona L. Halson
PhD<sup>2</sup>, ***DATASET PROVIDERS TBD***<sup>9</sup>, Chris Lonsdale
PhD<sup>10</sup>

<sup>1</sup>Institute for Positive Psychology and Education, Australian
Catholic University, North Sydney, Australia\
<sup>2</sup>School of Behavioural and Health Science, Australian
Catholic University, Strathfield, Australia\
<sup>3</sup>Centre for Alcohol Policy Research, School of Psychology and
Public Health, La Trobe University, Bundoora, Australia\
<sup>4</sup>Department of Sport Sciences, Faculty of Medicine, Health,
and Sports, Universidad Europea de Madrid\
<sup>5</sup>Office of the Deputy Vice-Chancellor (Research and
Enterprise), Australian Catholic University, North Sydney, Australia\
<sup>6</sup>School of Psychology, The University of Queensland,
Brisbane, Australia\
<sup>7</sup>School of Education, University of Wollongong, Wollongong,
Australia\
<sup>8</sup>Department of Sport and Physical Activity, Edge Hill
University, Ormskirk, United Kingdom\
<sup>9</sup>***TBD***\
<sup>10</sup>Office of the Provost, Australian Catholic University,
North Sydney, Australia

Correspondence to: Taren Sanders, 33 Berry St, North Sydney, NSW,
Australia; Taren.Sanders@acu.edu.au, +61 2 9701 4704

Main text: 3446 words

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
Mixed-effects models with age interactions separated within-person
(day-to-day) from between-person (habitual) associations of physical
activity with that night’s sleep, and of sleep with next-day activity,
adjusting for sex, BMI, and socioeconomic status. The protocol was
pre-registered (https://osf.io/ahd96).

**Findings** In 27,236 participants aged 2 to 94 years from 20 studies,
participants slept longer and fell asleep earlier on days when they were
more active than usual. A 1 SD higher volume than usual was associated
with 9.6 min longer sleep (standardised β \[95% CI\] 0.11 \[0.10,
0.12\]) and 12 min earlier sleep onset at age 11 years, and with 8.6 min
and 20 min, respectively, at age 65 years. A night’s sleep was barely
associated with next-day activity (all \|β\| \< 0.03). Habitually more
active participants had more regular sleep, but the day-level
association with sleep regularity reflected how the regularity index was
measured.

**Interpretation** At every age, people slept slightly longer and
earlier after more active days. Physical activity could be promoted to
support sleep in children and adolescents as well as adults, as a small,
consistent benefit rather than a remedy for poor sleep.

**Funding** This research was unfunded.

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
change with age. Whether the relationship runs in both directions, and
whether people sleep better after more active days rather than only
being more active in general, needs device-measured, day-level data.
These have been synthesised separately in adults, where a meta-analysis
of 33 studies did not support a bidirectional daily association, and in
children and youth, where a meta-analysis of 11 studies found weak and
inconsistent associations. Recent studies of consumer wearables with
millions of person-nights included only adults and did not examine sleep
regularity.

## Added value of this study

To our knowledge, this is the first study to examine day-to-day
associations between device-measured physical activity and sleep in a
large, geographically diverse sample spanning the lifespan. We
reprocessed raw accelerometer data from 20 studies with a common method,
giving daily data for 27,236 participants aged 2 to 94 years. By
separating day-to-day from habitual differences, we found that on days
when people were more active than usual they slept longer and fell
asleep earlier that night, from childhood to older age. These
associations were small, under 10 minutes of sleep per 1 SD of activity
volume, and a night’s sleep was barely associated with next-day
activity. Habitually more active people had more regular sleep.

## Implications of all the available evidence

Trials show that physical activity improves sleep in adults, and our
findings show that people of all ages, including children and
adolescents, sleep slightly longer and earlier after more active days.
Public health messages could present physical activity as a small,
consistent support for sleep at every age, rather than as a remedy for
poor sleep. That the day-level associations varied little with age
suggests that the weaker evidence in children and adolescents may
reflect study design or measurement rather than age. Trials should test
whether increasing physical activity lengthens sleep in children and
adolescents, and studies of sleep regularity should use measures that
separate sleep from daytime rest.

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
data matter for two reasons. First, they separate whether people who are
habitually more active sleep better (a between-person association, open
to confounding by stable characteristics) from whether people sleep
better after days on which they are more active than usual (a
within-person association, closer to the effect of changing one’s
activity). Second, the relationship may run in both directions: physical
activity may promote sleep through circadian and stress-related
pathways,<sup>18</sup> while a late or poor night’s sleep may reduce
activity the next day.<sup>14</sup> Pooling device-measured individual
participant data can overcome these limitations.<sup>13</sup>

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
eligible studies. We also searched for accelerometry consortiums,
searched trial registries (ANZCTR, WHO ICTRP, and ISRCTN) and the Open
Science Framework for unpublished results, and asked collaborators to
identify additional studies.

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
file-sharing system subject to original institutional ethics approvals
or new data transfer agreements.

## Data harmonisation and outcomes

We harmonised all accelerometer data by reprocessing raw files with the
open-source *GGIR* package (version 2.5-1) in R,<sup>19,20</sup> to
produce comparable outcomes across studies, device wear locations, and
populations. Processing used our *sleepIPD* R package, which fixed
settings across studies and documented the decisions made for specific
devices.<sup>21</sup> The core research team or the data contributors
ran *sleepIPD* on each study’s raw files. *GGIR*’s sleep detection has
been evaluated against polysomnography in both adults<sup>20</sup> and
children.<sup>22</sup> Non-wear time and abnormally high accelerations
were detected by default settings, classified as missing, and imputed
from the same time interval on the remaining days of the
week.<sup>19</sup>

Each day ran from waking to waking the following day, so each day’s
physical activity, calculated over its waking hours, was paired with the
sleep period that followed it. For wrist-worn devices, the sleep period
was detected with the HDCZA algorithm<sup>20</sup> and for hip-worn
devices *GGIR* instead detected time in bed from the device’s angle. For
sleep, *GGIR* produced total sleep time (minutes), sleep efficiency
(percentage of the sleep period, from sleep onset to final waking, spent
asleep), sleep onset (clock time), and sleep regularity (the sleep
regularity index: the likelihood of being in the same sleep or wake
state at time points 24 hours apart, with higher values indicating more
regular sleep). *GGIR* calculates the index by comparing each calendar
day with the next and, in the version we used, counts any sustained
inactivity as sleep, including daytime rest.<sup>23</sup> For physical
activity, we used two metrics proposed by Rowlands and
colleagues:<sup>24</sup> average acceleration (mg)—a measure of
volume—and the intensity gradient (unitless), the slope of log time
against log intensity, which describes how time is distributed across
intensities, with less negative values indicating relatively more time
at higher intensities. We chose these metrics because cut-points for
moderate-to-vigorous physical activity (MVPA) depend on age, device, and
wear location; those applied to our data differed more than two-fold
across groups (appendix), so minutes of MVPA are not directly comparable
across studies. Average acceleration and the intensity gradient need no
cut-points, capture light as well as vigorous activity, and separate how
much activity was done from how it was distributed across intensities.
To aid interpretation, we also analysed daily minutes of MVPA, from
*GGIR*’s age-, device-, and location-specific cut-points,<sup>21</sup>
as a secondary exposure.

## Covariates and moderators

The following individual participant data were obtained when available:
age, sex, height, weight, waist circumference, maturational status,
socioeconomic status (SES), ethnicity, screen time, sleep medications,
and medically diagnosed conditions known to affect sleep. Sex was
ascertained from the original studies, which primarily used self- or
parent-report for sex assigned at birth. The country, city, time zone,
daylight length, and season in which data were collected were also
obtained from the original studies. Because studies measured
socioeconomic status differently (e.g., education or income), we
harmonised each study’s measure into low, medium, and high categories
(see appendix).

Body mass index (BMI) was converted to an age- and sex-standardised
z-score. For participants under 18 years we used the CDC 2000
BMI-for-age growth reference, applied with the *childsds* package. For
adults we standardised BMI against the adults in this sample.

## Statistical analysis

To reduce the risk of bias, we used a blind analysis approach: we
developed and tested models on a 10% development subsample of the
participants; we conducted the final analysis once we determined the
code was ready. Data screening removed values outside fixed
physiological limits (for average acceleration, above 200 mg), all
physical activity data from files indicating device failure (76
participants), values more than 4 SD from their own study’s mean, and
biologically implausible growth values in participants under 18 years
(see appendix).

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
values on the Barnard-Rubin degrees of freedom, and used p\<0.05 to
assess statistical significance. We fitted 16 models for each moderator
and report all of them without adjustment for multiple comparisons, so p
values close to 0.05 should be interpreted with caution. We used R
(version 4.6.1) for all data cleaning and analysis.

To examine the relationship between sleep and physical activity we ran
mixed-effects models with random intercepts for study and participant,
accounting for the nesting of days within participants and of
participants within studies. We split each predictor into the
participant’s mean across their valid days (the between-person
component) and that day’s deviation from the mean (the within-person
component), and entered both, so that within-person associations were
not confounded by stable differences between people. We predicted each
sleep indicator from each physical activity indicator, adjusting for
SES, sex, and BMI z-score, with quadratic terms to allow curvilinear
associations. We interacted each component with age to examine how
associations change over the lifespan (RQ2). Age was centred at its mean
across person-days (35.6 years) and scaled per 10 years. For
interpretability, we report associations at ages 11, 18, 35, and 65
years. Models predicting physical activity from the preceding night’s
sleep (RQ3) used each sleep indicator from the previous day, so
person-days whose preceding day was absent or ineligible did not
contribute, leaving 136,319 of 168,613 eligible person-days. Because the
previous day’s sleep regularity index also spans the day being
predicted, we instead used the index for the two preceding calendar
days. Sleep regularity was missing most often, so its models with
physical activity volume used 125,155 person-days (RQ1) and 86,677
(RQ3). We also ran a cross-lagged sensitivity analysis, adjusting each
model for the outcome’s previous-day value, and tested whether next-day
activity, which cannot affect the night’s sleep, predicted each sleep
outcome (i.e., negative control; see appendix).

## Protocol deviations

This paper addresses the first two hypotheses in our registered
protocol: that physical activity volume and intensity would be
positively associated with optimum sleep duration and quality, with an
inverted U-shaped dose-response, and that associations would be stronger
in adolescents and adults than in children and older adults. Moderation
by the other pre-registered moderators is reported in the appendix, and
the remaining hypotheses will be addressed in future research. While
testing with the development subsample we made three changes to the
protocol. We modelled study as a random rather than a fixed effect, so
that estimates would generalise beyond the included studies. Sensitivity
analyses of fixed-effect models gave qualitatively similar results. We
analysed each sleep metric separately, because no validated composite
sleep indicator could be estimated from our data. We did not
log-transform skewed variables, because back-transformed predictions
followed extreme and implausibleexponential patterns near the edges of
the data range. Log-scale models are reported in the appendix.
Additionally, after running the full analysis we identified a mistake in
the BMI variable and in the imputation model, which we corrected and
reran the analysis. After data contributor review of the initial
results, we separated within- and between-person associations, which the
protocol did not specify, and realigned the sleep regularity index in
RQ3 models. The pre-registered models, which combine within- and
between-person associations, are reported in the appendix.

## Role of the funding source

None.

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

## Physical activity and that night’s sleep

On days when participants were more active than usual, they slept longer
and fell asleep earlier that night
(<a href="#tbl-sleep-outcomes" class="quarto-xref">Table 2</a>;
<a href="#fig-sleep-by-volume" class="quarto-xref">Figure 1</a>). These
within-person associations changed little with age for sleep duration
($\beta$ = 0.11 \[0.10, 0.12\] at age 11 and 0.10 \[0.08, 0.12\] at age
65) and strengthened with age for sleep onset ($\beta$ = -0.10 \[-0.11,
-0.09\] and -0.16 \[-0.17, -0.15\]). Within-person associations with
sleep efficiency were small, positive at age 11 ($\beta$ = 0.03 \[0.03,
0.04\]) and negative at age 65 ($\beta$ = -0.06 \[-0.07, -0.04\]).
Between people, habitually more active participants slept longer at age
11 ($\beta$ = 0.17 \[0.14, 0.19\]) but slightly less at age 65 ($\beta$
= -0.05 \[-0.08, -0.02\]), so the weakening with age of the association
with sleep duration in the pre-registered models (appendix) reflected
differences between people. Curvilinear terms were statistically
significant but did not meaningfully change these associations.

Activity volume was also associated with sleep regularity, within
($\beta$ = 0.18 \[0.17, 0.19\] at age 11 and 0.32 \[0.29, 0.34\] at age
65) and between people ($\beta$ = 0.41 \[0.39, 0.44\] and 0.48 \[0.43,
0.52\]). However, in the negative control, next-day activity was
associated with the night’s sleep regularity index at least as strongly
as same-day activity ($\beta$ = 0.23 \[0.22, 0.25\] *vs* 0.19 \[0.18,
0.20\]), whereas next-day associations with sleep duration (0.03 \[0.03,
0.04\] *vs* 0.10 \[0.09, 0.11\]) and onset (-0.02 \[-0.03, -0.02\] *vs*
-0.10 \[-0.11, -0.09\]) were much smaller (appendix). Days of unusually
low and unusually high activity were both associated with lower
regularity
(<a href="#fig-sleep-by-volume" class="quarto-xref">Figure 1</a>), so
the within-person association with the index cannot be attributed to
sleep.

<div class="landscape">

<div id="tbl-sleep-outcomes">

Table 2: Physical activity predicting sleep controlling for SES, sex,
and BMI z-score.

<div class="cell-output-display">

<table class="cell no-count" style="width:98%;">
<colgroup>
<col style="width: 27%" />
<col style="width: 19%" />
<col style="width: 13%" />
<col style="width: 22%" />
<col style="width: 15%" />
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
<td>-0.04 [-0.20, 0.11]</td>
<td>.592</td>
<td>-0.04 [-0.17, 0.10]</td>
<td>.609</td>
</tr>
<tr>
<td>Physical activity (within)</td>
<td>0.10 [0.09, 0.11]</td>
<td>&lt; .001</td>
<td>0.06 [0.05, 0.07]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>-0.04 [-0.06, -0.03]</td>
<td>&lt; .001</td>
<td>-0.03 [-0.04, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (within)²</td>
<td>0.01 [0.00, 0.03]</td>
<td>.113</td>
<td>0.00 [-0.01, 0.01]</td>
<td>.908</td>
</tr>
<tr>
<td>Physical activity (between)</td>
<td>0.07 [0.05, 0.09]</td>
<td>&lt; .001</td>
<td>0.11 [0.09, 0.13]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (between)²</td>
<td>0.00 [-0.01, 0.02]</td>
<td>.883</td>
<td>-0.02 [-0.03, 0.00]</td>
<td>.024</td>
</tr>
<tr>
<td>Physical activity (within) × Age</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.260</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (within)²</td>
<td>0.01 [0.00, 0.01]</td>
<td>.002</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.012</td>
</tr>
<tr>
<td>Age × Physical activity (between)</td>
<td>-0.04 [-0.05, -0.03]</td>
<td>&lt; .001</td>
<td>-0.03 [-0.04, -0.02]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (between)²</td>
<td>0.01 [0.00, 0.01]</td>
<td>.003</td>
<td>0.00 [0.00, 0.00]</td>
<td>.771</td>
</tr>
<tr>
<td colspan="5">Sleep efficiency (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.19 [0.03, 0.34]</td>
<td>.017</td>
<td>0.17 [0.00, 0.35]</td>
<td>.046</td>
</tr>
<tr>
<td>Physical activity (within)</td>
<td>-0.01 [-0.02, 0.00]</td>
<td>.164</td>
<td>0.00 [-0.01, 0.01]</td>
<td>.987</td>
</tr>
<tr>
<td>Age</td>
<td>0.01 [-0.01, 0.02]</td>
<td>.219</td>
<td>0.00 [-0.02, 0.01]</td>
<td>.705</td>
</tr>
<tr>
<td>Physical activity (within)²</td>
<td>-0.03 [-0.04, -0.02]</td>
<td>&lt; .001</td>
<td>-0.03 [-0.04, -0.02]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (between)</td>
<td>0.10 [0.08, 0.12]</td>
<td>&lt; .001</td>
<td>0.07 [0.05, 0.09]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (between)²</td>
<td>-0.04 [-0.06, -0.03]</td>
<td>&lt; .001</td>
<td>-0.05 [-0.06, -0.03]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (within) × Age</td>
<td>-0.02 [-0.02, -0.01]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (within)²</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>.019</td>
<td>0.01 [0.00, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (between)</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>.002</td>
</tr>
<tr>
<td>Age × Physical activity (between)²</td>
<td>0.00 [0.00, 0.01]</td>
<td>.630</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep onset (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.13 [-0.14, 0.40]</td>
<td>.359</td>
<td>0.16 [-0.10, 0.42]</td>
<td>.223</td>
</tr>
<tr>
<td>Physical activity (within)</td>
<td>-0.13 [-0.13, -0.12]</td>
<td>&lt; .001</td>
<td>-0.09 [-0.09, -0.08]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>0.00 [-0.01, 0.02]</td>
<td>.425</td>
<td>0.01 [0.00, 0.02]</td>
<td>.214</td>
</tr>
<tr>
<td>Physical activity (within)²</td>
<td>0.00 [-0.01, 0.01]</td>
<td>.961</td>
<td>0.00 [-0.01, 0.01]</td>
<td>.862</td>
</tr>
<tr>
<td>Physical activity (between)</td>
<td>-0.10 [-0.12, -0.09]</td>
<td>&lt; .001</td>
<td>-0.07 [-0.09, -0.05]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (between)²</td>
<td>-0.02 [-0.03, 0.00]</td>
<td>.024</td>
<td>-0.02 [-0.03, 0.00]</td>
<td>.024</td>
</tr>
<tr>
<td>Physical activity (within) × Age</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.179</td>
</tr>
<tr>
<td>Age × Physical activity (within)²</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>.005</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (between)</td>
<td>0.00 [0.00, 0.01]</td>
<td>.373</td>
<td>0.02 [0.01, 0.03]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (between)²</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.044</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep regularity (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.26 [0.12, 0.39]</td>
<td>&lt; .001</td>
<td>0.08 [-0.11, 0.27]</td>
<td>.404</td>
</tr>
<tr>
<td>Physical activity (within)</td>
<td>0.24 [0.23, 0.26]</td>
<td>&lt; .001</td>
<td>0.09 [0.08, 0.09]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age</td>
<td>0.04 [0.02, 0.06]</td>
<td>&lt; .001</td>
<td>-0.07 [-0.09, -0.05]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (within)²</td>
<td>-0.31 [-0.33, -0.28]</td>
<td>&lt; .001</td>
<td>-0.13 [-0.14, -0.12]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (between)</td>
<td>0.44 [0.41, 0.47]</td>
<td>&lt; .001</td>
<td>0.15 [0.13, 0.18]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (between)²</td>
<td>-0.19 [-0.21, -0.16]</td>
<td>&lt; .001</td>
<td>-0.13 [-0.15, -0.11]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Physical activity (within) × Age</td>
<td>0.03 [0.02, 0.03]</td>
<td>&lt; .001</td>
<td>-0.02 [-0.02, -0.02]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (within)²</td>
<td>-0.07 [-0.08, -0.06]</td>
<td>&lt; .001</td>
<td>0.03 [0.03, 0.03]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (between)</td>
<td>0.01 [0.00, 0.02]</td>
<td>.019</td>
<td>-0.07 [-0.08, -0.06]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Physical activity (between)²</td>
<td>-0.04 [-0.05, -0.03]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.01]</td>
<td>.105</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="5">Adjusted for SES, sex, and BMI z-score. Rows are grouped
by sleep outcome; columns give the physical activity predictor.
Variables marked (z) are standardised; ² denotes a quadratic term and ×
an interaction. (within) is the day’s deviation from the participant’s
mean, (between) is that mean, and (within, prior) is the previous day’s
deviation (for sleep regularity, the index for the two preceding days).
Age was centred at 35.6 years (the mean across person-days) and scaled
per 10 years, so main effects are associations at age 35.6 and age
interactions are the change per 10 years.</td>
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

Figure 1: Standardised sleep metrics predicted by within-person physical
activity volume (the day’s deviation from the participant’s mean), with
between-person volume held at the sample mean. The panels on the left
show the curvilinear relationship at the ages indicated in each column.
The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.

</div>

Within-person associations of physical activity intensity followed the
same pattern, with longer sleep ($\beta$ = 0.08 \[0.07, 0.09\] at age 11
and 0.04 \[0.03, 0.05\] at age 65) and earlier sleep onset ($\beta$ =
-0.09 \[-0.10, -0.08\] and -0.08 \[-0.09, -0.08\];
<a href="#fig-sleep-by-intensity" class="quarto-xref">Figure 2</a>).

Minutes of MVPA showed a different pattern for sleep timing (appendix).
Thirty minutes more MVPA than usual was associated with sleep onset 5
min later at age 11 ($\beta$ = 0.04 \[0.04, 0.05\]) and 4 min later at
age 65 ($\beta$ = 0.04 \[0.03, 0.04\]), and with sleep 1.8 min longer at
age 11 but 3.0 min shorter at age 65. Adjusting for the previous day’s
outcome in cross-lagged models changed the within-person estimates
little (appendix).

<div id="fig-sleep-by-intensity">

<img
src="../Figures/main/Sleep%20on%20scale_pa_intensity%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 2: Standardised sleep metrics predicted by within-person physical
activity intensity (the day’s deviation from the participant’s mean),
with between-person intensity held at the sample mean. The panels on the
left show the curvilinear relationship at the ages indicated in each
column. The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.

</div>

## Sleep and next-day physical activity

Within-person associations of a night’s sleep with next-day physical
activity volume and intensity were negligible at every age
(<a href="#tbl-pa-outcomes" class="quarto-xref">Table 3</a>;
<a href="#fig-pa-by-sleep-duration" class="quarto-xref">Figure 3</a> to
<a href="#fig-pa-by-sleep-regularity" class="quarto-xref">Figure 6</a>).
The largest was for sleep duration and next-day volume at age 11
($\beta$ = 0.03 \[0.02, 0.03\]). Sleep regularity over the two preceding
days was not associated with next-day volume ($\beta$ = -0.01 \[-0.01,
0.00\] at age 11 and -0.01 \[-0.02, 0.00\] at age 65). Between people,
those with habitually more regular sleep were more active ($\beta$ =
0.16 \[0.15, 0.18\] at age 11 and 0.18 \[0.16, 0.20\] at age 65). For
MVPA, a 1 SD earlier sleep onset than usual was followed by 3.2 min more
MVPA the next day at age 11 and 6.0 min more at age 65, and 1 SD longer
sleep by 1.0 min and 3.6 min less, respectively (appendix).

<div class="landscape">

<div id="tbl-pa-outcomes">

Table 3: Sleep predicting physical activity controlling for SES, sex,
and BMI z-score

<div class="cell-output-display">

<table class="cell no-count" style="width:98%;">
<colgroup>
<col style="width: 29%" />
<col style="width: 19%" />
<col style="width: 12%" />
<col style="width: 21%" />
<col style="width: 14%" />
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
<td>0.14 [-0.17, 0.45]</td>
<td>.379</td>
<td>-0.07 [-0.20, 0.05]</td>
<td>.252</td>
</tr>
<tr>
<td>Sleep duration (within, prior)</td>
<td>0.01 [0.01, 0.02]</td>
<td>&lt; .001</td>
<td>0.02 [0.01, 0.02]</td>
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
<td>Sleep duration (within, prior)²</td>
<td>-0.04 [-0.05, -0.04]</td>
<td>&lt; .001</td>
<td>-0.02 [-0.03, -0.02]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep duration (between)</td>
<td>0.07 [0.06, 0.08]</td>
<td>&lt; .001</td>
<td>0.06 [0.05, 0.07]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep duration (between)²</td>
<td>-0.05 [-0.06, -0.04]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, 0.00]</td>
<td>.012</td>
</tr>
<tr>
<td>Sleep duration (within, prior) × Age</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.00]</td>
<td>.010</td>
</tr>
<tr>
<td>Age × Sleep duration (within, prior)²</td>
<td>0.01 [0.00, 0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep duration (between)</td>
<td>-0.03 [-0.04, -0.03]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep duration (between)²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.885</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep efficiency (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.13 [-0.17, 0.43]</td>
<td>.392</td>
<td>-0.07 [-0.19, 0.05]</td>
<td>.252</td>
</tr>
<tr>
<td>Sleep efficiency (within, prior)</td>
<td>0.01 [0.00, 0.01]</td>
<td>.003</td>
<td>0.01 [0.00, 0.01]</td>
<td>.003</td>
</tr>
<tr>
<td>Age</td>
<td>-0.23 [-0.24, -0.22]</td>
<td>&lt; .001</td>
<td>-0.28 [-0.29, -0.27]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep efficiency (within, prior)²</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep efficiency (between)</td>
<td>-0.02 [-0.03, 0.00]</td>
<td>.008</td>
<td>0.01 [0.00, 0.02]</td>
<td>.254</td>
</tr>
<tr>
<td>Sleep efficiency (between)²</td>
<td>-0.03 [-0.04, -0.02]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep efficiency (within, prior) × Age</td>
<td>0.00 [0.00, 0.00]</td>
<td>.902</td>
<td>0.00 [-0.01, 0.00]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep efficiency (within, prior)²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.070</td>
<td>0.00 [0.00, 0.00]</td>
<td>.323</td>
</tr>
<tr>
<td>Age × Sleep efficiency (between)</td>
<td>0.02 [0.01, 0.02]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.01]</td>
<td>.016</td>
</tr>
<tr>
<td>Age × Sleep efficiency (between)²</td>
<td>0.01 [0.01, 0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.01]</td>
<td>.113</td>
</tr>
<tr>
<td colspan="5">Sleep onset (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.13 [-0.20, 0.45]</td>
<td>.444</td>
<td>-0.08 [-0.22, 0.05]</td>
<td>.222</td>
</tr>
<tr>
<td>Sleep onset (within, prior)</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.123</td>
<td>0.00 [0.00, 0.01]</td>
<td>.202</td>
</tr>
<tr>
<td>Age</td>
<td>-0.23 [-0.24, -0.21]</td>
<td>&lt; .001</td>
<td>-0.27 [-0.28, -0.26]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep onset (within, prior)²</td>
<td>-0.03 [-0.03, -0.02]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.02, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep onset (between)</td>
<td>-0.11 [-0.12, -0.09]</td>
<td>&lt; .001</td>
<td>-0.07 [-0.08, -0.06]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep onset (between)²</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>.153</td>
<td>0.01 [0.00, 0.01]</td>
<td>.105</td>
</tr>
<tr>
<td>Sleep onset (within, prior) × Age</td>
<td>0.00 [-0.01, 0.00]</td>
<td>&lt; .001</td>
<td>0.01 [0.00, 0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep onset (within, prior)²</td>
<td>0.00 [0.00, 0.01]</td>
<td>&lt; .001</td>
<td>0.00 [0.00, 0.01]</td>
<td>.001</td>
</tr>
<tr>
<td>Age × Sleep onset (between)</td>
<td>0.03 [0.02, 0.03]</td>
<td>&lt; .001</td>
<td>0.02 [0.01, 0.02]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep onset (between)²</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>&lt; .001</td>
<td>-0.01 [-0.01, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td colspan="5">Sleep regularity (z)</td>
</tr>
<tr>
<td>(Intercept)</td>
<td>0.02 [-0.27, 0.32]</td>
<td>.874</td>
<td>-0.17 [-0.30, -0.04]</td>
<td>.009</td>
</tr>
<tr>
<td>Sleep regularity (within, prior)</td>
<td>-0.01 [-0.01, 0.00]</td>
<td>.067</td>
<td>0.01 [0.00, 0.01]</td>
<td>.053</td>
</tr>
<tr>
<td>Age</td>
<td>-0.23 [-0.25, -0.21]</td>
<td>&lt; .001</td>
<td>-0.32 [-0.34, -0.31]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep regularity (within, prior)²</td>
<td>0.00 [-0.01, 0.01]</td>
<td>.643</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.251</td>
</tr>
<tr>
<td>Sleep regularity (between)</td>
<td>0.17 [0.16, 0.18]</td>
<td>&lt; .001</td>
<td>0.07 [0.06, 0.08]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep regularity (between)²</td>
<td>0.00 [-0.01, 0.01]</td>
<td>.632</td>
<td>-0.02 [-0.03, -0.01]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Sleep regularity (within, prior) × Age</td>
<td>0.00 [0.00, 0.00]</td>
<td>.878</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.002</td>
</tr>
<tr>
<td>Age × Sleep regularity (within, prior)²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.209</td>
<td>0.00 [-0.01, 0.00]</td>
<td>&lt; .001</td>
</tr>
<tr>
<td>Age × Sleep regularity (between)</td>
<td>0.00 [0.00, 0.01]</td>
<td>.108</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.251</td>
</tr>
<tr>
<td>Age × Sleep regularity (between)²</td>
<td>0.00 [0.00, 0.00]</td>
<td>.380</td>
<td>0.00 [-0.01, 0.00]</td>
<td>.003</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="5">Adjusted for SES, sex, and BMI z-score. Rows are grouped
by the previous night’s sleep predictor; columns give the physical
activity outcome. Variables marked (z) are standardised; ² denotes a
quadratic term and × an interaction. (within) is the day’s deviation
from the participant’s mean, (between) is that mean, and (within, prior)
is the previous day’s deviation (for sleep regularity, the index for the
two preceding days). Age was centred at 35.6 years (the mean across
person-days) and scaled per 10 years, so main effects are associations
at age 35.6 and age interactions are the change per 10 years.</td>
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

Figure 3: Standardised physical activity predicted by within-person
sleep duration the previous night (the deviation from the participant’s
mean), with between-person sleep duration held at the sample mean. The
panels on the left show the curvilinear relationship at the ages
indicated in each column. The panels on the right show the same
relationships but by age continuously. The white band indicates
predictions which were not significantly different from zero.

</div>

<div id="fig-pa-by-sleep-efficiency">

<img
src="../Figures/main/PA%20on%20scale_sleep_efficiency_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 4: Standardised physical activity predicted by within-person
sleep efficiency the previous night, with between-person sleep
efficiency held at the sample mean. The panels on the left show the
curvilinear relationship at the ages indicated in each column. The
panels on the right show the same relationships but by age continuously.
The white band indicates predictions which were not significantly
different from zero.

</div>

<div id="fig-pa-by-sleep-onset">

<img
src="../Figures/main/PA%20on%20scale_sleep_onset_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 5: Standardised physical activity predicted by within-person
sleep onset the previous night, with between-person sleep onset held at
the sample mean. The panels on the left show the curvilinear
relationship at the ages indicated in each column. The panels on the
right show the same relationships but by age continuously. The white
band indicates predictions which were not significantly different from
zero.

</div>

<div id="fig-pa-by-sleep-regularity">

<img
src="../Figures/main/PA%20on%20scale_sleep_regularity_lag%20by%20Age_nolog.jpg"
style="width:100.0%" />

Figure 6: Standardised physical activity predicted by within-person
sleep regularity over the two preceding calendar days, with
between-person sleep regularity held at the sample mean. The panels on
the left show the curvilinear relationship at the ages indicated in each
column. The panels on the right show the same relationships but by age
continuously. The white band indicates predictions which were not
significantly different from zero.

</div>

# Discussion

Pooling accelerometer data from 20 studies spanning the lifespan, we
found that on days when people were more active than usual, they slept
longer and fell asleep earlier that night, from childhood to older
adulthood. These associations were small. A 1 SD (23.7 mg) higher volume
than usual, about twice a typical day-to-day fluctuation (SD 11.2 mg),
was associated with 9.6 min longer sleep and 12 min earlier sleep onset
at age 11, and with 8.6 min and 20 min, respectively, at age 65. In
terms of MVPA, 30 minutes more than usual was associated with sleep
onset 4–5 min later and a change in sleep duration of no more than 3.0
min. In the reverse direction, a night’s sleep was barely associated
with next-day activity.

Our within-person findings agree in direction with meta-analyses of
experimental research showing that physical activity improves sleep,
although the associations were much weaker.<sup>7,8</sup> For example,
Kredlow and colleagues<sup>7</sup> found small ($0.22 - 0.30$) but
significant relationships between physical activity and sleep duration
and efficiency in adults using experimental studies for both acute and
regular physical activity. These effect sizes are not directly
comparable, however, as the experiments manipulated physical activity,
whereas we examined natural day-to-day variation. Accelerometry also
distinguishes quiet wakefulness from sleep less well than
polysomnography, particularly in children,<sup>22</sup> which may
attenuate associations, and the benefits of exercise for sleep are
concentrated in people whose sleep is poor at baseline,<sup>8</sup> whom
we did not target. That more MVPA than usual was associated with
slightly later sleep onset, unlike overall volume, is consistent with
evidence that strenuous exercise close to bedtime can delay
sleep.<sup>25</sup>

Among observational studies of day-to-day sleep and physical activity, a
meta-analysis of 33 studies in adults did not support a bidirectional
daily association,<sup>14</sup> and a meta-analysis of 11 studies in
children and youth found weak and inconsistent associations of physical
activity with sleep.<sup>15</sup> Two recent studies of consumer
wearables found that previous-day activity had little association with
sleep,<sup>16</sup> that shorter sleep was followed by more activity the
next day,<sup>16,17</sup> and that earlier-than-usual sleep onset was
followed by more moderate-to-vigorous activity.<sup>17</sup> Our
findings for MVPA match these previous studies: earlier and shorter
sleep than usual were followed by slightly more MVPA the next day.
Unlike the wearable studies, we found consistent associations of
activity with that night’s sleep, possibly reflecting differences in
devices, sleep algorithms, and populations. Between-person associations
were larger and changed more with age than within-person associations,
as in very old adults,<sup>26</sup> which shows why the two need to be
separated.

Because the regularity index spans two calendar days and counts daytime
rest as sleep, an unusually active or inactive day changes it regardless
of sleep, which explains why next-day activity predicted it as strongly
as same-day activity and why both extremes predicted lower regularity.
Once the index no longer overlapped the day being predicted, more
regular sleep was not followed by more activity, and the between-person
association may also partly reflect daytime rest. Because irregular
sleep predicts mortality and cardiovascular events independently of
sleep duration,<sup>27,28</sup> studies of physical activity and sleep
regularity need measures that separate sleep from daytime rest.

Within-person associations with sleep duration and onset were present
from childhood to older adulthood and varied little with age, whereas
several between-person associations, including that with sleep duration,
weakened or reversed with age. That the day-level associations were
similar in children and adults suggests that the clearer evidence from
meta-analyses in adults than in children and
adolescents<sup>6,7,10,11</sup> may reflect study design or measurement
rather than age.

Strengths of our study include device-measured sleep and physical
activity, harmonised from raw data collected with 24-hour wear protocols
across multiple studies, regions, and age groups, and the separation of
within- from between-person associations. It also has limitations.
Despite our efforts to harmonise data, variability in accelerometer wear
locations and slight differences in data collection protocols across
studies may have introduced measurement inconsistencies. Physical
activity estimates from different wear locations are strongly correlated
but agree less well in absolute terms, particularly between wrist and
hip,<sup>29</sup> and MVPA cut-points differed by age group, device, and
wear location. Restricting the sample to participants with at least
seven valid days gave similar within-person associations at age 65 but
weaker ones at age 11, for both sleep duration ($\beta$ = 0.07 \[0.06,
0.09\]) and onset ($\beta$ = -0.04 \[-0.05, -0.03\]). Within-person
estimates are free of stable differences between people but not of
factors that vary from day to day, such as illness, school or work
schedules, and weekends, so they do not establish causality. Adjusting
for the previous day’s outcome changed little, although such adjustment
can be biased when participants contribute few days. Finally,
adolescents and young adults were under-represented, which limits
generalisability to these groups and matters because young adults are
the most likely to report difficulty falling asleep.<sup>4</sup>

These findings have implications for public health recommendations.
Guidelines do not recommend physical activity to improve sleep in
children and adolescents, yet we found that people of all ages slept
slightly longer and earlier after more active days. The gains were
small, so physical activity is better presented as a consistent support
for sleep than as a remedy for poor sleep. We did not examine these
relationships specifically in individuals with sleep disorders or other
health conditions, where the effects of physical activity may differ.
Future research should test whether increasing physical activity
lengthens sleep in children and adolescents, and should measure sleep
regularity with methods that separate sleep from daytime rest.

<div class="no-count">

# Contributors

CL, TH, TS, BdPC, PP, and MN conceived and designed the study. TH
managed the data acquisition. TS managed the data harmonisation and
developed the accelerometer processing pipeline. JC, TS, and MN
conducted the statistical analyses. TH, TS, JC, and MN had direct access
to and verified the underlying data. TH, TS, BdPC, MN, PP, JC, and CL
drafted the manuscript. All co-authors not individually listed,
contributed data from their original studies. All authors contributed to
the interpretation of results and critically revised the manuscript for
important intellectual content. All authors approved the final version
of the manuscript and agree to be accountable for all aspects of the
work.

# Declaration of interests

We declare no competing interests.

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

<div id="ref-czeislerComparisonSleepRegularity2026" class="csl-entry">

<span class="csl-left-margin">23
</span><span class="csl-right-inline">Czeisler MÉ, Leota J, Le F, *et
al.* [Comparison of Sleep Regularity Index scores calculated by
open-source packages and implications for outcomes research: Rationale
and design of the RIRI statement (Reporting Items for Regularity
Indices)](https://doi.org/10.1093/sleep/zsaf299). *Sleep* 2026; **49**:
zsaf299.</span>

</div>

<div id="ref-rowlandsMovingForwardAccelerometerAssessed2018"
class="csl-entry">

<span class="csl-left-margin">24
</span><span class="csl-right-inline">Rowlands AV. [Moving Forward With
Accelerometer-Assessed Physical Activity: Two Strategies to Ensure
Meaningful, Interpretable, and Comparable
Measures](https://doi.org/10.1123/pes.2018-0201). *Pediatr Exerc Sci*
2018; **30**: 450–6.</span>

</div>

<div id="ref-leotaDoseresponseRelationshipEvening2025"
class="csl-entry">

<span class="csl-left-margin">25
</span><span class="csl-right-inline">Leota J, Presby DM, Le F, *et al.*
[Dose-response relationship between evening exercise and
sleep](https://doi.org/10.1038/s41467-025-58271-x). *Nat Commun* 2025;
**16**: 3297.</span>

</div>

<div id="ref-phamBidirectionalAssociationPhysical2025"
class="csl-entry">

<span class="csl-left-margin">26
</span><span class="csl-right-inline">Pham H, Svensson T, Chung U,
Svensson AK. [Bidirectional association between physical activity and
sleep in healthy Japanese super-seniors: The Japan Healthy Aging Study
(J-HAS)](https://doi.org/10.1038/s41514-025-00223-2). *npj Aging* 2025;
**11**: 36.</span>

</div>

<div id="ref-windredSleepRegularityStronger2024" class="csl-entry">

<span class="csl-left-margin">27
</span><span class="csl-right-inline">Windred DP, Burns AC, Lane JM, *et
al.* [Sleep regularity is a stronger predictor of mortality risk than
sleep duration: A prospective cohort
study](https://doi.org/10.1093/sleep/zsad253). *SLEEP* 2024; **47**:
zsad253.</span>

</div>

<div id="ref-chaputSleepRegularityMajor2025" class="csl-entry">

<span class="csl-left-margin">28
</span><span class="csl-right-inline">Chaput J-P, Biswas RK, Ahmadi M,
*et al.* [Sleep regularity and major adverse cardiovascular events: A
device-based prospective study in 72 269 UK
adults](https://doi.org/10.1136/jech-2024-222795). *J Epidemiol
Community Health* 2025; **79**: 257–64.</span>

</div>

<div id="ref-zisouHarmonisedAccelerometerderivedPhysical2026"
class="csl-entry">

<span class="csl-left-margin">29
</span><span class="csl-right-inline">Zisou C, Maylor BD, Acquah A, *et
al.* Towards harmonised accelerometer-derived physical activity: A
comparability study across wrist, thigh, and hip wear locations. *Int J
Behav Nutr Phys Act* 2026; published online Sept.
DOI:[10.1186/s12966-026-01975-0](https://doi.org/10.1186/s12966-026-01975-0).</span>

</div>

</div>
