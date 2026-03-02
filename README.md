This readme file was generated on [2026-01-28] by [Tyler Barrett]

-------------------
GENERAL INFORMATION
-------------------

Code and data dictionary from: Characterizing human mobility patterns and their association with SARS-CoV-2 infection across a gradient of market integration

Author Contact Information

	Lead Author: Tyler M. Barrett
	Institution: Duke University
	Email: tyler.barrett@duke.edu
	ORCID: 0000-0001-8166-8704

	Principal Investigator: Charles L. Nunn
	Institution: Duke University
	Email: clnunn@duke.edu
	ORCID: 0000-0001-9330-2873

Date of data collection: 20221103-20250105

Geographic location of data collection: SAVA, Madagascar 

Funding and grant numbers:
	Funding was provided by the joint NIH-NSF-NIFA Ecology and Evolution of Infectious Disease Program (DEB-2308460), an NSF Doctoral Dissertation Research Improvement Grant (BCS-2341234), and the Center for Population Health and Aging (CPHA) at Duke University and its NIA Center Grant (P30 AG034424).

--------------------
DATA & FILE OVERVIEW
--------------------
The data that support the findings of this study are available on request from the corresponding author. The data are not publicly available due to privacy or ethical restrictions.

List of Data Files:
	1. survey_infection_data.csv: survey and SARS-CoV-2 infection data to replicate analyses in the paper
	2. commune_edgelist.csv: commune-level network edge data to replicate travel network figure in the paper
	3. commune_nodelist.csv: commune-level network node data to replicate travel network figure in the paper
	4. travel_similarity_edgelist.csv: person-level edge data reflecting shared travel patterns

List of Code Files:
	1. replicate_results_figures.R: R code to replicate figures and statistical models in the results section of the paper

--------------------------
METHODOLOGICAL INFORMATION
--------------------------

A detailed description of the methods used to collect and generate the data are provided in the associated paper.

--------------------------
DATA-SPECIFIC INFORMATION 
--------------------------

Data dictionary for survey_infection_data.csv

| Variable Name | Description |
|----------|----------|
| participant_id    | unique participant identifier     |
| age    | age in years     |
| gender    | gender, male/female     |
| school_level    | highest level of formal education, primary/secondary/higher     |
| main_activity    | primary income-generating activity, farmer/non-farmer     |
| household_under_3   | number of children in household under three years old     |
| village    | numeric identifier for study village     |
| house_sol    | house lifestyle index, higher values indicate greater market integration, see paper for full description     |
| durable_goods_owned    | count of durable goods owned, ranging from 0 (no goods) to 7 (all goods)     |
| sell_avg    | average percentage of crops sold (versus consumed at home)     |
| travel_frequency    | total number of trips in the past year    |
| travel_distance_km    | total distance traveled in the past year, kilometers     |
| pct_econ_trips    | percentage of trips for economic reasons (work, sell/buy things)     |
| pct_family_friends_trips    | percentage of trips to visit family and friends     |
| pct_children_school_trips    | percentage of trips to take children to school     |
| pct_health_trips    | percentage of trips to healthcare facilities     |
| pct_other_trips    | percentage of trips for other reasons     |
| pct_walk_trips    | percentage of trips by foot    |
| pct_bicycle_trips    | percentage of trips by bicycle     |
| pct_tuktuk_trips    | percentage of trips by tuktuk     |
| pct_taxi_car_trips    | percentage of trips by taxi car     |
| pct_taxi_brousse_trips    | percentage of trips by taxi brousse     |
| pct_motorcycle_scooter_trips    | percentage of trips by motorcycle or scooter     |
| covid_travel_change    | reduction in travel due to COVID-19, 0 (no)/1 (yes)     |
| vaccinated_covid    | self-reported COVID-19 vaccination status, not vaccinated/vaccinated/unknown     |
| XBB1.5    | variant infection status, 0 (not infected)/1 (infected)     |
| Beta    | variant infection status, 0 (not infected)/1 (infected)     |
| XBB    | variant infection status, 0 (not infected)/1 (infected)     |
| BA.2    | variant infection status, 0 (not infected)/1 (infected)     |
| Alpha    | variant infection status, 0 (not infected)/1 (infected)     |
| Delta    | variant infection status, 0 (not infected)/1 (infected)     |
| Gamma    | variant infection status, 0 (not infected)/1 (infected)     |
| BA.5    | variant infection status, 0 (not infected)/1 (infected)     |
| SARS-CoV-2    | ancestral variant infection status, 0 (not infected)/1 (infected)     |
| XBB1.16    | variant infection status, 0 (not infected)/1 (infected)     |
| JN.1    | variant infection status, 0 (not infected)/1 (infected)     |
| XDV    | variant infection status, 0 (not infected)/1 (infected)     |
| EG.5    | variant infection status, 0 (not infected)/1 (infected)     |
| Delta Plus    | variant infection status, 0 (not infected)/1 (infected)     |
| BA.1    | variant infection status, 0 (not infected)/1 (infected)     |
| Lambda    | variant infection status, 0 (not infected)/1 (infected)     |
| Mu    | variant infection status, 0 (not infected)/1 (infected)     |

Data dictionary for commune_edgelist.csv

| Variable Name | Description |
|----------|----------|
| home_commune   | origin commune     |
| visited_commune    | destination commune     |
| weight    | number of trips from origin to destination     |
| travel_reason_family_friends    | percentage of trips to visit family and friends     |
| travel_reason_econ    | percentage of trips for economic reasons (work, buy/sell things)     |
| travel_reason_children_school    | percentage of trips to take children to school     |
| travel_reason_health    | percentage of trips to healthcare facilities     |
| travel_reason_other    | percentage of trips for other reasons     |

Data dictionary for commune_nodelist.csv

| Variable Name | Description |
|----------|----------|
| commune   | commune name     |
| long   | longitude of commune for mapping     |
| lat   | latitude of commune for mapping     |
| district   | district that the commune is in, Sambava/Andapa/Vohemar/Antalaha     |

Data dictionary for travel_similarity_edgelist.csv

| Variable Name | Description |
|----------|----------|
| from   | unique ego participant identifier     |
| to   | unique alter participant identifier     |
| weight   | number of shared travel destinations     |
   
Missing data indicated as NA