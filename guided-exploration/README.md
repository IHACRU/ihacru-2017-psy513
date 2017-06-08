Practical component guide.  
=========
 
# Table of Contents
- [GE1](#ge1) – Location Mapping: EHR Addresses and Class Compressors  
- [GE2](#ge2) – Patient Event Table: Sequencing encounters with the Service System
- [GE3](#ge3) – Mapped Encounters: Cohort Summary and Timeline Visualization  
- _[Lab1](#lab1) – Cohort Summary and Timeline Visualization_  
- GE4 – Flattened Encounter Timelines: Quantifying Service Engagement  
- GE5 – Statistical Models for Evaluating Encounter Profiles  
- _[Lab2](#lab2) – Patterns of Service Utilization_  


The practical component of the course will consist of 5 guided explorations (GE) and 2 labs. 
A guided exploration is walking through an R script that performs a particular set manipulations and analyses of data relevant for that topic. The learners are expected to demonstrate the reproduction of these scripts using prosthetic data on their machines and contribute to script development.  Each lab applies the skills and knowledge from preceding guided explorations and focuses on organizing and publishing a dynamic report that carries the argument of the analysis by providing the language of discourse and demonstrating the evidence for purported conclusions.  The documents created in the labs will be submitted for grading and evaluation. While feedback will be provided on each report, they will be graded on pass/fail scale and contribute to 30% of their course grade. 
 
**Prerequisites**: basic familiarity with R. While we do not exclude other statistical packages, the primary data handling, modeling, and graphing will be written out in R scripts. Student engagement with the data and models will take place via RStudio. Students will be expected to learn reading and applying existing R scripts to perform data manipulation, model estimation, and publishing results as dynamic documents.  
Each of the GE will be related to the following rubrics: 
- **Overview**: What happens at this stage of analytic work flow?
- **Questions**: what questions should you be able to answer?
- **Skills**: what analytic or technical skills does it require?
- **Learning Objectives**: what specific knowledge or skill should be acquired?
- **Glossary**: what terms and concepts are central to this stage?
 
 
# GE1
## Location Mapping: EHR Addresses and Class Compressors  
### Overview. 
We discuss how VIHA organizes its EHR and how ACRU’s cross-continuum classification system (aka “semantic layer”) prepares the clinical encounter data for general-purpose analysis. 
### Script
[`./manipulation/ge1.R`](ge1.R)
### Questions
- 1.1 What is a program of health service? How is it defined within Island Health? 
- 1.2 How does CERNER Millennium identify a health program?
- 1.3 How does VIHA’s Data Warehouse (DW) describe a health program?
- 1.4 Why can’t we use the encounter data for research as is? Why do we need a `semantic layer`?
- 1.5 What is a `compressor` and what is its function?
- 1.6 What are properties and features of each of the compressors? 
- 1.7 What is a `service class` and how is it useful?
- 1.8 What is a `research palette` and what is its role in communication between external researchers and Island Health data curators?
- 1.9 How is `service class ` different from a `palette color`?
- 1.10 How do I know that this palette is good for my purposes?

### Skills
- setting up the software environment for data science project (R + RStudio + GitHub)
- inputing location map data
- transforming variables
- producing aggregate views of frequencies

### Learning Objectives
- gain operational knowledge of assembling a research cohort from EHR using Cross-Continuum Classification (3C) system developed by ACRU. 
- by the end of the period, the learner must be able to explore marginal frequencies of the VIHA's _Location Map_ using command idiom of the `dplyr` package

### Glossary 

- _EHR address_ – a set of administrative descriptors that uniquely identify a program of health services in an EHR.
- _Compressor_ – aka. Classifier. A discrete variable spanning all possible values of dimension/feature of interest (e.g. Population_Age, Clinical_Focus, etc) that we consider useful to know about a program. 
- [_Star Schema_](../libs/images/star-schema-3t.png) - representation of how tables are related in the SQL query that returns the data

# GE2
## Patient Event Table: Sequencing encounters with the Service System  
### Overview. 
We discuss how EHR is queried to obtain the list of clinical events which constitute the medical histories of patients. We take a look at data sources available within Island Health and how they are combined to provide the data structure we will work with. We describe the useful indicators ERH may provide and how we can use them for research. 

### Script
[`./manipulation/ge2.R`](ge2.R)
### Questions
- 2.1 How does patient event table get assembled?
- 2.2 How is a research cohort can defined? 
- 2.3 What is the process by which a researcher obtains encounter data? Who is involved?
- 2.4 What information about patient comes in a "standard" patient event table?

### Skills
- groupped summarizations using `dplyr`

### Learning Objectives
- be able to explaine how unique ehr addresses become palette colours 
- know what kind of variables are supplied with in a standard patient event tabele



# GE3
## Mapped Encounters: Cohort Summary and Timeline Visualization  
### Overview. 
We connect patient histories to the semantic layer describing the properties of service programs.

### Script
[`./manipulation/ge3.R`](ge3.R)
### Questions
- 3.1 How can patients’ histories be visualized? What are the ways to look at them?
- 3.2 High-contrast, low-frequency vs low-contrast, high-frequency events. What is significance of each?
- 3.3. How does encounter relate to other units of analysis: events and episodes?

### Skills
- scatter plots with ggplot2
- joining dataframes using `dplyr` joins

### Learning Objectives
- understand the `dplyr` idioms in the custom function `unique_sums`
- produce a basic graph of medical history of a single patient

# Table of Contents
- [GE1](#ge1) – Location Mapping: EHR Addresses and Class Compressors  
- [GE2](#ge2) – Patient Event Table: Sequencing encounters with the Service System
- [GE3](#ge3) – Mapped Encounters: Cohort Summary and Timeline Visualization  
- _[Lab1](#lab1) – Cohort Summary and Timeline Visualization_  
- GE4 – Flattened Encounter Timelines: Quantifying Service Engagement  
- GE5 – Statistical Models for Evaluating Encounter Profiles  
- _[Lab2](#lab2) – Patterns of Service Utilization_  
