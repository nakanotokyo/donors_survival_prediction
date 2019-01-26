# Survival time prediction : Donors churn in a non-profit in Latin America

## The project 

### Intro
Random Survival Forest algorithm (randomForestSRC Library) applied to predict donors attrition in a non-profit. This is an on-going project.

### Problem
A non-profit collects and process donations for other non-profits. During 2018 4% of donation were cancelled, the non-profit looks forward to anticipate donors attrition in order to communicate with donors to avoid the cancellation. 

### Data

All the transactions are recoreded in a relational data base and contains data of the donors, donations, approved and rejected donations, payment methods, non-profits that receive the donation and data from google analytics. 

Data is not public but a sample of the dataset will be added soon under owner's permission. 

### Model

A Random Survival Forest algorithm is applied to predict survival curve for each donor and an ensamble-mortality score is used to rank donors and select the ones to target with a marketing campaign to avoid donation cancellation. 

## The repository

The repository is organized as follows:

*preprocess*: 

- Sql queries needed to generate the input dataset
- Code to geolocate donors and non-profits and distance calculation
- Code to preprocess text to normalize "occupation" feature that is a blank field in the donation form

*model:*
- R markdown notebook used to train a Random Survival Forest algorithm

*result*:
- csv files with test dataset output

![process](https://user-images.githubusercontent.com/23097609/51429942-036d2c00-1bf3-11e9-8a4f-04fc99ebf45f.png)
