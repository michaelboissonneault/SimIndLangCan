# SimIndLangCan

Estimation of the future number of speakers among 50+ indigenous languages of Canada using simulation.

I decided to create a new repository for our work as I created the previous one without any knowledge of GitHub. Hopefully we can create a more systematic workflow using this repository. I also decided to split the work into different do files. This way we can jump directly into the part that we are interested in. 

The do files are:

1.Fertility. Fits parametric models to data from the UN World Population Prospects.
2.Mortality. Same as 1. but with mortality.
3.StartingPop. Estimates language-specific distributions by age based on census data provided by Statistics Canada.
4.IntergenTrans. Estimates rates of intergenerational transmission based on the distributions by age.
5.Simulations. Runs a microsimulation model using the fertility and mortality rates, distributions by age, and intergenerational transmission rates.
6.Results. Displays simulation results.

The do files call different data files. Those are:

1121322... .CSV: data on speaker numbers, year 2016
1032512... .CSV: data on speaker numbers, year 2011
UNMortalityUpperMiddleIncome.csv: Age-specific mortality rates to the year 2100 (projection), United Nations Population prospects, Upper Middle Income countries. 
UNMortalityLowerMiddleIncome.csv: Same but for Lower Middle income countries.
UNFertilityLowerMIddleIncome.csv: Age-specific fertility rates to the year 2100 (projection), United Nations Population prospects, Lower Middle Income countries.