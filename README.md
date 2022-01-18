# SimIndLangCan

Estimation of the future number of speakers among 50+ indigenous languages of Canada using simulation.

I decided to create a new repository for our work as I created the previous one without any knowledge of GitHub and its best practices. Hopefully we can create a more systematic workflow using this repository. I also decided to split the work into different scripts. This way we can jump directly into the part that we are interested in. The scripts are:

1.Fertility. Fits parametric models to data from the UN World Population Prospects.
2.Mortality. Same as 1. but with mortality.
3.StartingPop. Estimates language-specific distributions by age based on census data provided by Statistics Canada.
4.IntergenTrans. Estimates rates of intergenerational transmission based on the distributions by age.
5.Simulations. Runs a microsimulation model using the fertility and mortality rates, distributions by age, and intergenerational transmission rates.
6.Results. Displays simulation results.