pmsampsize v1.1.3 (Release date: 2023-12-05)
=============================================
* added options to specify the cox-snell or nagelkerke's r-squared directly for binary outcome models
* simplified continuous outcome models criteria 3 calculation fixing MMOE=10% so that N=234+p

pmsampsize v1.1.2 (Release date: 2022-02-12)
=============================================
* fixes issue for continuous outcome models in criteria 4 in calculating the CI for the intercept based on final SS
* edit to criteria 2 for continuous outcomes to round up N

pmsampsize v1.1.1 (Release date: 2021-11-21)
=============================================
* fixes issues with rounding leading to small differences in Max_Rsq and subsequently sample size calculations
* additional output including Nagelkerke's R-sq (binary & survival outcomes)

pmsampsize v1.1.0 (Release date: 2021-06-24)
=============================================
* update allowing user to input C-statistic from existing model to approximate Cox-Snell R-squared (for binary outcome model sample size calculations) 
* additional parameter checks	

pmsampsize v1.0.4 (Release date: 2020-07-24)
=============================================
* version issues fixes

pmsampsize v1.0.3 (Release date: 2020-07-23)
=============================================

Continuous outcomes:

* calculation of shrinkage for each criteria based on N needed in specific criteria

* also allowance for the unique scenario when starting N already meets shrinkage requirement set by user (this is more for academic exercise, as in practice a high shrinkage will be desired and so it is very rare that the base N (set to p+2) will provide good shrinkage)

Binary and TTE outcomes:

* more informative error message added for rare scenario where shrinkage is specified lower than r-squared which results in NaNs from shrinkage formula 

pmsampsize v1.0.2 (Release date: 2019-11-15)
=============================================

* update forces the time-unit to be the mean follow-up time, as described in forthcoming BMJ paper which extends the original Statistics in Medicine paper (see reference in help file)

* updated in calculation of max R-sq for survival option, to prevent manipulation based on time units used 


pmsampsize v1.0.1 (Release date: 2019-08-02)
=============================================

* updated in line with correction to original article in eq. 13


pmsampsize v1.0.0 (Release date: 2019-01-02)
=============================================

* this is a new release

