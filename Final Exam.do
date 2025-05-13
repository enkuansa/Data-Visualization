* Final Exam Do File
* Economics 411
* Spring 2025
* Name: Elias Nkuansambo

import delimited https://raw.githubusercontent.com/flynnecon/forecasting2025/refs/heads/main/final/wti_exam.csv, clear


gen sdate = tm(1986m1) + _n-1
format sdate %tm
tsset sdate

/*
** The Thinking Questions

1. (10points) Explain the differences in evaluation performed when you use an information criterion (AIC,AICc,BIC) and mean absolute percent error(MAPE).

Information criterias are used to judge between models based on the best trade-offs between fittness and complexity levels, penalizing the models with many more parameters. On the other hand, the MAPE is used to describe the average difference between the forecasted values and the actual values, hence it only give information about the "fitness" of the model, not level of complexity.
We can see this in the formulas for both the information criterias and the formula of the MAPE. The information criterias contain information about the dispersion of the data (variance) and also the number of variables used in the model. On the other hand, the MAPE does not have information about the number of parameters so as to judge the level of complexity of models.





2.  (10points) In class we talked about training datasets and testing datasets. Discuss how you would determine the proper division of your data into these two groups generally. What would you recommend is done, or perhaps different from your general approach, with the incidence of a sharp shock to the system,such as seen with data in class and the COVID-19 pandemic.

There are a few ways I could go about it:
* One way would be to find a spot in the data where there is a structurak break or where the shock begins and apply the split there. This way I can forecast what would the variable in case be if the shock or structural breaak had not happen, and then contrast it with the value sin the testing window and evaluate performance.
* Another way, would be to only use the dataset after the shock and split it any how so  long as thetraining set has reasonably sufficient data to perform a reliable forecast.(Usually, I would go for 70% for the train set and 30% for the test set) This way I would forecast how well is the variable in question performing given that the shock has happened.

Thus it would highly depend on the goals I am looking to achieve.




3. (10 points) Describe the differences between an exponential smoothing approach to forecasting and the use of ARIMA for forecasting and discuss the situation(s) or circumstance(s) where you would prefer each approach.

Exponential smoothing assumes... while ARIMA uses lagged data and moving averages to perform its calculatons.



*/

* The Doing Questions


** 4a. (5 points) Run and interpret the KPSS test for the level value of the wti variable.
kpss wti
tsline wti
/*
The kpss test shows that up to 17 lags, the data wti is not stationary, as the test-statistics of all lags are greater than the critical values. 

*/

** 4b. (5 points) Run and interpret the KPPS test for the first-difference of the wti variable.
gen d_wti = D.wti
kpss d_wti
tsline d_wti

/*
The kpss test shows that the test-statistics of all lags are smaller than the critical values, even at 10% signifcant level. Thus, we fail to reject the null hypothesis (H0: d_wti is trend stationary), and must conclude that the differentiated variable `d_wti` is stationary.

*/

** 4c. (5 points) Run and interpret the KPSS test for the monthly percentage change of the wti variable.
gen pwti=[(wti - L.wti)/L.wti]*100
kpss pwti

/*
The kpss test shows that the test-statistics of all lags are smaller than the critical values, even at 10% signifcant level. Thus, we fail to reject the null hypothesis (H0: pwti is trend stationary), and must conclude that the variable `pwti` is stationary.

*/

** 5  (5 points) In the software package using the first differenced data create a training data set going from January 2021 to December 2023, inclusive. Also create a testing data set from January 2024 to March 2025 (the end of the data set). Create a time series graph of the two data sets.

drop if sdate < tm(2021m1) // Limit the dataset to training + testing sets only.

gen usable_set = d_wti if sdate >= tm(2021m1)
gen dwti_train = usable_set if sdate <= tm(2023m12)
gen dwti_test = usable_set if sdate > tm(2023m12)
tsline dwti_train dwti_test if sdate >= tm(2021m1), xtitle("Time") ytitle("Price($)") title("Training & Testing Datasets")


** 6. Using the training/testing divide to answer this question. Run each of the following baseline methods over the testing window and calculate mean error, mean absolute error, and mean absolute percent error.


gen obs = _n
replace obs =. if sdate > tm(2023m12)
qui summarize(obs)
scalar end = r(max)
 

** (a) (5 points) Mean forecast
egen fcstmean = mean(dwti_train) if sdate >= tm(2021m1)
replace fcstmean = dwti_train if sdate <= tm(2023m12)
tsline fcstmean dwti_test if sdate >= tm(2021m1), saving(fcstmean, replace) 
tsline fcstmean dwti_test if sdate > tm(2023m12), saving(fcstmean1, replace) 
graph combine fcstmean.gph fcstmean1.gph, title("Mean Forecast")

gen fcerr_mean = dwti_test - fcstmean if obs > scalar(end) // Errors
gen absfcerr_mean = abs(fcerr_mean) // Mean Absolute Errors
gen apefcerr_mean = 100*absfcerr_mean/abs(dwti_test) // Mean Absolute Percentage Errors
sum fcerr_mean absfcerr_mean apefcerr_mean // summary table



** (b) (5 points) Naive forecast
gen fcstnaive = dwti_train if sdate >= tm(2021m1)
replace fcstnaive = dwti_train[scalar(end)] if sdate > tm(2023m12)
tsline fcstnaive dwti_test if sdate >= tm(2021m1), saving(fcstnaive, replace) 
tsline fcstnaive dwti_test if sdate > tm(2023m12), saving(fcstnaive1, replace) 
graph combine fcstnaive.gph fcstnaive1.gph, title("Naive Forecast")

gen fcerr_naive = dwti_test - fcstnaive if obs > scalar(end) // Errors
gen absfcerr_naive = abs(fcerr_mean) // Mean Absolute Errors
gen apefcerr_naive = 100*absfcerr_mean/abs(dwti_test) // Mean Absolute Percentage Errors
sum fcerr_naive absfcerr_naive apefcerr_naive // summary table


** (c) (5 points) Drift forecast
obs > obs[scalar(end)]
gen fcstdrift = dwti_train
replace fcstdrift = fcstdrift[_n-1] + ((dwti_train[scalar(end)] - dwti_train[1])/(obs[scalar(end)]-obs[1])) if fcstdrift==.
tsline fcstdrift dwti_test, saving(fcstdrift, replace) 
tsline fcstdrift dwti_test if sdate > tm(2023m12), saving(fcstdrift1, replace)
graph combine fcstdrift.gph fcstdrift1.gph, title("Drift Forecast")

  
gen fcerr_drift = dwti_test - fcstdrift if obs > scalar(end) // Errors
gen absfcerr_drift = abs(fcerr_mean) // Mean Absolute Errors
gen apefcerr_drift = 100*absfcerr_mean/abs(dwti_test) // Mean Absolute Percentage Errors
sum fcerr_drift absfcerr_drift apefcerr_drift // summary table


** (d) (10 points) In your opinion which method is the most accurate and why?
sum fcerr_mean absfcerr_mean apefcerr_mean fcerr_naive absfcerr_naive apefcerr_naive fcerr_drift absfcerr_drift apefcerr_drift // Complete summary table




** 7. Over the training window run each of the following models and pick which one performs best. Then forecast over the training window and provide a graph of the forecast Using your preferred software package do each of the following and report results as appropriate over the training window:

** (a) (5 points) ARIMA(1 0 0)
arima dwti_train, arima(1 0 0)
estimates store model_a
estat ic

** (b) (5 points) ARIMA(2 0 0)
arima dwti_train, arima(2 0 0)
estimates store model_b
estat ic

** (c) (5 points) ARIMA(2 0 1)
arima dwti_train, arima(2 0 1)
estimates store model_c
estat ic

** (d) (5 points) Which of these models performs the best? Explain why you make that choice?
esttab model_a , stats(ic, labels("Information Criteria(AIC and BIC)")) se star(* 0.05 ** 0.01 *** 0.001) title("Regression Results")
display model_a
/*
	Model A (the very first model: ARIMA(1 0 0)) performs the best as it has the lowest AIC and BIC values.
*/


** (e) (5 points) Create a graph of the best model forecast over the testing window along with the actual values and discuss key features of the graph.

estimates restore model_a
//predict fcst_model_a
//predict fcst_model_a1, y
predict fcst_model_a2, y dynamic(tm(2023m12))

tsline usable_set fcst_model_a2, tline(2024m1 2025m3) saving(modelp, replace) ytitle("Price($)") xtitle("Time")
tsline usable_set fcst_model_a2 if sdate > tm(2023m12), saving(modelp1, replace) ytitle("Price($)") xtitle("Time")
graph combine modelp.gph modelp1.gph, title("Model_A Forecast")

/*
preserve
drop if sdate>tm(2023m12)
tsline usable_set fcst_model_a fcst_model_a1 fcst_model_a2 if sdate<=tm(2025m3), saving(model_az, replace)
tsline usable_set fcst_model_a if sdate<=tm(2025m3), saving(model_az1, replace)
tsline usable_set fcst_model_a1 if sdate<=tm(2025m3), saving(model_az2, replace)
tsline usable_set fcst_model_a2 if sdate<=tm(2025m3), saving(model_az3, replace)
restore
graph combine model_az.gph model_az2.gph model_az3.gph

*/

/*
	Model A (the very first model: ARIMA(1 0 0)) performs the best as it has the lowest AIC and BIC values.
*/

** 8. (Bonus points) Forecast oil prices from above methods for the remainder 2025 and report the results in a table.
tsappend, add(9)

estimates restore model_a
predict fcst_model_a
label variable fcst_model_a "model_a prediction"

estimates restore model_b
predict fcst_model_b
label variable fcst_model_b "model_b prediction"

estimates restore model_c
predict fcst_model_c
label variable fcst_model_c "model_c prediction"

tsline usable_set fcst_model_a fcst_model_b fcst_model_c, tline(2024m1  2025m3 2025m12) title("One step ahead foreacast type") 
drop



/* Just for fun
predict fcst_model_a1, y
predict fcst_model_a2, y dynamic(tm(2023m12))
predict fcst_model_a3, y dynamic(tm(2025m3))


tsline usable_set fcst_model_a fcst_model_a1 fcst_model_a2, saving(model_a, replace)
tsline usable_set fcst_model_a fcst_model_a1 fcst_model_a2 fcst_model_a3 if sdate>=tm(2025m3), saving(model_az, replace)


estimates restore model_b
predict fcst_model_b
predict fcst_model_b1, y
predict fcst_model_b2, y dynamic(tm(2023m12))
tsline usable_set fcst_model_b fcst_model_b1 fcst_model_b2, saving(model_b, replace)
tsline usable_set fcst_model_b fcst_model_b1 fcst_model_b2 if sdate>=tm(2025m3), saving(model_bz, replace)

estimates restore model_c
predict fcst_model_c
predict fcst_model_c1, y
predict fcst_model_c2, y dynamic(tm(2023m12))
tsline usable_set fcst_model_c fcst_model_c1 fcst_model_c2, saving(model_c, replace)
tsline usable_set fcst_model_c fcst_model_c1 fcst_model_c2 if sdate>=tm(2025m3), saving(model_cz, replace)

tsline usable_set fcst_model_c1 , saving(model_c1, replace)
tsline usable_set fcst_model_c2, saving(model_c2, replace)
tsline usable_set fcst_model_c1 fcst_model_c2 if sdate>=tm(2025m3), saving(model_c2a, replace)

tsline usable_set fcst_model_a fcst_model_a1 fcst_model_a2 fcst_model_b fcst_model_b1 fcst_model_b2 fcst_model_c fcst_model_c1 fcst_model_c2, saving(models, replace)
tsline usable_set fcst_model_a fcst_model_a1 fcst_model_a2 fcst_model_b fcst_model_b1 fcst_model_b2 fcst_model_c fcst_model_c1 fcst_model_c2 if sdate>=tm(2025m3), saving(models_z, replace)


graph combine model_az.gph model_bz.gph model_cz.gph
graph combine model_a.gph model_b.gph model_c.gph
graph combine models.gph models_z.gph

*/















