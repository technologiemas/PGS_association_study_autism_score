This folder contains the files for conducting measurement invariance analyses.
The script "generate_item_data.R" extracts item-level data from the main dataset and prepares it for analysis.
The script extract_data_from_models.R compiles the results from the Mplus output files into a summary CSV file.
Inside the folders, the scripts ending in .inp contain the input files for Mplus to run the measurement invariance models.

Different levels of strictness in measurement invariance can be tested using the following models:
- Configural Invariance: This model tests whether the same factor structure holds across groups without any equality constraints on parameters.
- Metric (or weak) Invariance: This model tests whether factor loadings are equal across groups, indicating that the constructs are measured in the same way.
- Strong (or scalar) Invariance: This model tests whether both factor loadings and item intercepts are equal across groups, allowing for meaningful comparisons of latent means.
- Strict Invariance: This model tests whether factor loadings, item intercepts, and residual variances are equal across groups, indicating that the measurement properties are identical.

Once strong invariance is established, latent mean comparisons can be conducted using full invariance in the "05_full....inp" file.

The output files .out contain the results of the measurement invariance analyses, which can be reviewed to assess the fit of each model and determine the level of invariance achieved. Each level is checked against the previous level using DIFFTEST which provides a p-value to determine if the additional constraints significantly worsen model fit.

# References:
- van der Sluis, S., Polderman, T. J. C., Neale, M. C., Verhulst, F. C., Posthuma, D., and Dieleman, G. C. (2017) Sex differences and gender-invariance of mother-reported childhood problem behavior. Int J Methods Psychiatr Res, 26: e1498. doi: 10.1002/mpr.1498.
- Millsap, R. E., & Yun-Tein, J. (2004). Assessing Factorial Invariance in Ordered-Categorical Measures. Multivariate Behavioral Research, 39(3), 479–515. https://doi.org/10.1207/S15327906MBR3903_4
