
# Enter the location of datasheet.
dir_in = "…/MEPS_data.csv";
# Enter the location of data analysis results.
dir_out = "…/output";

now = Sys.time();
folder_name = paste0(format(now, "%Y%m%d_%H%M%S_"),"output");
result_name_txt = paste0(format(now, "%Y%m%d_%H%M%S_"),"output.txt");
result_name_csv = paste0(format(now, "%Y%m%d_%H%M%S_"),"output.csv");
report_name = paste0(format(now, "%Y%m%d_%H%M%S_"),"Diagnostics_Report.doc");
# Entire dataset used.
K_1 = 1;
# VSURF method.
K_2 = 1;
# Tree-fit.
K_3 = 1;
# Tree-fit.
K_4 = 1;
# Number of factors.
K = 1;

# Create a new folder for output.
if (! file.exists(dir_out)){
dir.create(dir_out, showWarnings = FALSE);
}

dir_out = file.path(dir_out,folder_name);
dir.create(dir_out);
setwd (dir_out);
sink(file.path(dir_out, result_name_txt));


cat ("\n\n\n\nDiagnostics on Policy Data\n\n\n\n");

# Test 1: Principal Component Analysis (PCA) for Detecting Linearity among Variables.
# Read sample policies.
mrm_raw_data=read.csv(dir_in, header = TRUE);
# mrm_raw_data = mrm_raw_data;
set.seed(1);
train = sample(1:nrow(mrm_raw_data),nrow(mrm_raw_data)/K_1,replace=FALSE);
mrm_raw_data = mrm_raw_data[train,];
mrm_raw_data[,c("AGE","FAMSZEYR","FAMINC","WAGEP","YEAR")] = lapply(mrm_raw_data[,c("AGE","FAMSZEYR","FAMINC","WAGEP","YEAR")], as.numeric);
names(mrm_raw_data)[1] = "TOTSLF";
glm.fit = glm (factor(TOTSLF)~., data = mrm_raw_data, family = "binomial");
INSURC_rate = predict(glm.fit);
names(mrm_raw_data)[1] = "INSURC_rate";
mrm_raw_data[,1] = INSURC_rate;
mrm_data = mrm_raw_data;


# Prepare pool of variables with interaction variables, for PCA.
INSURC_rate = mrm_data$INSURC_rate;
AGE = mrm_data$AGE;
SEX = mrm_data$SEX;
RACE = mrm_data$RACE;
REGION = mrm_data$REGION;
mrm_data_1 = data.matrix(mrm_data, rownames.force = NA);
cor(mrm_data_1);
# mrm_data=data.frame("INSURC_rate" = INSURC_rate,  "AGE" = AGE ,  "SEX" = SEX,
# "RACE" = RACE, "REGION" = REGION);

# Run PCA over the data.
# By using scale = TRUE, we scale the variables to have standard deviation 1. We use scale = FALSE since # there is dummy variable.
pr.out = prcomp ( mrm_data_1, scale = FALSE );

# Print principal component loadings of each principal component.
cat ("Test 1: PCA Analysis on All Variables");
cat ("\n\n\nResult 1.1: Principal Component Loadings of Each Principal Component:\n\n");
pr.out$rotation;

# Print the variance explained by each principal component.
cat ("\n\n\nResult 1.2: Variance Explained by Each Principal Component:\n\n");
pr.var = pr.out$sdev^2;
pr.var;

# Print the proportional of variance explained by each principal component.
cat ("\n\n\nResult 1.3: Proportional of Variance Explained by Each Principal Component:\n\n");
pve = pr.var/sum ( pr.var );
pve;


# Plot the cumulative proportion explained by the principal components.
png(file.path(dir_out, "Result_1_3_PCA_Cumulative_Proportion_of_Variance.png"));
plot ( cumsum ( pve ), xlab = "Number of Principal Components", ylab = "Cumulative Proportion Variance Explained", ylim = c ( 0.45, 1 ), type = 'b' );
dev.off();



# Test 2: Variable Selection: Best Subset Selection.
# Run logistic regression.
cat ("\n\n\nResult 2.1: Linear Regression on Full Data:\n\n");
summary(glm.fit);

lm.fit = lm (INSURC_rate~., data = mrm_raw_data);

# Install package “leaps”, in order to call the function: regsubsets().
install.packages ( "leaps" );
library ( leaps );

# Run best subset variables selection, using linear regression model:
# INSURC_rate ~ ( RACE, AGE, SEX, REGION ).
set.seed(1);
regfit.full = regsubsets ( INSURC_rate~., mrm_data, nvmax = 200, really.big=T);
reg.summary = summary ( regfit.full );

cat ("\n\n\nResult 2.2: Best Subset Variable Selection:\n\n");
reg.summary;

for (i in 1:(regfit.full$np-1)){
list_data =list(coef(regfit.full,i));
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 2.2: Best Subset Variable Selection: Model with",i, "Variables");
write.table(list_name, file = file.path(dir_out,result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(list_data, file = file.path(dir_out,result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE)};



# Plot the RSS, adjusted r-square, Cp and BIC for all the models.
png(file.path(dir_out, "Result_2_3_Best_Subset_Measurements_Goodness_of_Fit.png"));

# Split screen into 2 by 2 windows.
par(mfrow = c(2,2));

# Plot the RSS of all the models, and highlight the one minimizing the RSS.
plot ( reg.summary$rss, xlab = "Number of Variables", ylab = "Residual Sum of Squares", type = "b" );
points ( which.min ( reg.summary$rss ), reg.summary$rss[which.min ( reg.summary$rss ) ], col = "red", cex = 2, pch = 20 );

# Plot the adjusted r-square of all the models, and highlight the one maximizing the adjusted r-square.
plot ( reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type="b");
points ( which.max ( reg.summary$adjr2 ), reg.summary$adjr2 [ which.max ( reg.summary$adjr2 ) ], col = "red", cex = 2, pch = 20 );

# Plot the Cp of all the models, and highlight the one minimizing the Cp.
plot ( reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b" );
points ( which.min ( reg.summary$cp ), reg.summary$cp [ which.min ( reg.summary$cp ) ], col = "red", cex = 2, pch = 20 );

# Plot the BIC of all the models, and highlight the one minimizing the BIC.
plot ( reg.summary$bic,xlab = "Number of Variables", ylab = "BIC", type = "b" );
points ( which.min ( reg.summary$bic ), reg.summary$bic [ which.min ( reg.summary$bic ) ], col = "red", cex = 2, pch = 20 );
dev.off();


# Display the selected variables for the best model, by the particular statistical measure.
png(file.path(dir_out, "Result_2_4_Best_Subset_QR_Code_Goodness_of_Fit.png"));

# Black squares at the top row denote the variable is selected by the best model.
par ( mfrow = c ( 2, 2 ) );
plot ( regfit.full, scale = "r2" );
plot ( regfit.full, scale = "adjr2" );
plot ( regfit.full, scale = "Cp" );
plot ( regfit.full, scale = "bic" );
dev.off();

# Display the coefficients estimates associated to the best fitting model.
cat ("\n\n\nResult 2.4: Best Fitting Subset Variables and their Coefficients:\n\n");
coef ( regfit.full, which.min ( reg.summary$bic ) );
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 2.4: Best Fitting Subset Variables and their Coefficients: with", which.min ( reg.summary$bic ), "Variables");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(coef ( regfit.full, which.min ( reg.summary$bic ) ), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);



# k-fold cross-validation for variable selection.
# Define Prediction function for the method regsubsets().
predict.regsubsets = function (object, newdata, id, ...)
{ form = as.formula ( object$call[[2]] );
mat = model.matrix ( form, newdata );
coefi = coef ( object, id = id );
xvars = names ( coefi );
mat [ , xvars ]%*%coefi
}

# k-fold cross-validation for all the models with different subsets of variables.
k = 5;
set.seed ( 1 );
n_row = nrow ( mrm_data );
n_col = regfit.full$np-2;
folds = sample ( 1:k, n_row, replace = TRUE );
cv.errors = matrix ( NA, k, n_col, dimnames = list ( NULL, paste (1:n_col ) ) );
for (j in 1:k){
set.seed(1);
best.fit = regsubsets ( INSURC_rate ~.,data = mrm_data [folds!=j, ], nvmax = n_col, really.big=T );
for ( i in 1:n_col ){
pred = predict(best.fit, mrm_data[folds == j, ], id = i);
cv.errors [j, i] = mean((mrm_data$INSURC_rate [folds == j] - pred)^2)}};
cv.best = apply ( cv.errors, 2, mean );
cv.best[is.na(cv.best)] = 0;

cat ("\n\n\nResult 2.5: Best Subset: Test MSE:\n\n");
cv.best;

cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 2.5: Best Subset: Number of Variables VS Test MSE");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(cv.best, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);



# Plot the best model via k-fold cross-validation.
png(file.path(dir_out, "Result_2_6_Best_Subset_CV_Errors.png"));
plot ( apply(cv.errors,2,mean),xlab = "Number of Variables", ylab = "Prediction MSE", type = "b" );
points ( which.min(apply(cv.errors,2,mean)), apply(cv.errors,2,mean) [ which.min ( apply(cv.errors,2,mean)) ], col = "red", cex = 2, pch = 20 );
dev.off();



# Display the coefficients estimates associated to the best forecasting model.
cat ("\n\n\nResult 2.7: Best Forecasting Subset Variables and their Coefficients:\n\n");
coef ( regfit.full, which.min ( apply(cv.errors,2,mean) ) );

cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 2.7: Best Forecasting Subset Variables and their Coefficients: with", which.min ( apply(cv.errors,2,mean) ) , "Variables");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(coef ( regfit.full,which.min ( apply(cv.errors,2,mean) )), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# Test 3: Variable Selection: Forward Stepwise Subset Selection.

# Run forward stepwise subset variables selection, using linear regression model:
set.seed(1);
regfit.fwd = regsubsets (INSURC_rate~., mrm_data,nvmax = 200, method = "forward", really.big=T);
reg.summary = summary ( regfit.fwd );

cat ("\n\n\nResult 3.1: Forward Subset Variable Selection:\n\n");
reg.summary;

for (i in 1:(regfit.fwd$np-1)){
list_data =list(coef(regfit.fwd,i));
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 3.1: Forward Subset Variable Selection: Model with",i, "Variables");
write.table(list_name, file = file.path(dir_out,result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(list_data, file = file.path(dir_out,result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE)};


# Plot the RSS, adjusted r-square, Cp and BIC for all the models.
png(file.path(dir_out, "Result_3_2_Forward_Subset_Measurements_Goodness_of_Fit.png"));

# Split screen into 2 by 2 windows.
par(mfrow = c(2,2));

# Plot the RSS of all the models, and highlight the one minimizing the RSS.
plot ( reg.summary$rss, xlab = "Number of Variables", ylab = "Residual Sum of Squares", type = "b" );
points ( which.min ( reg.summary$rss ), reg.summary$rss[which.min ( reg.summary$rss ) ], col = "red", cex = 2, pch = 20 );

# Plot the adjusted r-square of all the models, and highlight the one maximizing the adjusted r-square.
plot ( reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type="b");
points ( which.max ( reg.summary$adjr2 ), reg.summary$adjr2 [ which.max ( reg.summary$adjr2 ) ], col = "red", cex = 2, pch = 20 );

# Plot the Cp of all the models, and highlight the one minimizing the Cp.
plot ( reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b" );
points ( which.min ( reg.summary$cp ), reg.summary$cp [ which.min ( reg.summary$cp ) ], col = "red", cex = 2, pch = 20 );

# Plot the BIC of all the models, and highlight the one minimizing the BIC.
plot ( reg.summary$bic,xlab = "Number of Variables", ylab = "BIC", type = "b" );
points ( which.min ( reg.summary$bic ), reg.summary$bic [ which.min ( reg.summary$bic ) ], col = "red", cex = 2, pch = 20 );
dev.off();


# Display the selected variables for the best model, by the particular statistical measure.
png(file.path(dir_out, "Result_3_2_Forward_Subset_QR_Code_Goodness_of_Fit.png"));

# Black squares at the top row denote the variable is selected by the best model.
par ( mfrow = c ( 2, 2 ) );
plot ( regfit.fwd, scale = "r2" );
plot ( regfit.fwd, scale = "adjr2" );
plot ( regfit.fwd, scale = "Cp" );
plot ( regfit.fwd, scale = "bic" );
dev.off();


# Display the coefficients estimates associated to the best fitting model.
cat ("\n\n\nResult 3.3: Forward Best Fitting Subset Variables and their Coefficients:\n\n");
coef ( regfit.fwd, which.min ( reg.summary$bic ) );
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 3.3: Forward Best Fitting Subset Variables and their Coefficients: with", which.min ( reg.summary$bic ), "Variables");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(coef ( regfit.fwd, which.min ( reg.summary$bic ) ), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);

# k-fold cross-validation for variable selection.
# k-fold cross-validation for all the models with different subsets of variables.
set.seed ( 1 );
k = 5;
n_row = nrow ( mrm_data );
n_col = regfit.fwd$np-2;
folds = sample ( 1:k, n_row, replace = TRUE );
cv.errors = matrix ( NA, k, n_col, dimnames = list ( NULL, paste (1:n_col ) ) );
for (j in 1:k){
set.seed(1);
best.fit = regsubsets ( INSURC_rate~.,data = mrm_data [folds!=j, ], nvmax = n_col, method = "forward", really.big=T );
for ( i in 1:n_col ){
pred = predict(best.fit, mrm_data [folds == j, ], id = i);
cv.errors [j, i] = mean((mrm_data$INSURC_rate [folds == j] - pred)^2)}};

cv.fwd = apply ( cv.errors, 2, mean );
cv.fwd[is.na(cv.fwd)] = 0;

cat ("\n\n\nResult 3.4: Forward Subset: Test MSE:\n\n");
cv.fwd;
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 3.4: Forward Subset: Number of Variables VS Test MSE");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(cv.fwd, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# Plot the best model via k-fold cross-validation.
png(file.path(dir_out, "Result_3_5_Forward_Subset_CV_Errors.png"));
plot ( apply(cv.errors,2,mean),xlab = "Number of Variables", ylab = "Prediction MSE", type = "b" );
points ( which.min(apply(cv.errors,2,mean)), apply(cv.errors,2,mean) [ which.min ( apply(cv.errors,2,mean)) ], col = "red", cex = 2, pch = 20 );
dev.off();

# Display the coefficients estimates associated to the best forecasting model.

cat ("\n\n\nResult 3.6: Forward Forecasting Subset Variables and their Coefficients:\n\n");
coef ( regfit.fwd, which.min ( apply(cv.errors,2,mean) ) );
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 3.6: Forward Best Forecasting Subset Variables and their Coefficients: with", which.min ( apply(cv.errors,2,mean) ) , "Variables");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(coef ( regfit.fwd,which.min ( apply(cv.errors,2,mean) )), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# Test 4: Variable Selection: Backward Stepwise Subset Selection.
# Run backward stepwise subset variables selection, using linear regression model:
set.seed(1);
regfit.bwd = regsubsets ( INSURC_rate~., mrm_data,nvmax = 200, method = "backward", really.big=T);
reg.summary = summary ( regfit.bwd );

cat ("\n\n\nResult 4.1: Backward Subset Variable Selection:\n\n");
reg.summary;

for (i in 1:(regfit.bwd$np-1)){
list_data =list(coef(regfit.bwd,i));
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 4.1: Backward Subset Variable Selection: Model with",i, "Variables");
write.table(list_name, file = file.path(dir_out,result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(list_data, file = file.path(dir_out,result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE)};


# Plot the RSS, adjusted r-square, Cp and BIC for all the models.
png(file.path(dir_out, "Result_4_2_Backward_Subset_Measurements_Goodness_of_Fit.png"));

# Split screen into 2 by 2 windows.
par(mfrow = c(2,2));

# Plot the RSS of all the models, and highlight the one minimizing the RSS.
plot ( reg.summary$rss, xlab = "Number of Variables", ylab = "Residual Sum of Squares", type = "b" );
points ( which.min ( reg.summary$rss ), reg.summary$rss[which.min ( reg.summary$rss ) ], col = "red", cex = 2, pch = 20 );

# Plot the adjusted r-square of all the models, and highlight the one maximizing the adjusted r-square.
plot ( reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type="b");
points ( which.max ( reg.summary$adjr2 ), reg.summary$adjr2 [ which.max ( reg.summary$adjr2 ) ], col = "red", cex = 2, pch = 20 );

# Plot the Cp of all the models, and highlight the one minimizing the Cp.
plot ( reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b" );
points ( which.min ( reg.summary$cp ), reg.summary$cp [ which.min ( reg.summary$cp ) ], col = "red", cex = 2, pch = 20 );

# Plot the BIC of all the models, and highlight the one minimizing the BIC.
plot ( reg.summary$bic,xlab = "Number of Variables", ylab = "BIC", type = "b" );
points ( which.min ( reg.summary$bic ), reg.summary$bic [ which.min ( reg.summary$bic ) ], col = "red", cex = 2, pch = 20 );

dev.off();


# Display the selected variables for the best model, by the particular statistical measure.
png(file.path(dir_out, "Result_4_2_Backward_Subset_QR_Code_Goodness_of_Fit.png"));

# Black squares at the top row denote the variable is selected by the best model.
par ( mfrow = c ( 2, 2 ) );
plot ( regfit.bwd, scale = "r2" );
plot ( regfit.bwd, scale = "adjr2" );
plot ( regfit.bwd, scale = "Cp" );
plot ( regfit.bwd, scale = "bic" );

dev.off();


# Display the coefficients estimates associated to the best fitting model.
cat ("\n\n\nResult 4.3: Backward Best Fitting Subset Variables and their Coefficients:\n\n");
coef ( regfit.bwd, which.min ( reg.summary$bic ) );
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 4.3: Backward Best Fitting Subset Variables and their Coefficients: with", which.min ( reg.summary$bic ), "Variables");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(coef ( regfit.bwd, which.min ( reg.summary$bic ) ), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);

# k-fold cross-validation for variable selection.
# k-fold cross-validation for all the models with different subsets of variables.
set.seed ( 1 );
k = 5;
n_row = nrow ( mrm_data );
n_col = regfit.bwd$np-2;
folds = sample ( 1:k, n_row, replace = TRUE );
cv.errors = matrix ( NA, k, n_col, dimnames = list ( NULL, paste (1:n_col ) ) );
for (j in 1:k){
set.seed(1);
best.fit = regsubsets ( INSURC_rate~+.,data = mrm_data [folds!=j, ], nvmax = n_col, method = "backward", really.big=T );
for ( i in 1:n_col ){
pred = predict(best.fit, mrm_data [folds == j, ], id = i);
cv.errors [j, i] = mean((mrm_data$INSURC_rate [folds == j] - pred)^2)}};

cv.bwd = apply ( cv.errors, 2, mean );
cv.bwd [is.na(cv.bwd)] = 0;


cat ("\n\n\nResult 4.4: Backward Subset: Test MSE:\n\n");
cv.bwd;
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 4.4: Backward Subset: Number of Variables VS Test MSE");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(cv.bwd, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# Plot the best model via k-fold cross-validation.
png(file.path(dir_out, "Result_4_5_Backward_Subset_CV_Errors.png"));
plot ( apply(cv.errors,2,mean),xlab = "Number of Variables", ylab = "Prediction MSE", type = "b" );
points ( which.min(apply(cv.errors,2,mean)), apply(cv.errors,2,mean) [ which.min ( apply(cv.errors,2,mean)) ], col = "red", cex = 2, pch = 20 );
dev.off();

# Display the coefficients estimates associated to the best forecasting model.

cat ("\n\n\nResult 4.6: Backward Forecasting Subset Variables and their Coefficients:\n\n");
coef ( regfit.bwd, which.min ( apply(cv.errors,2,mean) ) );
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 4.6: Backward Best Forecasting Subset Variables and their Coefficients: with", which.min ( apply(cv.errors,2,mean) ) , "Variables");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(coef ( regfit.bwd,which.min ( apply(cv.errors,2,mean) )), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);

# Test 5: Variable Selection: the lasso.

# Install packages for the lasso regressions.
install.packages("glmnet");
library(glmnet);

# x is the set of factors. y is the response variable. grid is candidate values of lambda.
x = model.matrix (INSURC_rate~.,mrm_data) [, -ncol(mrm_data)];
y = mrm_data$INSURC_rate;
grid = 10^seq (10,-2, length = 100);

# Compute the best lasso model coefficients, by selecting the lambda which minimizes the fitting error.
set.seed(1);
set.seed(1);
lasso.mod = glmnet (x, y, alpha = 1, lambda = grid, thresh = 1e-12 );
cv.out = cv.glmnet (x, y, alpha = 1, lambda = grid, thresh = 1e-12 );
best_lambda = cv.out$lambda.min;
lasso.coef = predict (lasso.mod, type = "coefficients", s = best_lambda);
lasso.coef = lasso.coef[-nrow(lasso.coef),];
cat ("\n\n\nResult 5.1: the lasso and their Coefficients:\n\n");
lasso.coef;

cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 5.1: the lasso and their Coefficients:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(lasso.coef, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# Test 6: Variable Selection: Random Forests.
# Install packages for variable selection using random forests.
install.packages("VSURF");
install.packages("randomForest")
library(VSURF);
library(randomForest);

# Create candidate variables and response.
x = model.matrix (INSURC_rate~.,mrm_data) [, -ncol(mrm_data)];
y = mrm_data$INSURC_rate;
# Perform variable selection over 1/1000 scenarios.
set.seed(1);
train = sample (1:nrow(x), nrow(x)/K_2);
set.seed(1);
data.vsurf = VSURF (x = x[train,], y = y[train], ntree=100, mtry = floor(ncol(mrm_data)/3), nfor.thres=25, nfor.interp=10, parallel = TRUE, ncores=5);

cat ("\n\n\nResult 6.1: Variable Selection using Random Forests:\n\n");
summary (data.vsurf);

png(file.path(dir_out, "Result_6_2_ Variable_Selection_using_Random_Forests.png"));
plot (data.vsurf);
dev.off();

# List the best subset variables for fitting performance.
cat ("\n\n\nResult 6.2: Thresholding Variables Selected by Random Forests:\n\n");
colnames(x)[data.vsurf$varselect.thres];

cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 6.2: Thresholding Variables Selected by Random Forests:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(colnames(x)[data.vsurf$varselect.thres], file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);

# List the best subset variables for fitting performance.
cat ("\n\n\nResult 6.3: Interpretation Variables Selected by Random Forests:\n\n");
colnames(x)[data.vsurf$varselect.interp];
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 6.3: Interpretation Variables Selected by Random Forests:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(colnames(x)[data.vsurf$varselect.interp], file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# List the best subset variables for prediction performance.
# List the best subset variables for fitting performance.
cat ("\n\n\nResult 6.4: Prediction Variables Selected by Random Forests:\n\n");
colnames(x)[data.vsurf$varselect.pred];
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 6.4: Prediction Variables Selected by Random Forests:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(colnames(x)[data.vsurf$varselect.pred], file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


# Test 7: Alternative Methods.
# Method 1: Regression Tree.
# Install packages for tree-based methods.
install.packages("randomForest");
library(randomForest);

# Fit regression tree.
tree.fit=randomForest(INSURC_rate~.,data=mrm_data,mtry=floor(ncol(mrm_data)/3),ntree=100,importance=TRUE);

cat ("\n\n\nResult 7.1: Fitting by Random Forests:\n\n");
tree.fit;

# View the importance of variables. IncMSE measures the decrease of accuracy in test MSE when the
# variable is out; the IncNodePurity measures the decrease of accuracy in training RSS when the variable # is out.
cat ("\n\n\nResult 7.2: Importance of Variables:\n\n");
importance(tree.fit);

cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 7.2: Importance of Variables:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(importance(tree.fit), file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);

png(file.path(dir_out, "Result_7_3_ Importance_of_Variables.png"));
varImpPlot(tree.fit);
dev.off();

# Use the regression tree to predict the other half data and compute its test MSE.
k = 5;
set.seed(1);
folds = sample(1:k,nrow(mrm_data),replace=TRUE);
r =0;
for (j in 1:k) {
tree.fit=randomForest(INSURC_rate~.,data=mrm_data[folds!=j,],ntry=ncol(mrm_data)/3,ntree=50);
yhat=predict(tree.fit,newdata=mrm_data[folds==j,]);
tree.test=mrm_data[folds==j,"INSURC_rate"];
r = r + mean((yhat-tree.test)^2)
};
cat ("\n\n\nResult 7.4: CV Tree:\n\n");
cv.tree = r/k;
cv.tree[is.na(cv.tree)] = 0;
cv.tree;

# Method 2: Ridge regression.
# x is the set of factors. y is the response variable. grid is candidate values of lambda.
install.packages("MASS");
library(MASS);
x = model.matrix (INSURC_rate~.,mrm_data) [, -ncol(mrm_data)];
y = mrm_data$INSURC_rate;
grid = 10^seq (10,-2, length = 1000);

# Use half-set cross validation to compute test MSE.
k = 5;
set.seed(1);
folds = sample(1:k,nrow(mrm_data),replace=TRUE);
r =0;
for (j in 1:k) {
y.test = y[folds==j];
ridge.mod = glmnet (x[folds!=j,], y[folds!=j], alpha = 0, lambda = grid, thresh = 1e-12 );
ridge.pred = predict (ridge.mod, s = 0.01, newx = x [folds==j,]);
r = r+ mean((ridge.pred - y.test)^2)
}
cv.ridge = r/k;
cv.ridge[is.na(cv.ridge)] = 0;

# Method 3: Lasso regression.
k = 5;
set.seed(1);
folds = sample(1:k,nrow(mrm_data),replace=TRUE);
r =0;
for (j in 1:k) {
y.test = y[folds==j];
lasso.mod = glmnet (x[folds!=j,], y[folds!=j], alpha = 1, lambda = grid, thresh = 1e-12 );
lasso.pred = predict (lasso.mod, s = 0.01, newx = x [folds==j,]);
r = r+mean((lasso.pred - y.test)^2)
};
cv.lasso = r/k;
cv.lasso[is.na(cv.lasso)] = 0;

# Use k-fold cross-validation to estimate the test MSE of the current model.
k = 5;
set.seed ( 1 );
n_row = nrow ( mrm_data );
folds = sample ( 1:k, n_row, replace = TRUE );
cv.errors =numeric(k);
for (j in 1:k){
set.seed(1);
current.fit = lm (INSURC_rate~., data = mrm_data [folds!=j, ]);
pred = predict(current.fit, mrm_data[folds == j, ]);
cv.errors [j] = mean((mrm_data$INSURC_rate [folds == j] - pred)^2)};
cv.current = mean ( cv.errors);
cv.current[is.na(cv.current)] = 0;


# Compare the test MSE of the logit function of the lapse rates of the 7 models: best, forward, backward, # logistic regression, tree, ridge and lasso, current.
cv.reg_best = min(cv.best);
cv.reg_fwd= min(cv.fwd);
cv.reg_bwd= min(cv.bwd);
MSE=c(cv.reg_best,cv.reg_fwd,cv.reg_bwd,cv.tree,cv.ridge,cv.lasso,cv.current);

cat ("\n\n\nResult 7.5: Comparison of the 7 Models – MSE Logit Lapse Rates:\n\n");
list_data = list(c("reg best","reg fwd","reg bwd","random forest","ridge","lasso","current"),MSE);
list_data;
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 7.5: Comparison of the 7 Models – MSE Logit Lapse Rates:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(list_data, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


png(file.path(dir_out, "Result_7_5_ Comparison_of_Predictive_Models.png"));

plot (1:7,MSE,xlab = "Models", ylab = "Prediction MSE – Logit Lapse Rate", type = "b" );
points ( which.min(MSE), MSE [ which.min (MSE) ], col = "red", cex = 2, pch = 20 );
points ( 7, MSE [ 7 ], col = "blue", cex = 2, pch = 20 );

dev.off();

# Compare the test MSE of the lapse rates of the 7 models: best, forward, backward, logistic regression, # tree, ridge and lasso, current.
weight = 144*mean(exp(2*INSURC_rate)/(1+exp(INSURC_rate))^4);
MSE_lapse=c(cv.reg_best,cv.reg_fwd,cv.reg_bwd,cv.tree,cv.ridge,cv.lasso,cv.current)*weight;

cat ("\n\n\nResult 7.5: Comparison of the 7 Models – Lapse Rates:\n\n");
list_data = list(c("reg best","reg fwd","reg bwd","random forest","ridge","lasso","current"),MSE_lapse);
list_data;
cat("\n", file = file.path(dir_out, result_name_csv), append = TRUE);
list_name = paste("Result 7.5: Comparison of the 7 Models – Lapse Rates:");
write.table(list_name, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE);
write.table(list_data, file = file.path(dir_out, result_name_csv), append = TRUE, sep = ',', row.names = TRUE, col.names = FALSE);


png(file.path(dir_out, "Result_7_6_ Comparison_of_Predictive_Models.png"));

plot (1:7,MSE_lapse,xlab = "Models", ylab = "Prediction MSE – Lapse Rate", type = "b" );
points ( which.min(MSE_lapse), MSE_lapse [ which.min (MSE_lapse) ], col = "red", cex = 2, pch = 20 );
points ( 7, MSE_lapse [ 7 ], col = "blue", cex = 2, pch = 20 );

dev.off();

end = Sys.time();


sink (result_name_txt, append = TRUE, split = TRUE);









# Generate diagnostics report.
# Use the package “rtf”.
install.packages("rtf");
library(rtf);

# Create report.
report = RTF(report_name);

# Add Title, date and table of content.
addHeader(report, "              Diagnostics Result Report", subtitle = "                   ------ Experience Study on Health Data", font.size = 20);
addNewLine(report, n = 1);

addParagraph (report, "Data analytics process started at ",now);
addParagraph (report, "Data analytics process ended at ",end);
addParagraph (report, "Report generated at ",Sys.time());
addNewLine(report, n = 1);

addParagraph (report, "Table of Content");
addNewLine(report, n = 1);
addParagraph (report, "    Test 1: Principal Component Analysis (PCA) for Detecting Linearity among Variables");
addParagraph (report, "        Result 1.1: Principal Component Loadings of Each Principal Component");
addParagraph (report, "        Result 1.2: Variance Explained by Each Principal Component");
addParagraph (report, "        Result 1.3: Proportion of Variance Explained by Each Principal Component");
addNewLine(report, n = 1);

addParagraph (report, "    Test 2: Variable Selection: Subset, the Lasso, Random Forests");
addParagraph (report, "        Test 2.1: Variable Selection: Best Subset Selection");
addParagraph (report, "            Result 2.1.1: Linear Regression on Full Data");
addParagraph (report, "            Result 2.1.2: Best Subset Variable Selection");
addParagraph (report, "            Result 2.1.3: Best Subset Selection – Measurements of Goodness of Fit");
addParagraph (report, "            Result 2.1.4: Best Fitting Subset Variables and their Coefficients");
addParagraph (report, "            Result 2.1.5: Best Subset: Number of Variables VS Test MSE");
addParagraph (report, "            Result 2.1.6: Best Forecasting Subset Variables and their Coefficients");
addParagraph (report, "        Test 2.2: Variable Selection: Forward Stepwise Subset Selection");
addParagraph (report, "            Result 2.2.1: Forward Stepwise Subset Variable Selection");
addParagraph (report, "            Result 2.2.2: Forward Stepwise Subset Selection – Measurements of Goodness of Fit");
addParagraph (report, "            Result 2.2.3: Forward Stepwise Best Fitting Subset Variables and their Coefficients");
addParagraph (report, "            Result 2.2.4: Forward Stepwise Subset: Number of Variables VS Test MSE");
addParagraph (report, "            Result 2.2.5: Forward Stepwise Forecasting Subset Variables and their Coefficients");
addParagraph (report, "        Test 2.3: Variable Selection: Backward Stepwise Subset Selection");
addParagraph (report, "            Result 2.3.1: Backward Stepwise Subset variable Selection");
addParagraph (report, "            Result 2.3.2: Backward Stepwise Subset Selection – Measurements of Goodness of Fit");
addParagraph (report, "            Result 2.3.3: Backward Stepwise Best Fitting Subset Variables and their Coefficients");
addParagraph (report, "            Result 2.3.4: Backward Stepwise Subset: Number of Variables VS Test MSE");
addParagraph (report, "        Test 2.4: Variable Selection: the Lasso");
addParagraph (report, "            Result 2.4.1: the Lasso and its Coefficients");
addParagraph (report, "        Test 2.5: Variable Selection: Random Forests");
addParagraph (report, "            Result 2.5.1: Summary: Variable Selection using Random Forests");
addParagraph (report, "            Result 2.5.2: Figure: Variable Selection using Random Forests");
addParagraph (report, "            Result 2.5.3: Thresholding Variables Selected by Random Forests");
addParagraph (report, "            Result 2.5.4: Interpretation Variables Selected by Random Forests");
addParagraph (report, "            Result 2.5.5: Prediction Variables Selected by Random Forests");
addParagraph (report, "            Result 2.5.6: Figure: Importance of Variables using Random Forests");
addNewLine(report, n = 1);

addParagraph (report, "    Test 3: Model Selection: Logistic Regression, Random Forests, Ridge, the Lasso");
addParagraph (report, "        Result 3.1: Ridge Regression and its Coefficients");
addParagraph (report, "        Result 3.2: The Lasso and its Coefficients");
addParagraph (report, "        Result 3.3: Comparison of the Models: Best Subset, Forward Stepwise Subset, Backward Stepwise Subset, Random Forests, Ridge, Lasso");

addPageBreak(report);



# Test 1: Principal Component Analysis (PCA) for Detecting Linearity among Variables.
addParagraph (report, " Test 1: Principal Component Analysis (PCA) for Detecting Linearity among Variables");
addNewLine(report, n = 1);

addParagraph (report, "Goal: detecting linearity among the variables.");
addNewLine(report, n = 1);

# Test 1.1: Print principal component loadings of each principal component.
addParagraph (report, "Result 1.1: Principal Component Loadings of Each Principal Component:");
addNewLine(report, n = 1);

addParagraph (report, "The table below presents the coefficients of all variables for each principal component.");
addNewLine(report, n = 1);

table = cbind(rownames(pr.out$rotation), round(pr.out$rotation,10));
colnames(table)[1] = "Variable";
addTable(report,table);
addNewLine(report, n=1);

# Test 1.2: Print variance explained by each principal component.
addParagraph (report, "Result 1.2: Variance Explained by Each Principal Component:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the variance of the entire data explained by each principal component.");
addNewLine(report, n=1);

table = cbind(colnames(pr.out$rotation), round(pr.var,10));
colnames(table)[1:2] = c("PC","Variance Explained");
addTable(report,table);
addNewLine(report, n=1);

# Test 1.3: Print the proportion of variance explained by each principal component.
addParagraph (report, "Result 1.3: Proportion of Variance Explained by Each Principal Component:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the proportion of variance explained by each principal component.");
addNewLine(report, n=1);

table = cbind(colnames(pr.out$rotation), round(pve,10));
colnames(table)[1:2] = c("PC","Proportional of Variance Explained");
addTable(report,table);
addNewLine(report, n=1);

# Plot the cumulative proportion explained by the principal components.
addParagraph (report, "Result 1.3: Figure 1.1: Proportional of Variance Explained by Each Principal Component:");
addNewLine(report, n = 1);

addParagraph (report, "The figure below illustrates the cumulative proportions of variance explained by the principal components.");

addPng(report, "Result_1_3_PCA_Cumulative_Proportion_of_Variance.png", width = 6, height = 6);

# Test 2: Variable Selection.
# Test 2.1: Variable Selection: Best Subset Selection.
# Test 2.1.1: Linear Regression on Full Data.

addParagraph (report, "Test 2: Variable Selection: Subsets, the Lasso, Random Forests.");
addNewLine(report, n = 1);
addParagraph (report, "Test 2.1: Variable Selection: Best Subset Selection.");
addNewLine(report, n = 1);

addParagraph (report, "Goal: Select the subset of variables which best explains the response variable, using full subset selection approach.");
addNewLine(report, n = 1);

addParagraph (report, "Result 2.1.1: Linear Regression on Full Data:");
addNewLine(report, n = 1);

addParagraph (report, "The table below presents the statistics from the ordinary linear regression over all variables.");
addNewLine(report, n = 1);


out = function (s1)
{
# coefficients
r1 <- round(coef(s1), 9);
# summary statistics
sigma <- round(s1$sigma, 9);
rsq <- round(s1$adj.r.squared, 9);
# sample sizes
sample_size <- length(s1$residuals);
outtab <- rbind(r1, sigma, rsq, sample_size);
rownames(outtab) <- c(rownames(coef(s1)), "sigma", "Adj. R-Squared", "sample size");
outtab
}

table = cbind(rownames(out(summary(lm.fit))), out(summary(lm.fit)));
colnames(table)[1] = "Variable";
addTable(report,table);
addNewLine(report, n = 1);

# Test 2.1.2: Run best subset variables selection, using linear regression model.
addParagraph (report, "Result 2.1.2: Best Subset Variable Selection:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the best subset of i variables,  for i going from 1 to", regfit.full$np-1, "through comparing the RSS. * denotes that the corresponding variables is selected into the subset.");
addNewLine(report, n=1);

table = summary ( regfit.full )$outmat;
rownames(table) = 1:nrow(table);
table = cbind(rownames(table),table)
colnames(table)[1] = "Number of Variables";
addTable(report,table,header.col.justify = "L", font.size = 6);
addNewLine(report, n=1);

addParagraph (report, "The tables below present the variables and coefficients of the best models with i variables, for i going from 1 to", regfit.full$np-1, ".");
addNewLine(report, n=1);


for (i in 1:(regfit.full$np-1)){
addParagraph (report, "Result 2.1.2: Best Subset Variable Selection: Model with ",i, " Variables");
addNewLine(report, n=1);
table = cbind(names(coef(regfit.full,i)), round(coef(regfit.full,i),10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);
}

# Test 2.1.3: Plot the RSS, adjusted r-square, Cp and BIC for all the models.
addParagraph (report, "Result 2.1.3: Figure 2.1.1: Best Subset Selection – Measurements of Goodness of Fit:");
addNewLine(report, n = 1);
addParagraph (report, "The figure below presents the RSS, adjusted r-square, Cp and BIC for the best models with i variables, for i going from 1 to", regfit.full$np-1, ".");

addPng(report, "Result_2_3_Best_Subset_Measurements_Goodness_of_Fit.png", width = 6, height = 6);

# Test 2.1.3: Plot the coefficients estimates associated to the best fitting model.
addParagraph (report, "Result 2.1.3: Figure 2.1.2: Best Subset Selection – QR Code Plot:");
addNewLine(report, n = 1);
addParagraph (report, "The figure below presents the best subset chosen via the thresholding values of  r-square, adjusted r-square, Cp and BIC.");

addPng(report, "Result_2_4_Best_Subset_QR_Code_Goodness_of_Fit.png", width = 6, height = 6);

# Test 2.1.4: Print Best Fitting Subset of Variables and their Coefficients.
addParagraph (report, "Result 2.1.4: Best Fitting Subset Variables and their Coefficients:");
addNewLine(report, n = 1);
addParagraph (report, "The table below presents the best subset for fitting with its coefficients, selected by the BIC.");
addNewLine(report, n = 1);

coefficient = coef (regfit.full, which.min ( summary(regfit.full)$bic ) );
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.1.5: Print the Test MSE of the Models via k-fold Cross Validation.
addParagraph (report, "Result 2.1.5: Best Subset: Number of Variables VS Test MSE:");
addNewLine(report, n = 1);
addParagraph (report, "The table below presents the best subset for forecasting with its coefficients, selected by the test MSE.");

table = cbind(1:length(cv.best), round(cv.best,10));
colnames(table)[1:2] = c("Number of Variables", "Coefficient");
addTable(report,table);
addNewLine(report, n = 1);

# Plot the best model via k-fold cross-validation.
addParagraph (report, "Result 2.1.5: Figure 2.1.3: Best Subset: Number of Variables VS Test MSE:");
addNewLine(report, n = 1);

addParagraph (report, "The figure below illustrates the test MSE of each model containing i variables, for i going from 1 to", regfit.full$np-1, ".");

addPng(report, "Result_2_6_Best_Subset_CV_Errors.png", width = 6, height = 6);

# Test 2.1.6: Print the coefficients estimates associated to the best forecasting model.
addParagraph (report, "Result 2.1.6: Best Forecasting Subset Variables and their Coefficients:");
addNewLine(report, n = 1);
addParagraph (report, "The table below presents the best subset and its variables' coefficients, which minimizes the test MSE.");


coefficient = coef (regfit.full, which.min ( cv.best ) );
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n = 1);

# Test 2.2: Variable Selection: Forward Stepwise Subset Selection.
addParagraph (report, "Test 2.2: Variable Selection: Forward Stepwise Subset Selection.");
addNewLine(report, n = 1);

# Test 2.2.1: Run forward stepwise subset variables selection, using linear regression model.
addParagraph (report, "Result 2.2.1: Forward Stepwise Subset Variable Selection:");
addNewLine(report, n = 1);

addParagraph (report, "Goal: Select the subset of variables which best explains the response variable, using forward stepwise subset selection approach.");
addNewLine(report, n = 1);

addParagraph (report, "The table below presents the forward stepwise best subset of i variables,  for i going from 1 to", regfit.fwd$np-1, "through comparing the RSS. * denotes that the corresponding variables is selected into the subset.");
addNewLine(report, n=1);


table = summary ( regfit.fwd)$outmat;
rownames(table) = 1:nrow(table);
table = cbind(rownames(table),table)
colnames(table)[1] = "Number of Variables";
addTable(report,table,header.col.justify = "L", font.size = 6);
addNewLine(report, n=1);

addParagraph (report, "The tables below present the variables and coefficients of the best models with i variables, for i going from 1 to", regfit.fwd$np-1, ".");
addNewLine(report, n=1);

for (i in 1:(regfit.fwd$np-2)){
addParagraph (report, "Result 2.2.1: Forward Stepwise Subset Variable Selection: Model with ",i, " Variables");
addNewLine(report, n=1);
table = cbind(names(coef(regfit.fwd,i)), round(coef(regfit.fwd,i),10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);
}

# Test 2.2.2: Plot the RSS, adjusted r-square, Cp and BIC for all the models.
addParagraph (report, "Result 2.2.2: Figure 2.2.1: Forward Stepwise Subset Selection – Measurements of Goodness of Fit:");

addParagraph (report, "The figure below presents the RSS, adjusted r-square, Cp and BIC for the forward stepwise best models with i variables, for i going from 1 to", regfit.fwd$np-1, ".");

addPng(report, "Result_3_2_Forward_Subset_Measurements_Goodness_of_Fit.png", width = 6, height = 6);

addParagraph (report, "The figure below presents the forward stepwise best subset chosen via the thresholding values of  r-square, adjusted r-square, Cp and BIC.");

addParagraph (report, "Result 2.2.2: Figure 2.2.2: Forward Stepwise Subset Selection – QR Code Plot:");
addPng(report, "Result_3_2_Forward_Subset_QR_Code_Goodness_of_Fit.png", width = 6, height = 6);

# Test 2.2.3: Print Best Fitting Subset of Variables and their Coefficients.
addParagraph (report, "Result 2.2.3: Forward Stepwise Best Fitting Subset Variables and their Coefficients:");
addNewLine(report, n = 1);

addParagraph (report, "The table below presents the forward stepwise best subset for fitting with its coefficients, selected by the BIC.");
addNewLine(report, n = 1);

coefficient = coef (regfit.fwd, which.min ( summary(regfit.fwd)$bic ) );
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.2.4: Print the Test MSE of the Models via k-fold Cross Validation.
addParagraph (report, "Result 2.2.4: Forward Stepwise Subset: Number of Variables VS Test MSE:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the forward stepwise best subset for forecasting with its coefficients, selected by the test MSE.");

table = cbind(1:length(cv.fwd), round(cv.fwd,10));
colnames(table)[1:2] = c("Number of Variables", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Plot the best model via k-fold cross-validation.
addParagraph (report, "Result 2.2.4: Figure 2.2.3: Forward Stepwise Subset: Number of Variables VS Test MSE:");

addParagraph (report, "The figure below illustrates the test MSE of each model containing i variables, for i going from 1 to", regfit.fwd$np-1, ".");

addPng(report, "Result_3_5_Forward_Subset_CV_Errors.png", width = 6, height = 6);

# Test 2.2.5: Print the coefficients estimates associated to the best forecasting model.
addParagraph (report, "Result 2.2.5: Forward Stepwise: Best Forecasting Subset Variables and their Coefficients:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the forward stepwise best subset and its variables' coefficients, which minimizes the test MSE.");
coefficient = coef (regfit.fwd, which.min ( cv.fwd ) );
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.3: Variable Selection: Backward Stepwise Subset Selection.
addParagraph (report, "Test 2.3: Variable Selection: Backward Stepwise Subset Selection.");
addNewLine(report, n = 1);

# Test 2.3.1: Run backward stepwise subset variables selection, using linear regression model.
addParagraph (report, "Result 2.3.1: Backward Stepwise Subset Variable Selection:");
addNewLine(report, n = 1);

addParagraph (report, "Goal: Select the subset of variables which best explains the response variable, using backward stepwise subset selection approach.");
addNewLine(report, n = 1);

addParagraph (report, "The table below presents the backward stepwise best subset of i variables,  for i going from 1 to", regfit.bwd$np-1, "through comparing the RSS. * denotes that the corresponding variables is selected into the subset.");
addNewLine(report, n=1);


table = summary ( regfit.bwd)$outmat;
rownames(table) = 1:nrow(table);
table = cbind(rownames(table),table)
colnames(table)[1] = "Number of Variables";
addTable(report,table,header.col.justify = "L", font.size = 6);
addNewLine(report, n=1);

addParagraph (report, "The tables below present the variables and coefficients of the best models with i variables, for i going from 1 to", regfit.bwd$np-1, ".");
addNewLine(report, n=1);

for (i in 1:(regfit.bwd$np-2)){
addParagraph (report, "Result 2.3.1: Backward Stepwise Subset Variable Selection: Model with ",i, " Variables");
addNewLine(report, n=1);
table = cbind(names(coef(regfit.bwd,i)), round(coef(regfit.bwd,i),10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);
}

# Test 2.2.2: Plot the RSS, adjusted r-square, Cp and BIC for all the models.
addParagraph (report, "Result 2.3.2: Figure 2.3.1: Backward Stepwise Subset Selection – Measurements of Goodness of Fit:");

addParagraph (report, "The figure below presents the RSS, adjusted r-square, Cp and BIC for the backward stepwise best models with i variables, for i going from 1 to", regfit.bwd$np-1, ".");

addPng(report, "Result_4_2_Backward_Subset_Measurements_Goodness_of_Fit.png", width = 6, height = 6);

addParagraph (report, "The figure below presents the backward stepwise best subset chosen via the thresholding values of  r-square, adjusted r-square, Cp and BIC.");

addParagraph (report, "Result 2.3.2: Figure 2.3.2: Backward Stepwise Subset Selection – QR Code Plot:");
addPng(report, "Result_4_2_Backward_Subset_QR_Code_Goodness_of_Fit.png", width = 6, height = 6);

# Test 2.3.3: Print Best Fitting Subset of Variables and their Coefficients.
addParagraph (report, "Result 2.3.3: Backward Stepwise Best Fitting Subset Variables and their Coefficients:");
addNewLine(report, n = 1);

addParagraph (report, "The table below presents the backward stepwise best subset for fitting with its coefficients, selected by the BIC.");
addNewLine(report, n = 1);

coefficient = coef (regfit.bwd, which.min ( summary(regfit.bwd)$bic ) );
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.3.4: Print the Test MSE of the Models via k-fold Cross Validation.
addParagraph (report, "Result 2.3.4: Backward Stepwise Subset: Number of Variables VS Test MSE:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the backward stepwise best subset for forecasting with its coefficients, selected by the test MSE.");

table = cbind(1:length(cv.bwd), round(cv.bwd,10));
colnames(table)[1:2] = c("Number of Variables", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Plot the best model via k-fold cross-validation.
addParagraph (report, "Result 2.3.4: Figure 2.3.3: Backward Stepwise Subset: Number of Variables VS Test MSE:");

addParagraph (report, "The figure below illustrates the test MSE of each model containing i variables, for i going from 1 to", regfit.bwd$np-1, ".");

addPng(report, "Result_4_5_Backward_Subset_CV_Errors.png", width = 6, height = 6);

# Test 2.3.5: Print the coefficients estimates associated to the best forecasting model.
addParagraph (report, "Result 2.3.5: Backward Stepwise: Best Forecasting Subset Variables and their Coefficients:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the backward stepwise best subset and its variables' coefficients, which minimizes the test MSE.");
coefficient = coef (regfit.bwd, which.min ( cv.bwd ) );
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.4: Variable Selection: the lasso.
addParagraph (report, "Test 2.4: Variable Selection: the lasso");
addNewLine(report, n=1);

addParagraph (report, "Goal: Remove the variables if their coefficients are estimated as 0 by the lasso approach.");
addNewLine(report, n=1);


# Print the coefficients estimates of the lasso model.
addParagraph (report, "Result 2.4.1: the lasso and their Coefficients:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents the variables selected by the lasso and their coefficients.");

coefficient = lasso.coef;
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.5: Variable Selection: Random Forests.
addParagraph (report, "Test 2.5: Variable Selection: Random Forests");
addNewLine(report, n=1);

addParagraph (report, "Goal: Use Random Forests to determine the thresholding variables.");
addNewLine(report, n=1);


# Test 2.5.1: summarize the number of variables selected by the random forest.
addParagraph (report, "Result 2.5.1: Summary: Variable Selection using Random Forests:");
addNewLine(report, n=1);

addParagraph (report, "The table below summarizes the number of variables selected by the random forests.");
table = cbind(c("Thresholding", "Interpretation", "Prediction"), data.vsurf$nums.varselect);
table = as.data.frame(table);
colnames(table) = c("Variable", "Number");
addTable(report,table);
addNewLine(report, n=1);

# Test 2.5.2: Plot the subset selection results by using the random forest.
addParagraph (report, "Result 2.5.2: Figure 2.5.1: Variable Selection using Random Forests:");
addNewLine(report, n=1);

addParagraph (report, "The figure below illustrates the variable selection results by the random forests. The 3 set of variables are respectively thresholding, interpretation and prediction variables. They are determined via different measurements.");

addPng(report, "Result_6_2_ Variable_Selection_using_Random_Forests.png", width = 6, height = 6);

# Test 2.5.3: List the thresholding subset variables for fitting performance.
addParagraph (report, "Result 2.5.3: Thresholding Variables Selected by Random Forests:");
addNewLine(report, n=1);

addParagraph (report, "The table below lists the thresholding variables selected by the random forests.");

table = as.data.frame(colnames(x)[data.vsurf$varselect.thres]);
colnames(table) = "Thresholding Variables";
addTable(report, table);
addNewLine(report, n=1);

# Test 2.5.4: List the Interpretation subset variables for fitting performance.
addParagraph (report, "Result 2.5.4: Interpretation Variables Selected by Random Forests:");
addNewLine(report, n=1);

addParagraph (report, "The table below lists the interpretation variables selected by the random forests.");

table = as.data.frame(colnames(x)[data.vsurf$varselect.interp]);
colnames(table) = "Interpretation Variables";
addTable(report,table);
addNewLine(report, n=1);

# Test 2.5.5: List the prediction subset variables for fitting performance.
addParagraph (report, "Result 2.5.5: Prediction Variables Selected by Random Forests:");
addNewLine(report, n=1);


addParagraph (report, "The table below lists the prediction variables selected by the random forests.");

table = as.data.frame(colnames(x)[data.vsurf$varselect.pred]);
colnames(table) = "Prediction Variables";
addTable(report,table);
addNewLine(report, n=1);

# Test 2.5.6: Illustrate the importance of variables.
addParagraph (report, "Result 2.5.6: Figure 2.5.2: Importance of Variables using Random Forests:");
addNewLine(report, n=1);

addParagraph (report, "The figure below ranks the variables through their level of importance.");
addPng(report, "Result_7_3_ Importance_of_Variables.png", width = 6, height = 6);


# Test 3: Model Selection.
addParagraph (report, "Test 3: Model Selection: Logistic Regression, Random Forests, Ridge, the Lasso");
addNewLine(report, n=1);

# Test 3.1: Ridge regression and its coefficients.
addParagraph (report, "Result 3.1: Ridge Regression and its Coefficients:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents some ridge regression coefficients.");
ridge.coef = predict (ridge.mod, type = "coefficients", s = best_lambda);
ridge.coef = ridge.coef[-nrow(ridge.coef),];
coefficient = ridge.coef;
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 3.2: the lasso and its coefficients.
addParagraph (report, "Result 3.2: the lasso and its Coefficients:");
addNewLine(report, n=1);

addParagraph (report, "The table below presents some lasso coefficients.");
lasso.coef = predict (lasso.mod, type = "coefficients", s = best_lambda);
lasso.coef = lasso.coef[-nrow(lasso.coef),];
coefficient = lasso.coef;
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);

# Test 3.3: Compare the test MSE of the 6 models: logistic regression full subset, fwd, bwd , tree, ridge
# and lasso.
addParagraph (report, "Result 3.3: Comparison of the Models: Best Subset, Forward Stepwise Subset, Backward Stepwise Subset, Random Forests, Lasso, Current");
addNewLine(report, n = 1);

# Print the coefficients estimates of the lasso model.
addParagraph (report, "The table below presents our current variables selected and their coefficients.");

coefficient = coef(current.fit);
table = cbind(names(coefficient), round(coefficient,10));
colnames(table)[1:2] = c("Variable", "Coefficient");
addTable(report,table);
addNewLine(report, n=1);


addParagraph (report, "The table below presents the test MSE of the logit lapse rates of the 7 models, estimated by the cross validation method.");

table = as.data.frame(MSE);
rownames(table) = c("reg best","reg fwd","reg bwd","random forest","ridge","lasso","current");
addTable(report,cbind(rownames(table),table));
addNewLine(report, n=1);

addParagraph (report, "Result 3.3: Figure 3.1: Comparison of Predictive Models – Logit Lapse Rates:");
addNewLine(report, n=1);

addParagraph (report, "The figure below illustrates the test MSE of the 7 models, estimated by the cross validation method.");

addPng(report, "Result_7_5_ Comparison_of_Predictive_Models.png", width = 6, height = 6);

addParagraph (report, "The table below presents the test MSE of the lapse rates of the 7 models, estimated by the cross validation method.");

table = as.data.frame(MSE_lapse);
rownames(table) = c("reg best","reg fwd","reg bwd","random forest","ridge","lasso","current");
addTable(report,cbind(rownames(table),table));
addNewLine(report, n=1);

addParagraph (report, "Result 3.3: Figure 3.2: Comparison of Predictive Models – Lapse Rates:");
addNewLine(report, n=1);

addParagraph (report, "The figure below illustrates the test MSE of Lapse Rates of the 7 models, estimated by the cross validation method.");

addPng(report, "Result_7_6_ Comparison_of_Predictive_Models.png", width = 6, height = 6);


done(report);



