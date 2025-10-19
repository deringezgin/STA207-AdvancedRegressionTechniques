df_residual = 21
df_total = 24
df_regression = df_total - df_residual

SSR = 345
SSE = 903
SST = SSR + SSE

MSR = SSR / df_regression
MSE = SSE / df_residual

f_statistic = MSR / MSE

pf(f_statistic, df_regression, df_residual, lower.tail = FALSE)
