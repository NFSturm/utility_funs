# Variable Combination Generator Function (getVarCombs)

# Inputs:
# df: A dataframe or similar structure (data.table does not work at the moment)
# y: Name of independent variable

getVarCombs <- function(df, y) {
  if(typeof(df) != "list") stop("Input must be a dataframe or similar structure")
  if(typeof(y) != "character") stop("Dependent variable must be specified as a character string")
  
  df_vars <- df[!(names(df) %in% y)]
  var_num <- length(names(df_vars))
  vars <- names(df_vars)
  
  getCombs <- function(vars, m) {
    obj <- combn(vars, m)
    
    out <- c()
    for (combn in 1:ncol(obj)) {
      vc <- paste0(obj[,combn], collapse = "+")
      out <- append(out, vc)
    }
    formula <- c()
    for (form in out) {
      fm <- paste0(y, " ~ ", form)
      formula <- append(formula, fm)
    }
    formula  
  }  
  
  all_combs <- c()
  for (i in 1:var_num) {
    combs <- getCombs(vars, m = i)
    all_combs <- append(all_combs, combs)
  }
  
  all_combs
}

# Variable combinations must be turned into formula by using "formula('''Name of getVarComb-Output''')"