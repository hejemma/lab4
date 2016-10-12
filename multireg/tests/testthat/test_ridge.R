test_that("Testing ridgereg()", {
  #data(iris)
  #formel<-Petal.Length~Sepal.Width
  #model<- linreg(formula=formel, data=iris)
  #stderr<- sqrt(diag(model$variance_of_regression_coefficent))
  #tval<-as.vector(model$tvalue)
  
  #lmodel<- lm(formula= formel, data= iris)
  
  expect_that(ridgereg, is_a("function"),
              info = "Error: linreg is not a function.")
  expect_that(all(names(formals(ridgereg)) %in% c("formula","data", "lambda")), 
              condition=is_true(), info = "Error: Argument name is wrong.")
  
  expect_is(model, "ridgereg", info = "Error: Returned value is not of ridgereg class.")
  
  
}
)




