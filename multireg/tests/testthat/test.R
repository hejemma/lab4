test_that("Testing linreg()", {
  data(iris)
  formel<-Petal.Length~Sepal.Width
  model<- linreg(formula=formel, data=iris)
  stderr<- sqrt(diag(model$variance_of_regression_coefficent))
  
  lmodel<- lm(formula= formel, data= iris)
      
  expect_that(linreg, is_a("function"),
              info = "Error: linreg is not a function.")
  expect_that(all(names(formals(linreg)) %in% c("formula","data")), 
              condition=is_true(), info = "Error: Argument name is wrong.")
  
  expect_is(model, "linreg", info = "Error: Returned value is not of linreg class.")
  
  expect_equal(model$coefficients, as.matrix(lmodel$coefficients), 
               info="Error: Coefficients are not correct.")
  
  expect_equal(model$residuals, as.vector(lmodel$residuals),
               info="Error: Coefficients are not correct.")
  
  expect_equal(model$fitted, as.matrix(lmodel$fitted.values), 
               info="Error: Coefficients are not correct.")
  }
)

