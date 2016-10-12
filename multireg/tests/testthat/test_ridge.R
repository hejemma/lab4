test_that("Testing ridgereg()", {
  data(iris)
  formel<-Petal.Length~Sepal.Width+Petal.Width
  ridgemodel<- ridgereg(formula=formel, data=iris, lambda=1)
  ridgemodel2<- ridgereg(formula=formel, data=iris, lambda=5)

  lmobj<- lm.ridge(formula= formel, data= iris, lambda=1)
  lmobj2<- lm.ridge(formula= formel, data= iris, lambda=5)
  
  
  expect_that(ridgereg, is_a("function"),
              info = "Error: linreg is not a function.")
  expect_that(all(names(formals(ridgereg)) %in% c("formula","data", "lambda")), 
              condition=is_true(), info = "Error: Argument name is wrong.")
  
  expect_is(ridgemodel, "ridgereg", info = "Error: Returned value is not of ridgereg class.")
  expect_equal(coef(ridgemodel)[-1], lmobj$coef, tolerance= 0.005,
               info="Error: Coefficients are not correct.")
  expect_equal(coef(ridgemodel2)[-1], lmobj2$coef, tolerance= 0.005,
               info="Error: Coefficients are not correct.")
}
)




