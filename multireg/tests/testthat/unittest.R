test_that("Testing function linreg()", {
  iris<- data(iris) 
  formel<- as.formula(Petal.Length ~ Sepal.Length + Petal.Width)
  test<- lm(formula = as.formula(Petal.Length ~ Sepal.Length + Petal.Width), data=iris)
  
  faith<- data(faithful)
  formel_faith<- as.formula(eruptions ~ waiting)
  test_faith<- lm(formula= formel_faith, data=faithful)
  
  expect_that(linreg, is_a("function"),
              info = "Error: linreg is not a function.")
  expect_that(all(names(formals(linreg)) %in% c("formula","data")), condition=is_true(),
              info = "Error: Argument name is wrong.")
  expect_that(class(linreg(formula=formel, data=iris)), is_a("linreg"),
              info = "Error: Returned value is not of linreg class.")
  expect_that(class(linreg(formula=formel_faith, data=faithful)), is_a("linreg"),
              info = "Error: Returned value is not of linreg class.")
  expect_equal(linreg(formula=formel, data=iris)$coefficients, test$coefficients,
               info="Error: Coefficients are not correct.")
  expect_equal(linreg(formula=formel_faith, data=faithful)$coefficients, test_faith$coefficients,
               info="Error: Coefficients are not correct.")
  }
)

test_that("Testing method coefficients()", {
  
  
  
  }
)