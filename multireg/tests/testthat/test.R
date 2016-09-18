iris<- data(iris) 
formel<- as.formula(Petal.Length ~ Sepal.Length + Petal.Width)
test<- lm(formula = formel, data=iris)

faith<- data(faithful)
formel_faith<- as.formula(eruptions ~ waiting)
test_faith<- lm(formula= formel_faith, data=faithful)

test_that("Testing linreg()", {
  
  expect_that(linreg, is_a("function"),
              info = "Error: linreg is not a function.")
  expect_that(all(names(formals(linreg)) %in% c("formula","data")), condition=is_true(),
              info = "Error: Argument name is wrong.")
  #expect_that(class(linreg(formula=formel, data=iris)), is_a("linreg"),
              #info = "Error: Returned value is not of linreg class.")
  expect_that(class(linreg(formula=formel_faith, data=faithful)), is_a("linreg"),
              info = "Error: Returned value is not of linreg class.")
  expect_equal(linreg(formula=formel, data=iris)$coefficients, test$coefficients,
               info="Error: Coefficients are not correct.")
  expect_equal(linreg(formula=formel_faith, data=faithful)$coefficients, test_faith$coefficients,
               info="Error: Coefficients are not correct.")
  }
)
