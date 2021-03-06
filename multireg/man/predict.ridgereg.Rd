% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictridgereg.R
\name{predict.ridgereg}
\alias{predict.ridgereg}
\title{Extract Fitted Values from linreg object.}
\usage{
\method{predict}{ridgereg}(object, newdata, ...)
}
\arguments{
\item{object}{An object of class \code{ridgereg}}

\item{newdata}{If another data set with same variables is wished to use for prediction}

\item{...}{optional arguments that can be passed on the function.}
}
\value{
A numeric vector of fitted values extracted from the model in \code{\link{ridgereg}}.
}
\description{
\code{pred} returns the fitted values of the regression model calculated by the \code{\link{ridgereg}} function.
}
\examples{
x<- ridgereg(formula= Sepal.Width ~ Sepal.Length + Petal.Width, data= iris)
predict(x)
}

