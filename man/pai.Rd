% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pai.R
\name{pai}
\alias{pai}
\title{Predictive Accuracy Index}
\usage{
pai(dat, count, pred, area, other = c())
}
\arguments{
\item{dat}{data frame with the predictions, observed counts, and area sizes (can be a vector of ones)}

\item{count}{character specifying the column name for the observed counts (e.g. the out of sample crime counts)}

\item{pred}{character specifying the column name for the predicted counts (e.g. predictions based on a model)}

\item{area}{character specifying the column name for the area sizes (could also be street segment distances, see Drawve & Wooditch, 2019)}

\item{other}{vector of strings for any other column name you want to keep (e.g. an ID variable), defaults to empty \code{c()}}
}
\value{
A dataframe with the columns:
\itemize{
\item \code{Order}, The order of the resulting rankings
\item \code{Count}, the counts for the original crimes you specified
\item \code{Pred}, the original predictions
\item \code{Area}, the area for the units of analysis
\item \verb{Cum*}, the cumulative totals for Count/Pred/Area
\item \verb{PCum*}, the proportion cumulative totals, e.g. \code{CumCount/sum(Count)}
\item \code{PAI}, the PAI stat
\item \code{PEI}, the PEI stat
\item \code{RRI}, the RRI stat (probably should analyze/graph the \code{log(RRI)})!
}

Plus any additional variables specified by \code{other} at the end of the dataframe.
}
\description{
Given a set of predictions and observed counts, returns the PAI (predictive accuracy index),
PEI (predictive efficiency index), and the RRI (recovery rate index)
}
\details{
Given predictions over an entire sample, this returns a dataframe with the sorted best PAI (sorted by density of predicted counts per area).
PAI is defined as:

\deqn{PAI = \frac{c_t/C}{a_t/A}}{PAI = (ct/C)/(at/A)}

Where the numerator is the percent of crimes in cumulative t areas, and the denominator is the percent of the area encompassed.
PEI is the observed PAI divided by the best possible PAI if you were a perfect oracle, so is scaled between 0 and 1.
RRI is \code{predicted/observed}, so if you have very bad predictions can return Inf or undefined!
See Wheeler & Steenbeek (2019) for the definitions of the different metrics.
User note, PEI may behave funny with different sized areas.
}
\examples{

# Making some very simple fake data
crime_dat <- data.frame(id=1:6,
                        obs=c(6,7,3,2,1,0),
                        pred=c(8,4,4,2,1,0))
crime_dat$const <- 1
p1 <- pai(crime_dat,'obs','pred','const')
print(p1)

# Combining multiple predictions, making
# A nice table
crime_dat$rand <- sample(crime_dat$obs,nrow(crime_dat),FALSE)
p2 <- pai(crime_dat,'obs','rand','const')
pai_summary(list(p1,p2),c(1,3,5),c('one','two'))

}
\references{
Drawve, G., & Wooditch, A. (2019). A research note on the methodological and theoretical considerations for assessing crime forecasting accuracy with the predictive accuracy index. \emph{Journal of Criminal Justice}, 64, 101625.

Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. \emph{Journal of Quantitative Criminology}, 37(2), 445-480.
}
\seealso{
\code{\link[=pai_summary]{pai_summary()}} for a summary table of metrics for multiple pai tables given fixed N thresholds
}
