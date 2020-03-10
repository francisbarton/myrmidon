\name{insertAssPipe}
\alias{insertAssPipe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assignment pipe add-in
}
\description{
Inserts assignment pipe at cursor position in RStudio. Ripe for being set up with a keyboard shortcut.
See https://rstudio.github.io/rstudioaddins/ for documentation and hints.
}
\usage{
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  none
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Francis Barton
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
insertAssPipe <- function() {
  rstudioapi::insertText(" \%<>\% ")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~utlities } % use one of  RShowDoc("KEYWORDS")
