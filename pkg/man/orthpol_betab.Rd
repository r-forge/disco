\name{orthpol_betab}
\Rdversion{1.1}
\alias{orthpol_betab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orthpol_betab(k, ntrials, a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{ntrials}{
%%     ~~Describe \code{ntrials} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(k,ntrials,a,b) {
	n<-ntrials
	if (k==0) {
        h<-function(x){
          y<-1
          return(y)
        }
      }
	else if (k==1) {
        h<-function(x){
          y<--(-a*x-x*b+a*n)/(a+b)/(n/(a+b)^2/(a+b+1)*a*b*(a+b+n))^(1/2)
          return(y)
        }
      }
	else if (k==2) {
        h<-function(x){
          y<-1/2*(-a^2*n+a^2*n^2+a^2*x^2-2*a^2*x*n+a^2*x-4*a*x*n+a*n^2+3*a*x^2+2*a*x^2*b-2*x*b*a*n+a*x-a*n+x^2*b^2-2*x*n*b+2*x^2-2*x*n+3*x^2*b-x*b-b^2*x)/(a+b+1)/(a+b+2)*2^(1/2)/(b*(1+b)*(1+a)*a*(a*n-a+n*b-b+n^2-1)*n*(a+b+n)/(a+b+2)^2/(a+b)/(a+b+3)/(a+b+1)^2)^(1/2)
          return(y)
        }
      }
	else if (k==3) {
        h<-function(x){
          y<--1/6*6^(1/2)/((1+b)^3*(b+2)*b^5*(a+2)*(1+a)^3*a^5*(n^2+a*n+n*b-2*a-2*b-4)*(a*n-a+n*b-b+n^2-1)^3*(a+b+n)^5*n^5/(a+b+5)/(a+b+4)^2/(a+b+3)^4/(a+b+2)^6/(a+b)^7/(a+b+1)^7)^(1/2)*(a+b+n)^2*b^2*a^2*n^2*(25*a^2*n^2-18*n*a^3+16*a*b*n^2+10*a*b^2*n^2+50*a^2*n^2*b-10*a^2*n*b^2-26*a^3*n*b-4*n*a*b^2-24*n*b*a^2+5*a^5*n^2-4*a^5*n^3-8*a^3*n*b^2-12*a^4*n*b+5*a^4*n^2*b^2+5*a^5*n^2*b-4*a^5*n^3*b-2*a^5*n*b-2*b^2*a^4*n+28*b*a^4*n^2+b^2*a^4*n^4+5*a^2*b^2*n^4-4*a^4*b^2*n^3+2*a^4*n^4*b+a^4*n^4+4*a^3*b^2*n^4-3*a^3*b*n^4-2*b^5*x*n+20*a^3*b^2*n^2-19*a^4*n^3*b-16*a^3*b^2*n^3-3*b^5*x^2+51*a^2*x^2-51*x^2*b^2+40*a^3*b^2*x^3-20*a^2*b^2*n^3-8*a*b^2*n^3+2*a*n^3-3*a^2*n^3+2*x*b^5+18*x*b^3+37*a^3*n^2-10*n^2*x^3*b^3+36*n^3*x^2*b^2+26*b*x*n^2+25*a^2*b^2*n^2+9*n^2*b^4*x^2-8*a^2*b*n^4-4*a*b*n^4-12*a^4*n^3*x-10*a^4*n*x^3+21*a^4*n^2*x^2-7*a^3*n^4-13*a^2*n^4+12*b^4*x^2*n-76*a^2*x*n-20*b^3*x*n^2+2*b^2*x*n^2+b^5*x^3-32*a^3*b*n^3+57*a^3*b*n^2+150*a*n^2*x^2*b-100*a*n*x^3*b-18*x^2*b+14*b^2*x-16*a^3*n^3-4*a*n-36*x^2*n-10*n*b^4*x^3-23*a^2*b*n^3+6*a*n^2-14*a^2*n-204*a^2*b^2*x^2*n-15*a^2*n^3*b^3*x+85*a^2*x^3+51*a^3*x^2+18*a^3*x-6*a*b*n^3+66*n^3*x^2*b-35*n^2*x^3*b^2+2*a^5*x*b+9*a^5*x*n^2+4*a*x+4*x*b-51*b^3*x^2-21*b^4*x^2-3*a^2*b^4*x*n^2+23*a^4*n^2-2*a^5*n+11*b^4*x^3-3*a^5*n^3*x*b-8*a^5*x*n*b+3*b^5*x^2*n-10*a^4*n-15*a^4*n^3-12*n^4*x+4*a^3*n^5*b-50*n*b^2*x^3+a^4*n^5*b+14*b^3*x*n-9*a^3*b^3*x^2*n-6*n^4*b^2*x+15*a*b^4*x^2*n-11*a*b^4*x*n^2-a*n^2*x^3*b^4-3*a*b^5*x^2+3*a^5*n^2*x^2*b-a^5*n*x^3*b+4*a^3*n^5+a^4*n^5+5*a^2*n^5+a^5*b*n^4+15*a*b^4*x^3-27*a*b^4*x^2+12*a*x*b^4-8*b^4*x*n^2+163*a^3*b*x*n^2+30*a^3*n^3*x^2*b+2*n^5*a+a^5*n^4+12*x*n^2+2*a*n^5*b-35*n*x^3*b^3+54*n^2*x^2*b^3+2*a*n^4*b^2-a*n*b^5*x^3-129*a^2*x*n*b-129*a^2*n*x^3*b+174*a^2*n^2*x^2*b-50*n^2*x^3*b-13*a*n^2*x^3*b^3+12*a*n^2*b^4*x^2-20*a*b^3*x*n^2-14*a*n*b^4*x^3+10*x*b^4+5*a^2*n^5*b-2*b^4*x*n+109*a*b*x*n^2+3*a*b^5*x^2*n+141*a*n^3*x^2*b+74*b*x^3+85*b^2*x^3-129*a*n*b^2*x^3-94*a*n^2*x^3*b+24*x^3-8*a*b*n+26*b^2*x*n-51*a*n^4*x*b-3*a^3*n^4*b^2*x-6*a^3*n*x^3*b^3+9*a^3*n^2*x^2*b^3+14*a^2*x-78*a*n^3*x*b^2+45*x^3*b^3-24*a*x*n-6*a*n^4-3*a^3*n^3*b^3*x-93*a^3*b^2*x^2*n-3*a^2*n^2*x^3*b^3-n^2*x^3*b^4+213*a*n^2*x^2*b^2-291*a*b*x^2*n+a^5*x^3+185*a^2*x^3*b-126*a^3*x^2*n+185*a*x^3*b^2+97*a^3*x*n^2+4*a^4*b^2*x^3-24*n^2*x^3-24*n^3*x*b-36*n^3*x*b^2+2*a^5*x+3*a^5*x^2+6*n^3*x^2*b^3-15*a^4*b^2*x^2*n+8*a^2*x*b^3-18*n^4*x*b-a*b^4*x*n+9*a*n^3*x^2*b^3-27*a*b^3*x^2*n+79*a*x^3*b^3-6*a^2*b^4*x^2+2*a^2*x*b^4+40*a^2*x^3*b^3-345*a^2*x^2*b*n+36*n^3*x^2+3*a^3*b^3*x*n^2+45*a^3*x^3+9*a^5*b*x*n^2+18*a*x^2-12*n^3*b^3*x+24*a^3*x^2*b^2+8*a^3*b^2*x+79*a^3*b*x^3+3*a^2*n^2*b^4*x^2+3*a^2*b^3*x*n^2-4*a^2*n*b^4*x^3+a*b^5*x^3+6*a^4*x^2*b^2+2*a^4*b^2*x+3*a^2*n^3*x^2*b^3-27*a^2*b^3*x^2*n+4*a^2*b^3*x*n-12*a^2*n^4*b^2*x-37*a^2*n*x^3*b^3+48*a^2*n^2*x^2*b^3+a^2*b^4*x*n+12*a*n^3*x+64*a^4*b*x*n^2+3*a^4*n^3*x^2*b-4*a^4*n*b^2*x^3-88*a^3*x*n-9*a^3*n^3*x+27*a^4*x^2*b-45*a^4*x^2*n+12*a^4*x*b-24*a*n*x^3+18*a*n^2*x^2-3*a^4*n^4*x+49*a^4*x*n^2-a^4*n^2*x^3+3*a^4*n^3*x^2+51*a^3*n^2*x^2-35*a^3*n*x^3+15*a^4*b*x^3-a^4*n^2*x^3*b-11*a^4*b^2*x*n-3*a^4*n^4*x*b+9*a^4*n^2*x^2*b^2			+a^5*b*x^3-32*x*b*a*n-171*a^2*x^2*n+69*a^2*x^2*b+28*a^2*x*b+102*a^2*n^3*x^2*b-112*a^2*n*b^2*x^3-56*a^2*n^2*x^3*b-50*a^2*b^2*x*n-66*a^2*n^3*x*b^2-51*a^2*n^4*x*b-39*a^2*n^4*x+89*a^2*x*n^2-6*a^5*b*x^2*n+69*a^2*n^3*x^2-35*a^2*n^2*x^3-69*a*x^2*b^2+99*n^2*x^2*b^2+28*a*b^2*x+194*a*b*x^3-60*a^4*b*x^2*n+6*a^3*n^3*x^2*b^2+69*a^3*b^2*x*n^2-3*a^3*n^2*x^3*b^2-18*a^4*n^3*x*b-6*a^4*n^3*x*b^2+11*a^4*x^3+6*a^3*x^3*b^3+12*a^2*n^3*x-75*a*b^3*x^2-37*a^3*n*b^2*x^3-13*a^3*n^2*x^3*b-44*a^3*b^2*x*n-30*a^3*n^3*x*b^2-21*a^3*n^4*x*b-24*a^2*b^3*x^2+136*a^2*b^2*x^3+36*a^2*n^3*x^2*b^2+108*a^2*b^2*x*n^2-24*a^2*n^2*x^3*b^2+12*x*n*b-55*a^4*x*n*b+30*a^4*n^2*x^2*b-14*a^4*n*x^3*b-39*a^2*n^3*x*b+2*a*x*b^5-n*b^5*x^3-50*a^2*n*x^3+51*a^2*n^2*x^2-2*a*b^5*x*n+3*a^5*x^2*b-6*a^5*x^2*n-24*a*n^3*b^3*x+74*a*x^3+75*a^3*x^2*b+26*a^3*x*b-18*a^3*n^4*x+63*a^3*n^2*x^2*b^2-210*a^3*b*x^2*n-102*b*x^2*n-84*b^2*x^2*n-9*b^3*x^2*n-10*a^3*n^2*x^3+24*a^3*n^3*x^2-44*a^4*x*n+4*a^2*b^4*x^3-36*a^3*n^3*x*b+54*n^2*x^2*b+10*a^4*x+21*a^4*x^2+191*x*b*a^2*n^2+20*a^2*b^2*x+26*a*x*b^3-120*a*x^2*n+168*a^2*n^2*x^2*b^2+66*a*n^3*x^2*b^2+56*a*b^2*x*n^2-56*a*n^2*x^3*b^2+18*a*b^3*x*n-15*a*n^4*b^2*x-66*a*n*x^3*b^3+93*a*n^2*x^2*b^3-24*n*x^3*b-132*a^3*x*n*b+105*a^3*n^2*x^2*b-66*a^3*n*x^3*b+16*a*x*b+3*a^2*b^4*x^2*n-36*a*n^4*x+44*a*x*n^2-8*a^5*x*n-50*a*n^2*x^3+9*a*b^2*x*n-210*x^2*b^2*a*n-42*a*n^3*x*b-3*a^5*n^3*x-a^5*n*x^3+3*a^5*n^2*x^2+84*a*n^3*x^2+15*a^4*b^2*x*n^2)/(a^6+6*b*a^5+9*a^5+15*b^2*a^4+26*a^4+45*a^4*b+90*a^3*b^2+20*a^3*b^3+24*a^3+104*b*a^3+156*b^2*a^2+72*b*a^2+90*a^2*b^3+15*a^2*b^4+72*a*b^2+104*a*b^3+6*a*b^5+45*b^4*a+b^6+26*b^4+9*b^5+24*b^3)/(a+b+2)^2/(a+b+3)/(a+b+1)^3
          return(y)
        }
      }
	else if (k==4) {
        h<-function(x){
          y<-1/12*(342*b*x^4+119*b^2*x^4+4*a^3*b*x^4+18*a^3*x^4+18*b^3*x^4+54*a^2*b*x^4+54*a*b^2*x^4+a^4*x^4+b^4*x^4+238*a*b*x^4+342*a*x^4+119*a^2*x^4+6*a^2*b^2*x^4+4*a*b^3*x^4+360*x^4+6*a^4*x^3-12*b^3*n*x^3-60*a^3*n*x^3-282*b^2*x^3+72*a^2*b*x^3+12*a^3*b*x^3-12*a^3*n*b*x^3-564*n*b*x^3-72*b^3*x^3-332*a^2*n*x^3+282*a^2*x^3-4*a*b^3*n*x^3-6*b^4*x^3-720*n*x^3+72*a^3*x^3-476*a*n*b*x^3-72*a*b^2*x^3-12*a*b^3*x^3-4*a^4*n*x^3-84*a*b^2*n*x^3+360*a*x^3-144*b^2*n*x^3-132*a^2*n*b*x^3-360*b*x^3-12*a^2*b^2*n*x^3-804*a*n*x^3+229*a^2*x^2+18*a*b^2*x^2+72*a^3*n^2*x^2-180*a^3*n*x^2+198*a*x^2-648*a*n*x^2+8*a^3*b*x^2-594*a^2*n*x^2+198*b*x^2+432*b*n*x^2+36*b^3*n*x^2+12*a*b^3*n*x^2+102*a^2*n^2*b*x^2-18*a^4*n*x^2+8*a*b^3*x^2+229*b^2*x^2+90*b^3*x^2+12*a^3*n^2*b*x^2+318*a^2*n^2*x^2+72*x^2+36*b^2*n^2*x^2+432*n^2*x^2+282*a*n^2*b*x^2+6*a^2*b^2*n*x^2+11*b^4*x^2-6*a^2*b^2*x^2-24*a^3*b*n*x^2+252*n^2*b*x^2+252*b^2*n*x^2+90*a^3*x^2+26*a*b*x^2+6*a^4*n^2*x^2-114*a^2*b*n*x^2+18*a^2*b*x^2+11*a^4*x^2+30*a*b^2*n^2*x^2+102*b^2*a*n*x^2+612*a*n^2*x^2+6*a^2*b^2*n^2*x^2+18*a*b*n*x^2-24*a^2*n^3*b*x-6*b^4*x-132*n*b*x-30*a*b^2*n^2*x-66*b^2*x-156*a^3*n*x-18*a*b^2*n*x-36*b^2*n^2*x+324*a*n^2*x+6*a^2*b^2*n*x-108*b^2*n*x-6*a^2*b^2*n^2*x-36*a^3*n^3*x-24*b^3*n*x-36*b*x-24*n^3*b*x+18*a^4*n^2*x-4*a^4*n^3*x+42*b*a^2*n^2*x+66*a^2*x+12*a^3*b*n^2*x-18*a^2*n*b*x-264*a*n*x-72*n*x-116*a^2*n^3*x-22*a^4*n*x+36*a*x-8*a^3*n*b*x-44*a*n^3*b*x-72*n^3*x-156*a*n^3*x-18*a*b*n^2*x-26*b*a*n*x+144*a^3*n^2*x-4*a^3*n^3*b*x+6*a^4*x-8*a*b^3*n*x-36*b^3*x+378*a^2*n^2*x+36*a^3*x-350*a^2*n*x-108*b*n^2*x+11*a^4*n^2+11*a^2*n^4+a^4*n^4-36*a*n^3-6*a^4*n-6*a^4*n^3+6*a*n^4+66*a*n^2+66*a^3*n^2-36*a^3*n+6*a^3*n^4-36*a^3*n^3+121*a^2*n^2-66*a^2*n-36*a*n-66*a^2*n^3)*6^(1/2)/(b*(6+6*b^2+b^3+11*b)*n*(11*n-6*n^2+n^3-6)*a*(36*a+36*n+36*b+4*a^6*n+4*a^3*n*b^3+12*a^4*b^2*n+198*a*n+12*a^5*n*b+36*n^3+30*a^3*n^3+386*a^2*n+80*a^2*n^3+6*a^3*b^2*n^2+24*a^2*b^3*n+12*a^4*b*n^2+108*a^4*b*n+240*b*n^2*a^2+132*a^2+42*a^5*n+108*n^2*b+90*b^2*a^3*n+6*a^5*n^2+6*b^4+458*b*n*a+24*b^3*n+36*n^2*b^2+24*n^3*b+6*n^4+193*a^3+270*a*n*b^2+386*b*a^2+229*a*n^2+198*a*b+66*b^2+185*a^3*n^2+360*a^3*n+270*b*n^2*a+144*a^4+108*n*b^2+132*b*n+229*a*b^2+90*a*n^3+66*n^2+44*a*n^3*b+66*a*n^2*b^2+600*a^2*n*b+300*a^2*n^2+370*b*n*a^3+4*b*n^3*a^3+90*b*n^2*a^3+58*a^5+24*b*n^3*a^2+12*a^6+44*a*b^3*n+240*a^2*n*b^2+36*a^2*n^2*b^2+6*a^2*n^4+4*a^4*n^3+a^3*n^4+54*a^4*n^2+174*a^4*n+300*a^2*b^2+360*a^3*b+36*b^3+a^7+6*a^5*b^2+90*a*b^3+11*a*n^4+4*a^4*b^3+80*a^2*b^3+54*a^4*b^2+42*a^5*b+185*a^3*b^2+11*a*b^4+a^3*b^4+174*a^4*b+30*a^3*b^3+6*b^4*a^2+4*a^6*b)/(a^2+2*a+2*a*b+2*b+b^2)/(b+7+a)/(a+b+1)/(a+b+3)^2/(a+b+4)^2/(a+b+5)^2/(b+6+a)^2)^(1/2)/(a+b+3)/(a^2+2*a*b+9*a+20+9*b+b^2)/(b+6+a)
          return(y)
        }
      }
	return(h)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
