ZIP.optfn <-
function(pars,x.bar,n,n0) {
  # pars[1]: p ; pars[2]=lambda
  (1-x.bar/pars[2]-pars[1])^2+(pars[2]/(1-exp(-pars[2]))-x.bar/(1-n0/n))^2
}

