logar.optfn <-
function(Beta,x.bar){
  (x.bar+Beta/((1-Beta)*log(1-Beta)))^2
}

