print.wmwdiagnose <-
function(x,...) {
  cat("\nEstimation of p112=Pr(max(X21,X22)<X1) and p112=Pr(max(X11,X12)<X2), and Var(MW) \n\n")
  cat("  p112 = ",x$p112,"\n")
  cat("  p221 = ",x$p221,"\n")
  cat("  Estimated Var(MW) = ",x$var,"\n")
  cat("  Null Var(MW) = ",(x$n1+x$n2+1)/(x$n1*x$n2*12),"\n")
  ratio<-x$var*(x$n1*x$n2*12)/(x$n1+x$n2+1)
  cat("  Ratio Estimated / Null = ",round(ratio,2),"\n\n")
  if(ratio<1) cat("  WMW test may be too conservative\n\n")
  if(ratio>1) cat("  WMW test may be too liberal\n\n")
}

