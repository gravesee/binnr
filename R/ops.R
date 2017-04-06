setMethod("+", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$expand(e2)
})

setMethod("-", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$collapse(e2)
})

setMethod("!=", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$neutralize(e2)
})
