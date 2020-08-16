data = c(2:40,101:140,300:340)
data= matrix(a,nrow = 4)
test_that("k-mean finds the correct clusters for k = 3", {
  expect_equal(c(rep(1,10),rep(2,10),rep(3,10)), attr(kmean(data,k=3),"clusters"))
})

