test_that("stops after one iteration when initial point is minimum",{
  expect_equal(do_iterations(num_iter = 100, x1_init = 1, x2_init = 1, tol = 0.1)[4], 1)
})

test_that("iterations greater than one when initial point is not minimum",{
  expect_gt(do_iterations(num_iter = 100, x1_init = 2, x2_init = 2, tol = 0.1)[4], 1)
})

test_that("iterations do not exceed num_iter",{
  expect_lte(do_iterations(num_iter = 100, x1_init = 5, x2_init = 7, tol = 0.1)[4], 100)
})

for (i in seq(5:10)) {
  test_that("surrogate function satisfies the descent property",{
    expect_true(sum(diff(do_iterations(num_iter = i,2,2, tol = 0.1)[5:(5 + i -1)]) < 0) == ((5+i-1)-5))
  })
}


expect_error(do_iterations(num_iter = 0, x1_init = 1, x2_init = 1, tol = 0.1))
expect_error(do_iterations(num_iter = -1, x1_init = 1, x2_init = 1, tol = 0.1))
expect_error(do_iterations(num_iter = 1, x1_init = 1, x2_init = 1, tol = -0.1))
expect_error(do_iterations(num_iter = -1, x1_init = 1, x2_init = 1, tol = -0.1))
