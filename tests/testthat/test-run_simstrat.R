context("run example simulation")

test_that("running Simstrat simulation", {
  sim_folder <- system.file('extdata', package = 'SimstratR')
  status = run_simstrat(sim_folder, verbose = TRUE)
  
  expect_true(is.character(status))
  
  expect_true(file.exists(file.path(sim_folder, 'Results/T_out.dat')))
  
})

test_that("running Simstrat simulation - verbose = FALSE", {
  sim_folder <- system.file('extdata', package = 'SimstratR')
  status = run_simstrat(sim_folder, verbose = FALSE)
  
  expect_true(status == 0)
  
  expect_true(file.exists(file.path(sim_folder, 'Results/T_out.dat')))
  
})