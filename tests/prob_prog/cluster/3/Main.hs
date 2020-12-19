module Main where

import Probability

import Data.CSV

xs = map read_double $ head $ read_csv "xs"

n_points = length xs

cluster_dist = do
  mean <- cauchy 0.0 1.0
  sigma <- exponential 1.0
  return (mean, sigma)

prior = do

  n <- (1+) `liftM` geometric 0.33

  clusters <- iid n cluster_dist

  let dists = [normal mean sigma | (mean,sigma) <- clusters]

  ps <- symmetric_dirichlet n 0.5

  let loggers = ["n_clusters" %=% n,
                 "weights" %=% ps,
                 "clusters" %=% clusters]

  return (ps, dists, loggers)

main = do

  (ps, dists, loggers) <- sample $ prior

  xs ~> iid n_points (mixture ps dists)

  return loggers
