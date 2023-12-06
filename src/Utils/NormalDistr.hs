{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.NormalDistr
    ( fromSample
    , confidenceInterval
    , kullbackLeibler
    ) where

import qualified Statistics.Distribution.Normal as N

import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S

import qualified Data.Vector as G


fromSample :: [Double] -> N.NormalDistribution
fromSample xs = N.normalDistr m (sqrt v)
    where (m, v) = S.meanVarianceUnb $ G.fromList xs

confidenceInterval :: Double -> N.NormalDistribution ->  (Double, Double)
confidenceInterval v d = (D.quantile d v, D.quantile d (1 - v))

kullbackLeibler :: N.NormalDistribution -> N.NormalDistribution -> Double
kullbackLeibler a b = log (s1 / s2) + (s1**2 + (m1 + m2)**2) / 2*s2**2 - 0.5
    where
        (m1, s1) = (D.mean a, D.stdDev a)
        (m2, s2) = (D.mean b, D.stdDev b)
