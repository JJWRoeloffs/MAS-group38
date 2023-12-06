module Main where

import Control.Monad (replicateM)
import Control.Lens

import System.Random.MWC (create)
import Data.Random (normal, uniform, RVar, Sampleable(sampleFrom), randomElement)
import Data.List (maximumBy)
import Data.Ord

import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Graphics.Rendering.Chart.Easy

import qualified Statistics.Distribution.Normal as D
import qualified Utils.NormalDistr as N

-- We know the lists aren't empy, so we might as well not unwrap a Maybe
unsafeMean :: (Foldable t) => t Double -> Double
unsafeMean xs = sum xs / fromIntegral (length xs)

maxIndex :: (Foldable t) => t a -> Int
maxIndex xs = length xs - 1

randomFloatList :: Int -> Double -> Double -> RVar [Double]
randomFloatList n lower upper = replicateM n $ uniform lower upper

bandid :: [Double] -> Double -> Int -> RVar Double
bandid config stdDev which = normal (config !! which) stdDev

bandidLog :: [[Double]] -> Int -> Double -> [[Double]]
bandidLog currentValues which result =
    (element which .~ (currentValues !! which) ++ [result]) currentValues

regret :: [Double] -> Int -> Double
regret config which = maximum config - config !! which

regret1 :: [Double] -> [[Double]] -> Double
regret1 config calls = sum $ zipWith
          (\len i -> fromIntegral len * regret config i)
          (map length calls) [0 .. ]

epsilonGreedy :: Double -> [[Double]] -> RVar Int
epsilonGreedy epsilon currentValues = do
    value <- uniform 0 1
    if value < epsilon then uniform 0 $ maxIndex currentValues
    else return $ snd $ maximumBy (comparing fst) (zip (map unsafeMean currentValues) [0..])

upperConfidence :: Double -> [Double] -> Double
upperConfidence v xs = max l r
    where (l, r) = N.confidenceInterval v $ N.fromSample xs

ucbStrategy :: Double -> [[Double]] -> RVar Int
ucbStrategy confidence currentValues =
    return $ snd $ maximumBy (comparing fst) (zip (map (upperConfidence confidence) currentValues) [0..])

randomStrategy :: [[Double]] -> RVar Int
randomStrategy currentValues = randomElement $ zipWith (\_ x -> x) currentValues [0 .. ]

laiRobbins :: [Double] -> Double -> Int -> Double
laiRobbins config stdDev t = logBase constant $ fromIntegral t
    where
        constant = sum $ zipWith (\val i -> regret config i / kl val) config [0 .. ]
        kl val = N.kullbackLeibler (D.normalDistr val stdDev) (D.normalDistr (maximum config) stdDev)

runner1 :: Int -> ([[Double]] -> RVar Int) -> (Int -> RVar Double) -> ([[Double]] -> Double) -> ([Double], [[Double]]) -> RVar ([Double], [[Double]])
runner1 0 _ _ _ state = return state
runner1 amount algorithm bandidInstance regretFunction state = do
    strategy <- algorithm $ snd state
    output <- bandidInstance strategy
    let spins = bandidLog (snd state) strategy output
        newState = (fst state ++ [regretFunction spins], spins)
    runner1 (amount - 1) algorithm bandidInstance regretFunction newState

runner :: Int -> ([[Double]] -> RVar Int) -> (Int -> RVar Double) -> ([[Double]] -> Double) -> ([Double], [[Double]]) -> RVar [(Int, Double)]
runner amount algorithm bandidInstance regretFunction state = do
    results <- runner1 amount algorithm bandidInstance regretFunction state
    return $ zip [0..] $ fst results

getInitState :: (Int -> RVar Double)-> Int -> Int -> RVar [[Double]]
getInitState ourBandid amount len = mapM sequence [[ourBandid i | _ <- [0..amount]] | i <- [0..len]]

plotRegrets :: [[(Int, Double)]] -> [[(Int, Double)]] -> [[(Int, Double)]] -> [[(Int, Double)]] -> IO ()
plotRegrets epsilon ucb random lowerBound = toFile def "plot.png" $ do
    layout_title .= "Regret over time"
    setColors [opaque blue, opaque red, opaque black, opaque green]
    plot (line "Epsilon-greedy" epsilon)
    plot (line "UCB" ucb)
    plot (line "Random" random)
    plot (line "Lower Bound" lowerBound)

main :: IO ()
main = do
    let nrRuns = 1000
        stdDev = 1.0
    mwc <- create
    config <- sampleFrom mwc $ randomFloatList 3 (-2.0) 2.0
    let ourBandid = bandid config stdDev
        ourEpsilon = epsilonGreedy 0.10
        ourUcb = ucbStrategy 0.60
        ourRegret = regret1 config
    initState <- sampleFrom mwc $ getInitState ourBandid 1 $ maxIndex config

    resultsEpsilon <- sampleFrom mwc $ runner nrRuns ourEpsilon ourBandid ourRegret ([0.0], initState)
    resultsUcb <- sampleFrom mwc $ runner nrRuns ourUcb ourBandid ourRegret ([0.0], initState)
    resultsRandom <- sampleFrom mwc $ runner nrRuns randomStrategy ourBandid ourRegret ([0.0], initState)
    let lowerBound = [(i, laiRobbins config stdDev i) | i <- [0..nrRuns-1]]

    plotRegrets [tail resultsEpsilon] [tail resultsUcb] [tail resultsRandom] [lowerBound]
