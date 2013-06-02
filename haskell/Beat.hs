module Beat where

import List

import Data.Maybe

un = undefined

type Time = Double
type Error = [Maybe Time]

type Input = [Time]

type Interval = Time

type Ratio = (Int, Int)
type Subdivision = Int
type Offset = Ratio
type Beat = (Interval, Subdivision, Offset)
type Beat1 = (Interval, Time) -- interval, offset
type Index = Int


differences :: Num a => [a] -> [a]
differences [] = []
differences [a] = []
differences (x : y : zs) = (y-x) : differences (y : zs)

mean xs = go 0.0 0 xs
  where
    go sum len [] = sum / fromIntegral len
    go sum len (x:xs) = go (sum+x) (len+1) xs
cov :: [Time] -> Double
cov hits =
  let n = fromIntegral $ length hits in
  sum (zipWith (*) [0..] hits) - 
  sum [0.0..n-1] * sum hits / n
var :: [Time] -> Double
var hits =
  let n = fromIntegral $ length hits in
  (n*n*n - n) / 12
beta hits = cov hits / var hits
linreg hits =
  let b = beta hits in
  b

-- takes linear sequence of times and outputs the multiplier
reg :: [Time] -> Interval
reg hits =
  let diffs = differences hits in
  mean diffs

getError :: Interval -> [Time] -> Error
getError int hits =
  map Just $
  map (uncurry (-)) $
  zip (iterate (+int) 0.0)
      hits

rError hits = getError (reg hits) hits
maxE = mapMaybe id

-- does not update "interval" as it finds matches
extrapolate :: Interval -> Input -> Index -> ([Index], Error)
extrapolate int hits start = go start 0.0 int [] (drop start hits)
  where
    threshold = 0.125
    go index position int matches [] =
      let (indices, hits) = unzip $ reverse matches in
      (indices, getError int hits)
    go index position int matches (hit:hits) =
      let closest =
            head $
            dropWhile (\t -> t < hit + threshold && abs (t-hit) > threshold) $
            iterate (+ int) position
      in
        if abs (closest - hit) < threshold then
          go (index+1) closest int ((index, hit) : matches) hits
        else
          go (index+1) closest int (matches) hits

toFraction :: Int -> Double -> Maybe Ratio
toFraction res f =
  let c = round (fromIntegral res * f) in
  if abs (f - fromIntegral c / fromIntegral res) < 1 / (4 * fromIntegral res) then
    Just (c, res)
  else
    Nothing

offset :: Time -> [Time] -> [Time]
offset t = map (subtract t)

type Unmatched = [Index]
beatify :: Input -> ([Beat1], Unmatched)
beatify input | length input < 2 = error "input too short"
beatify input@(x:y:_) = go (Just (0, 1)) [] [] where
  get ind = input !! ind
  delta ind0 ind1 =
    get ind1 - get ind0

  indices = [0..length input-1]
  incr i0 i1 = if i1 == length input - 1
               then
                 if i0 == length input - 1
                 then
                   Nothing
                 else
                   Just(i0+1, 0)
               else Just (i0, i1+1)

  go :: Maybe (Index, Index) -> [Index] -> [Beat1] -> ([Beat1], Unmatched)
  go Nothing matched beats = (beats, (diff indices matched))
  go (Just (ind0, ind1)) matched beats =
    let (matches, error) = extrapolate (delta ind0 ind1) (offset (get ind0) input) ind0
        matched' = merge matches matched
        unmatched = diff indices matched'
        nun = length unmatched
    in
      if length matches <= 2 then
        go (incr ind0 ind1) matched beats
      else
        go (incr ind0 ind1) matched' ((new_delta, get ind0) : beats)


test = [0.0, 1.0, 2.1, 3.1, 3.9, 5.0, 5.9, 6.9, 8.0]
test2 = [2.1, 3.1, 3.9, 5.0, 6.1, 7.25, 8.0, 8.95]
test3 = [0.0, 0.25, 1.0, 2.0, 3.0]