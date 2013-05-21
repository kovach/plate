module Waves where

-- WAV support
import Data.WAVE
import Data.List

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

un = undefined

pp (WAVEHeader channels fr bits frames) = 
  concat $ intersperse "," $ [show channels, show fr, show bits, show frames]

reverse' = foldl' (flip (:)) []
waveTest = do
  -- read wav file
  wavFile <- getWAVEFile "mouthbreather.wav"
  let header = waveHeader wavFile
  putStrLn . pp $ header

  let foo = replicate 6000000 1
  let x = reverse foo
  --print $ take 1 x

  let samples = waveSamples wavFile
  let samples' = reverse' $ take 1000000 $ samples
  putWAVEFile "breathermouth.wav" (WAVE header samples)

