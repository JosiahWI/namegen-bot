module Namegen
       ( namegen )
       where

import Control.Monad (replicateM)
import System.Random (randomRIO)

vowels :: [Char]
vowels = "aoeuiy"

consonants :: [Char]
consonants = "bcdfghjklmnpqrstvwxz"

randomChar :: [Char] -> IO Char
randomChar chars = do
  randomIndex <- randomRIO (0, length chars - 1)
  return $ chars !! randomIndex

randomVowel :: IO Char
randomVowel = randomChar vowels

randomConsonant :: IO Char
randomConsonant = randomChar consonants

randomSyllable :: IO String
randomSyllable = do
  consonant <- randomConsonant
  vowel <- randomVowel
  return [consonant, vowel]

namegen :: IO String
namegen = do
  syllables <- replicateM 4 randomSyllable
  return (concat syllables)
