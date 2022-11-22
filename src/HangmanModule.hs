{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module HangmanModule where


import           Data.Char
import           Data.Either as Either
import           Data.Foldable         as Fold
import           Prelude


---- data ----

type HangmanData = GuessData

type GuessData = (GuessWord, [Either Char Char])

type GuessWord = String
type GuessChar = Char
type IsGuessedBool = Bool

emptyGuessChar :: Char
emptyGuessChar = '_'

--- initialize ---

initializeGuessWord :: GuessWord -> GuessData
initializeGuessWord inputString = (lowerCaseInputString,  ( map initializeCharForTuple (lowerCaseInputString) ))
                                where lowerCaseInputString = stringToLower inputString

initializeCharForTuple :: GuessChar -> Either Char Char
initializeCharForTuple inputChar = Left inputChar

----- runtime ----

doGuessForWord :: Char -> GuessData -> GuessData
doGuessForWord inputGuessChar (guessWord, guessTuple) = (guessWord, fmap (doGuessPerChar inputGuessChar) guessTuple )    

doGuessPerChar :: Char -> Either Char Char -> Either Char Char
doGuessPerChar inputChar compareChar = case compareChar of
                            (Left ch) -> if ( ch == inputChar) then Right ch else Left ch
                            _ -> compareChar
            

isGuessed :: GuessData -> Bool
isGuessed ( _ , guessTuple) =  and ( fmap  isTupleTrue guessTuple ) 

isTupleTrue :: Either Char Char -> Bool
isTupleTrue compareChar = isRight compareChar 

getCharForWord :: Either Char Char -> Char
getCharForWord compareChar = case compareChar of
                                (Right ch) -> ch
                                _ -> emptyGuessChar
--- print functions

printGame :: GuessData -> String
printGame guessData             
                                | isGuessed guessData = printWordState guessData
                                | otherwise =  printWordState guessData

printWordState :: GuessData -> String
printWordState ( _ , guessTuple) = fmap getCharForWord guessTuple

---- utils ----

stringToCaps :: String -> String
stringToCaps inputString = fmap toUpper inputString

stringToLower :: String -> String
stringToLower inputString = fmap toLower inputString
