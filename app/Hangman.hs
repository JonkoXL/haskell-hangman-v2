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

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8       as BS8
import           Data.Char
import           Data.Either as Either
import           Data.Foldable         as Fold
import           Data.Maybe
import           Data.String
import qualified Data.String           as FS
import           Data.Time.Clock
import           Data.Time.Format
import           Prelude
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit



------------------------------------------------------

main :: IO ()
main = do
  arguments <- getArgs
  printLine $ "\n" ++ const_string_Message_Hangman_Title ++ "\n"
  printMenu

printMenu :: IO ()
printMenu = do

  menuInputOptionCharacter <- promptChar const_string_Message_Hangman_Menu const_string_Error_NoCharacterEntered

  case menuInputOptionCharacter of
    '1' -> initializeHangmanBuiltInDictionary
    '2' -> initializeHangmanHotSeat
    '3' -> die $ const_string_Message_Hangman_Exit
    _ -> printMenu

  printMenu

--- hangman ---




initializeHangmanBuiltInDictionary :: IO ()
initializeHangmanBuiltInDictionary = do
                                      word <- getRandomWord const_listOfString_words
                                      playHangManLoop $ initializeGuessWord $ word
                                      promptLine const_string_Message_PressAnyKey
                                      return ()

getRandomWord :: [String] -> IO String
getRandomWord listOfWords = do
                                      let dictionaryWordLength = (-1 :: Int) +  ( length listOfWords )
                                      currentTimeForRandomNumber <- getCurrentTimeAsInteger
                                      let randomIndex =  currentTimeForRandomNumber `mod` dictionaryWordLength
                                      let word = listOfWords !!  (randomIndex)
                                      return word

initializeHangmanHotSeat :: IO ()
initializeHangmanHotSeat = do

    wordToGuess <- promptLine const_string_Message_EnterWord
    printLine clearScreenString
    playHangManLoop $ initializeGuessWord wordToGuess

playHangManLoop :: HangmanData -> IO ()
playHangManLoop hangmanData = do
    
    printLine clearScreenString
    printHangManGame $ hangmanData

    guessCharacter <- promptChar const_string_Message_EnterCharacter const_string_Error_NoCharacterEntered
    let updatedHangmanData = doGuessForWord guessCharacter hangmanData

    if isGuessed updatedHangmanData
      then ( printLine $ const_string_Message_GameWon ++ printGame updatedHangmanData ++ "\n" )
      else   playHangManLoop updatedHangmanData

printHangManGame :: HangmanData -> IO ()
printHangManGame hangmanData = printLine $ "\n" ++  printGame hangmanData

--- utils ---


printLine :: String -> IO ()
printLine inputString = putStrLn inputString

printSameLine :: String -> IO ()
printSameLine inputString = putStr inputString

readLine :: IO String
readLine = getLine

promptLine :: String -> IO String
promptLine inputString = printLine inputString >> readLine

promptChar :: String -> String -> IO (Char) 
promptChar displayString errorString = do 
                         inputString <- promptLine displayString
                         let inputCharacter = stringToMaybeFirstChar inputString
                         if (Data.Maybe.isNothing inputCharacter )
                           then do
                              printLine errorString
                              promptChar displayString errorString
                           else
                             return $ case inputCharacter of
                                                            Just character -> character
                                                            Nothing -> '?'

stringToMaybeFirstChar :: String -> Maybe Char
stringToMaybeFirstChar inputString 
                                    | length inputString > 0 = Just (inputString !! 0)
                                    | otherwise = Nothing

                                  

printErrorIfInvalidMaybeCharacterEntered :: Maybe Char -> String -> IO ()
printErrorIfInvalidMaybeCharacterEntered character errorString
                                                  | isValidCharacter = printSameLine ""
                                                  | otherwise = printLine errorString
                                                    where 
                                                    isValidCharacter = Data.Maybe.isJust character 
              
getCurrentTimeAsInteger :: IO Int
getCurrentTimeAsInteger  = do
    currentTime <- getCurrentTime
    let seconds = formatTime defaultTimeLocale "%s" (currentTime)
    return $ read seconds


--- consts ---

const_string_Message_Hangman_Title :: String
const_string_Message_Hangman_Title = "[ Hangman v1.0 ] "

const_string_Message_Hangman_Menu :: String
const_string_Message_Hangman_Menu = "Welcome to Hangman!\nPlease choose whether you want to\n1) Play against the computer, using a built-in dictionary\n2) Play in hotseat mode (enter a word by yourself)\n3) Exit\nOption: "

const_string_Message_Hangman_Exit :: String
const_string_Message_Hangman_Exit = "Thank you for playing!"

const_string_Message_EnterWord :: String
const_string_Message_EnterWord = "> Give word to guess: "

const_string_Message_EnterCharacter :: String
const_string_Message_EnterCharacter = "> Give character to guess: "

const_string_Message_GameWon :: String
const_string_Message_GameWon = ">>> You won the game!\nThe word was: "

const_string_Message_PressAnyKey :: String
const_string_Message_PressAnyKey = "Press any key to continue"
const_string_Message_Done :: String
const_string_Message_Done = "Done"


const_string_Error_NoCharacterEntered :: String
const_string_Error_NoCharacterEntered = "[error] 'Please enter a valid character!\n"

const_string_Error_Hangman_DictionaryFileDoesNotExist :: String
const_string_Error_Hangman_DictionaryFileDoesNotExist = "[error] A file named 'words.txt' does not exist in the Hangman application directory!"


const_string_File_Hangman_Dictionary :: String
const_string_File_Hangman_Dictionary = "words.txt"

const_listOfString_words :: [String]
const_listOfString_words = ["apple","banana","fruit","car","train","airplane","engineer","nurse","soldier","cake","pizza","bread","bird","chicken","tiger"]

clearScreenString :: String
clearScreenString = "\n\n\n\n\n\n\n\n\n\n\n\n"

