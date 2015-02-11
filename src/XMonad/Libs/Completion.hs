module XMonad.Libs.Completion
   (
     windowPromptGoto',
     windowPromptBring',
   ) where

import           Data.Char (isLetter)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)

import           Text.Regex.Posix

import qualified XMonad.StackSet as W
import           XMonad
import           XMonad.Prompt
import           XMonad.Actions.WindowBringer

type Pattern = String

data WindowPromptRegEx = Goto | Bring
instance XPrompt WindowPromptRegEx where
    showXPrompt Goto      = "Go to window: "
    showXPrompt Bring     = "Bring window: "
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

windowPromptGoto', windowPromptBring' :: XPConfig -> X ()
windowPromptGoto'  = doPrompt Goto
windowPromptBring' = doPrompt Bring

doPrompt :: WindowPromptRegEx -> XPConfig -> X ()
doPrompt t c = do
  a <- case t of
         Goto  -> fmap gotoAction  windowMap
         Bring -> fmap bringAction windowMap
  wm <- windowMap
  mkXPrompt t c (compList wm) a

    where
      winAction a m    = flip whenJust (windows . a) . flip M.lookup m
      gotoAction       = winAction W.focusWindow
      bringAction      = winAction bringWindow

      compList m s = return $ mkCompListFuzzy (map fst $ M.toList m) s

mkCompListFuzzy :: [String] -> Pattern -> [String]
mkCompListFuzzy ss p = mapMaybe (\x -> match x (filter isLetter p)) ss
  where
    fuzzyMatch s p = s =~ (intercalate ".*?" $ map (:[]) p)
    match s p      = if (fuzzyMatch s p) then Just s else Nothing
