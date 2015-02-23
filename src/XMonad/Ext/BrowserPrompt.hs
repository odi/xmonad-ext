{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Ext.BrowserPrompt
-- Copyright   :  (C) 2015 Oliver Dunkl
-- License     :  BSD3
--
-- Maintainer  :  Oliver Dunkl <oliver.dunkl@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for browser prompts.
--
-----------------------------------------------------------------------------
module XMonad.Ext.BrowserPrompt
       ( Browser(..)
       , urlHistoryPrompt
       , browserHistory
       ) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple
import Text.Regex.Posix ((=~))

-- | BrowserHistory <url>
data BrowserHistory = BrowserHistory String

data BrowserPrompt = BrowserPrompt

instance FromRow BrowserHistory where
  fromRow = BrowserHistory <$> field

instance XPrompt BrowserPrompt where
  showXPrompt BrowserPrompt = "open url: "
  commandToComplete _ c     = c
  nextCompletion _          = getNextCompletion

urlHistoryPrompt :: XPConfig -> (IO [BrowserHistory]) -> X ()
urlHistoryPrompt conf hs = do
  hist    <- io hs           -- get history
  browser <- io getBrowser   -- get browser from $BROWSER
  mkXPrompt BrowserPrompt conf (mkComplListHist $ map (\(BrowserHistory x) -> x) hist) $
    (\x -> spawn $ unwords [ browser, x ])

mkComplListHist :: [String] -> String -> IO [String]
mkComplListHist ss p = return $ mapMaybe (\x -> match x p) ss
  where
    match s p = if (upperStr s =~ upperStr p) then Just s else Nothing
    upperStr s = map toUpper s

-- | Datastructure for browsers.
data Browser = Firefox    -- ^ firefox
             | Conkeror   -- ^ conkeror

browserHistory :: Browser -> (String -> IO [BrowserHistory])
browserHistory Firefox  = browserHistoryFirefox
browserHistory Conkeror = browserHistoryConkeror

browserHistoryFirefox :: String -> IO [BrowserHistory]
browserHistoryFirefox = browserHistoryConkeror

browserHistoryConkeror :: String -> IO [BrowserHistory]
browserHistoryConkeror db = do
  conn <- open db
  let q = "select url from moz_places where title != '' and visit_count > 1 order by visit_count desc;"
  query_ conn q >>= return
