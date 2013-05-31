-- | Module     : XMonad.Libs.Sound
-- | Copyright  : (c) Oliver Dunkl
-- | License    : BSD (see LICENSE)
--
-- | Maintainer : Oliver Dunkl <oliver.dunkl@gmx.at>
--
-- | This module is responsible for working with sounds in 'XMonad'.
module XMonad.Libs.Sound (
  toggleVol,      -- ^ toggles the volume control mute|unmute
  incVol,         -- ^ increases the volume
  decVol,         -- ^ decreases the volume
  fireVolPrompt   -- ^ prompt for a volume value
  ) where

import System.Directory (doesFileExist)

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input

-- | Program that can set the volume.
amixer :: String
amixer = "amixer sset Master "

-- | Status file of the mixer. If the given file is available the volume
-- control is set to 'unmute' otherwise it is set to 'mute'.
volStatFile :: FilePath
volStatFile = "/home/odi/data/run/vol-on"

-- | Toggles between 'mute' and 'unmute' of the mixer.
-- It take the status of the mixer from the function 'volStatFile'. If the
-- file which is given in that function is available the mixer is 'unmute'
-- otherwise it is 'mute'.
toggleVol :: X ()
toggleVol = do
  fileExists <- liftIO $ doesFileExist volStatFile
  if fileExists
     then spawn $ amixer ++ "off" ++ " && " ++ "rm "   ++ volStatFile
  else spawn $ amixer ++ "on"  ++ " && " ++ "touch "   ++ volStatFile

-- | Increases volume by given percentages.
incVol :: Int -> X ()
incVol n = spawn $ amixer ++ show n ++ "%+"

-- | Decreases volume by given percentages.
decVol :: Int -> X ()
decVol n = spawn $ amixer ++ show n ++ "%-"

-- | Pop up a prompt for defining a value for the volume.
fireVolPrompt :: XPConfig -> X ()
fireVolPrompt config = inputPrompt config "Volume" ?+ fireVol
  where
    fireVol v = spawn $ amixer ++ v ++ "%"
