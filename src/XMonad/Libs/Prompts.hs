module XMonad.Libs.Prompts (
  switchPrompt
  ) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input

statusPrompt :: XPConfig -> String -> Bool -> String -> X ()
statusPrompt config prompt notify run = do
  inputPromptWithCompl config prompt cFun ?+ action notify
  where
    comps = switchComps run
    cFun = mkComplFunFromList' comps
    run' = switchExec run
    not' = "msg=`" ++ run' ++ " status` && twmnc -c \"$msg\""
    action n s | n         = spawn $ run' ++ " " ++ s ++ " && " ++ not'
               | otherwise = spawn $ run' ++ " " ++ s

-- Hier sind alle Programme die nach dem Schema definiert sind aufgelistet.
-- Diese Programme sind in ~/bin zu finden. Alle diese Programme müssen zumindest
-- 3 Argumente annehmen. Das sind: 'on', 'off' und 'status'. 'on' aktiviert
-- diesen Switch, 'off' deaktiviert diesen Switch und 'status' wird für die
-- Notification-Message verwendet. Für die Notification-Message ist es wichtig
-- das 'twmnd' läuft.
switchProgs :: [((String,String),[String])]
switchProgs = [ (("mouse", "mouse.sh"), ["on","off","toggle"])
              , (("monitor", "monitor.sh"), ["on","off"])
              ]
              
switchProgNames :: [String]
switchProgNames = map (fst . fst) switchProgs

switchExec :: String -> String
switchExec name = chkList $ execList name
  where
    execList n = filter ((==n) . fst . fst) switchProgs
    chkList [] = ""
    chkList ls = (snd . fst) $ head ls

switchComps :: String -> [String]
switchComps = chkList . execList
  where
    execList n = filter ((==n) . fst . fst) switchProgs
    chkList [] = []
    chkList ls = snd $ head ls

switchPrompt :: XPConfig -> Bool -> X ()
switchPrompt config notify = do
  inputPromptWithCompl config "Switch" cFun ?+
    statusPrompt config "status" notify
    where
      cFun = mkComplFunFromList' switchProgNames

