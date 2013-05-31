-- | Module     : XMonad.Libs.SSH
-- | Copyright  : (c) Oliver Dunkl
-- | License    : BSD (see LICENSE)
--
-- | Maintainer : Oliver Dunkl <oliver.dunkl@gmx.at>
--
-- | This module is responsible for working with SSH.
module XMonad.Libs.SSH (
  SSH,
  fireSSHPrompt
  ) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run      (runInTerm)

-- | Data structure for SSH.
data SSH = SSH

-- | Instance for prompting for SSH connections.
instance XPrompt SSH where
  showXPrompt SSH       = "SSH to: "
  commandToComplete _ c = c
  nextCompletion      _ = getNextCompletion

-- | Pop up a prompt for connecting to a defined server.
-- The first parameter of that function is the configuration of the prompt.
-- The second parameter ist a list of tuples. The first value of the tuple
-- is the short form of the url and the second is the real URL.
--   e.g. [("server1","server1.myorg.com")]
--
-- First the prompt will ask you for the user which will be connected to
-- the server. The second prompt is the url in the short version form.
fireSSHPrompt :: XPConfig           -- ^ configuration of the prompt
              -> [(String,String)]  -- ^ [(short URL, long URL)]
              -> X ()
fireSSHPrompt config sshList = do
  inputPrompt config "SSH user" ?+ fireSSH
  where
    fireSSH   user       = mkXPrompt SSH config
                           (mkComplFunFromList $ map fst sshList) $
                           ssh user

    ssh       user input = runInTerm "" ("ssh " ++ sshUrl user input)
    sshUrl    user input = user ++ "@" ++ (sshFilter input)
    sshFilter      input = snd $ head $ filter ((==input) . fst) sshList
