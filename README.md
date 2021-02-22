Slybot
======

IRC Bot code for TGS's IRC server.

Before installing, ensure you have phantomjs on your machine, though this is only required for use with ForumPost. You will also require mueval (see below), the program can be built and ran without it if you don't mind the rd command giving weird error messages.

To install:
```
    git clone https://github.com/DrewBarclay/Slybot.git
    cd Slybot
    stack build
```

To set up a database if you don't have one, run:
```
    stack exec CreateDatabase
```

If you need mueval, good luck. You'll need it.
    
Now run (in different terminals):
```
    stack exec Slybot
    stack exec SceneSummary
```

You'll also have to create src/Config.hs and fill in the following:

```
{-# LANGUAGE OverloadedStrings #-}                                                                                    
module Config (                                                                                                       
  server,                                                                                                             
  port,                                                                                                               
  nick,                                                                                                               
  pass,                                                                                                               
  user,                                                                                                               
  real_name,                                                                                                          
  channels,                                                                                                           
  forum_url,                                                                                                          
  forum_user,                                                                                                         
  forum_password,                                                                                                     
  summarize_url,                                                                                                      
  help_text_lines,                                                                                                    
  database,                                                                                                           
  default_teaser_length,                                                                                              
  muevalBinary                                                                                                        
                                                                                                                      
) where                                                                                                               
                                                                                                                      
import qualified Network                                                                                              
                                                                                                                      
server = "put your irc server here like irc.freenode.net" :: String                                                                              
port = Network.PortNumber (fromIntegral 6667) :: Network.PortID                                                       
nick = "put nick here" :: String                                                                                             
pass = "password here" :: String                                                                                       
user = "username here" :: String                                                                                             
real_name = "put your real name here" :: String                                                                               
channels = ["#public"] :: [String]                                                            
forum_url = "SMF forum url" :: String     
forum_user = "forum user" :: String
forum_password = "forum password" :: String
summarize_url = "url to scene summary server" :: String
database = "logs.db"
default_teaser_length = 25
muevalBinary = "mueval" -- recommended install via `stack install mueval` with the exact GHC version you use to compile Slybot, then replace with /home/ubuntu/.local/bin/mueval

help_text_lines = [
  "Slybot is your friend!",
  "Commands:",
  "!help",
  "!echo (text)",
  "!rn (dice)r(rollagain)",
  "!game create (name) (board) (thread)",
  "!scene create",
  "!scene cancel",
  "!scene end",
  "!scene summary (optional summary or leave blank for link)",
  "!joinchannel #channel [opers only]",
  "!leavechannel #channel [opers only]",
  "!command COMMAND args [opers only]"
  ] :: [String]
