
Config { 

   -- appearance
     font =         "xft:GohuFont:style=Regular:size=14"
   , bgColor =      "#2e3440"
   , fgColor =      "#d8dee9"
   , position =     Static { xpos = 0 , ypos = 0, width = 1920, height = 24 }
   , border =       NoBorder
   , borderColor =  "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = " %StdinReader% }{ %dynnetwork% | %alsa:default:Master%| %battery% | %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   , commands = 
       
        [ Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "#bf616a"
                             , "--normal"   , "#d08770"
                             , "--high"     , "#a3be8c"
                             ] 10

        , Run Alsa "default" "Master" [ "-t", "Vol: <volume>% <status>"
                                      , "-f", "▊", "-b", "─"
                                      , "-H", "70", "-h", "#bf616a"
                                      , "-L", "21", "-l", "#a3be8c"

                                      , "--" --
                                                , "-c", "#bf616a"
                                                , "-C", "#a3be8c"
                                      ] 

        -- battery monitor
        , Run Battery        [ "--template" , "Batt: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#bf616a"
                             , "--normal"   , "#d08770"
                             , "--high"     , "#a3be8c"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#a3be8c><left>% +</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#a3be8c><left>% (full)</fc>"
                             ] 50 

        -- network activity monitor (dynamic interface resolution)
        , Run DynNetwork [ "--template" , "<dev>: <rx>KB - <tx>KB"
                         , "-S"         , "True"
                         , "--Low"      , "1000"       -- units: B/s
                         , "--High"     , "5000"       -- units: B/s
                         , "--low"      , "#a3be8c"
                         , "--normal"   , "#a3be8c"
                         , "--high"     , "#bf616a"

                         , "--", "--devices", "mullvad-134"
                         ] 10
          
        -- time and date indicator 
        , Run Date           "<fc=#d8dee9>%F (%a) %r</fc>" "date" 10

        -- pipe my workspaces into the bar.
	, Run StdinReader
        ]
   }
