
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
   , template = " %StdinReader% }{ %battery% - %date% "

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
                                       , "-O"	, "<fc=#a3be8c>Charging</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#a3be8c>Charged</fc>"
                             ] 50 
          
        -- time and date indicator 
        , Run Date           "<fc=#d8dee9>%F (%a) %T</fc>" "date" 10

        -- pipe my workspaces into the bar.
	, Run StdinReader
        ]
   }
