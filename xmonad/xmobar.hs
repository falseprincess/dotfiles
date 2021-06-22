
Config { 

   -- appearance
     font =         "xft:GohuFont:style=Regular:size=14" 
   , additionalFonts = [ "Font Awesome 5 Free Solid:style=Regular" ]
   , bgColor =      "#2e3440"
   , fgColor =      "#d8dee9"
   , position =     Static { xpos = 0, ypos = 0, width = 1920, height = 26 }
  -- , border =       BottomB
   , borderColor =  "#D8DEE9"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = " %StdinReader% }{ %dynnetwork% <fc=#4C566A>|</fc> %alsa:default:Master%<fc=#4C566A>|</fc> %battery% <fc=#4C566A>|</fc> %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   , commands = 
          -- memory
        [ Run Memory         [ "--template" ,"<fc=#4C566A>Mem:</fc> <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "#bf616a"
                             , "--normal"   , "#d08770"
                             , "--high"     , "#a3be8c"
                             ] 10

        -- volume
        , Run Alsa "default" "Master" [ "-t", "<fc=#8FBCBB>Vol:</fc> <volume>% <status>"
                                      , "-f", "▊", "-b", "─"
                                      , "-H", "50", "-h", "#D8DEE9"
                                      , "-L", "25", "-l", "#D8DEE9"

                                      , "--"
                                                , "-c", "#D8DEE9"
                                                , "-C", "#D8DEE9"
                                      ] 

        -- battery monitor
        , Run Battery        [ "--template" , "<fc=#81A1C1>Batt:</fc> <acstatus>"
                             , "--Low"      , "25"        -- units: %
                             , "--High"     , "75"        -- units: %
                             , "--low"      , "#D8DEE9"
                             , "--normal"   , "#D8DEE9"
                             , "--high"     , "#D8DEE9"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#D8DEE9><left>% +</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#D8DEE9><left>% (full)</fc>"
                             ] 50 

        -- network activity monitor (dynamic interface resolution)
        , Run DynNetwork [ "--template" , "<fc=#81A1C1><dev>:</fc> <rx>KB - <tx>KB"
                         , "-S"         , "True"
                         , "--Low"      , "1000"       -- units: B/s
                         , "--High"     , "5000"       -- units: B/s
                         , "--low"      , "#D8DEE9"
                         , "--normal"   , "#D8DEE9"
                         , "--high"     , "#D8DEE9"

                         , "--", "--devices", "mullvad-4"
                         ] 10
          
        -- time and date indicator 
        , Run Date           "%F (%a) <fc=#8FBCBB>%r</fc>" "date" 10

        -- pipe my workspaces into the bar.
	, Run StdinReader
        ]
   }
