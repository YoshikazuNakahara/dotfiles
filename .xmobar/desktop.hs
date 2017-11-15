-- -*- mode:haskell -*-
--Config { font = "xft:Ricty:size=18:bold:antialias=true"
Config { font = "xft:monospace:size=18"
       , bgColor = "#263238"
       , fgColor = "#CFD8DC"
       , position = TopSize C 100 16
       , lowerOnStart = False
       , overrideRedirect = True
       , pickBroadest =     False
       , persistent = True
       , allDesktops = True
       , alpha = 204
       , border = NoBorder
       , commands = [ Run Network "enp0s25" [ "-t"        , "<fc=#d500f9,#263238><icon=.icons/ethernet.xbm/>⇩</fc><rx><fc=#d500f9>⇧</fc><tx>" -- unit:Kb/s
                                          , "-L"       , "40"
                                          , "-H"       , "200"
                                          , "--low"    , "#e53935"
                                          , "--normal" , "#eceff1"
                                          , "--high"   , "#69f0ae"
                                          ] 30
                    , Run MultiCpu        [ "-t"       , "<fc=#ffe57f,#263238><icon=.icons/cpu.xbm/>R</fc><total0>.<total1>.<total2>.<total3>"
                                          , "-L"       , "40"
                                          , "-H"       , "85"
                                          , "--low"    , "#69f0ae"
                                          , "--normal" , "#eceff1"
                                          , "--high"   , "#ff8f00"
                                          ] 10

                    , Run Memory          [ "-t"       , "<fc=#80d8ff,#263238><icon=.icons/memory.xbm/></fc><usedratio>"
                                          , "-L"       , "40"
                                          , "-H"       , "90"
                                          , "--low"    , "#69f0ae"
                                          , "--normal" , "#eceff1"
                                          , "--high"   , "#ff8f00"
                                          ] 10
                    , Run Date "<icon=.icons/calendar.xbm/>%a %m/%d %H:%M" "date" 10
                    , Run Kbd            [ ("jp" , "<fc=#ffd180,#263238><icon=.icons/keyboard.xbm/>JP</fc>")
                             , ("us"         , "<fc=#ffab40,#263238><icon=.icons/keyboard.xpm/>US</fc>")
                             , ("mysymbols(usjis)"         , "<fc=#ff9100,#263238><icon=.icons/keyboard.xbm/>C.US</fc>")
                             , ("mysymbols(emacs)"         , "<fc=#fb6d00,#263238><icon=.icons/keyboard.xbm/>C.EMACS</fc>")
                             , ("keypad(pointerkeys)"         , "<fc=#e65100,#263238><icon=.icons/keyboard.xbm/>C.MOUSE</fc>")
                             ]
                    , Run StdinReader
                     , Run MPD ["-t",
                     "<fc=#FF80AB,#263238><icon=.icons/music.xbm/></fc><artist><statei><fc=#FF80AB,#263238><icon=.icons/speaker.xbm/></fc><volume><fc=#FF80AB>[</fc><flags><fc=#FF80AB>]</fc>",
                     "--", "-P", "<icon=.icons/mplay.xbm/>", "-Z", "<icon=.icons/mpause.xbm/>", "-S", "<icon=.icons/mstop.xbm/>"
                     ] 10
                     , Run Brightness
                     [ "-t", "<fc=#FFD600,#263238><icon=.icons/light-bulb.xbm/></fc><percent>"
                     ] 30
                     ,  Run CoreTemp [
                                      "-t", "<fc=#ffe57f,#263238>T</fc><core0>.<core1>.<core2>.<core3>"
                                      , "-L", "40"
                                      , "-H", "60"
                                      , "--low"    , "#69f0ae"
                                      , "--normal" , "#eceff1"
                                      , "--high"   , "#ff8f00"
                                     ] 50
                    ,   Run Weather "RJFF" [ "--template", "<fc=#CCFF90,#263238><skyCondition></fc> <fc=#C6FF00,#263238><tempC></fc>"
                             ] 36000
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader%%RJFF%%bright%%mpd%%enp0s25%%multicpu%%coretemp%%memory%<fc=#80D8FF>%date%</fc>%kbd% " }
