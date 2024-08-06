Config { font    = "Hack Nerd Font Mono 11"
       , additionalFonts = [ "Font Awesome 5 Free Solid 11"
                           , "Font Awesome 5 Brands 11"
                           ]
       , bgColor = "#282c34"
       , fgColor = "#ff6c6b"
       , position = TopSize L 90 24
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       --, iconRoot = "~/.xmonad/xpm/"  -- default: "."
       , commands = [

                      Run Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
                        -- Get kernel version (script found in .local/bin)
                    , Run Com ".local/bin/kernel" [] "kernel" 36000
                        -- Cpu usage in percent

                    , Run Cpu ["-t", "<fn=2>\xf108</fn> cpu: (<total>%)","-H","50","--high","red"] 20
                        -- Ram used number and percent

                    , Run Memory ["-t", "<fn=2>\xf233</fn> mem: <used>M (<usedratio>%)"] 20
                        -- Disk space free

                    , Run DiskU [("/", "<fn=2>\xf0c7</fn> hdd: <free> free")] [] 60
                        -- Echos an "up arrow" icon in front of the uptime output.

                    , Run Com "echo" ["<fn=2>\xf0aa</fn>"] "uparrow" 3600
                        -- Uptime

                    , Run Uptime ["-t", "uptime: <days>d <hours>h"] 360
                        -- Echos a "bell" icon in front of the pacman updates.

                    , Run Com "echo" ["<fn=2>\xf0f3</fn>"] "bell" 3600
                        -- Check for pacman updates (script found in .local/bin)

                    , Run Com ".local/bin/pacupdate" [] "pacupdate" 36000
                        -- Echos a "battery" icon in front of the pacman updates.

                    , Run Com "echo" ["<fn=2>\xf242</fn>"] "baticon" 3600
                        -- Battery

                    , Run BatteryP ["BAT0"] ["-t", "<acstatus><watts> (<left>%)"] 360
                        -- Time and date

                    , Run Date "<fn=2>\xf017</fn> %b %d %Y - %k:%M:%S %p" "date" 1
                        -- Script that dynamically adjusts xmobar padding depending on number of trayer icons.

                    , Run Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
                        -- Prints out the left side items such as workspaces, layout, etc.
                    , Run Com "volume" [] "volume" 36000
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeXMonadLog% }{<box type=Bottom width=2 mb=2 color=#98be65><fc=#98be65><action=`alacritty -e btop`>%volume%</action></fc></box>  <box type=Bottom width=2 mb=2 color=#ecbe7b><fc=#ecbe7b><action=`alacritty -e btop`>%cpu%</action></fc></box>  <box type=Bottom width=2 mb=2 color=#ff6c6b><fc=#ff6c6b><action=`alacritty -e btop`>%memory%</action></fc></box>  <box type=Bottom width=2 mb=2 color=#a9a1e1><fc=#a9a1e1>%disku%</fc></box>  <box type=Bottom width=2 mb=2 color=#98be65><fc=#98be65>%uparrow% %uptime%</fc></box>  <box type=Bottom width=2 mb=2 color=#c678dd><fc=#c678dd>%bell% <action=`alacritty -e sudo pacman -Syu`>%pacupdate%</action></fc></box>  <box type=Bottom width=2 mb=2 color=#da8548><fc=#da8548>%baticon% %battery%</fc></box>  <box type=Bottom width=2 mb=2 color=#46d9ff><fc=#46d9ff><action=`emacsclient -c -a 'emacs' --eval '(doom/window-maximize-buffer(dt/year-calendar))'`>%date%</action></fc></box>"
       }
