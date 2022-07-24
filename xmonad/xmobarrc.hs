Config
  { 
   -- appearance
    font =         "xft:JetBrainsMono Nerd Font:size=10:bold:antialias=true",
    bgColor =      "#282828",
    fgColor =      "#ebdbb2",
    position =     TopW L 90,
    border =       BottomB,
    borderColor =  "#222222",

    -- items
    commands =
    [
      Run UnsafeStdinReader,
      Run Date "%d %b %Y * %H:%M" "date" 30,
      Run Alsa "default" "Master" ["-t", "<status>",
                                   "--",
                                   "-L 10",
                                   "-H 60",
                                   "-o", "婢 <volume>%",
                                   "-O", " <volume>%",
                                   "-h", "墳",
                                   "-m", "奔",
                                   "-l", "奄",
                                   "-c", "#ebdbb2", "-C", "#ebdbb2"
                                  ],
      
      Run BatteryP ["BAT0"] ["-t", "<acstatus>",
                             "--",
                             "-L", "20", "-H", "80",
                             "-O", " AC", "-i", " AC", "-o", " <left>%",
                             "--lows", "",
                             "--mediums", "",
                             "--highs", ""
                            ] 600
    ],
    template = " %UnsafeStdinReader%}{%alsa:default:Master% %battery% : %date% "
   }
