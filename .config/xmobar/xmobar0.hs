Config { font = "xft:Ubuntu Mono:pixelsize=16:antialias=true:hinting=true"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
             Run UnsafeXPropertyLog "_XMONAD_LOG_0"
           , Run Network "enp34s0" ["-L","0","-H","32",
                                 "--normal","green","--high","red"] 10
           , Run Cpu ["-L","3","-H","50",
                      "--normal","green","--high","red"] 10
           , Run Memory ["-t","Mem: <usedratio>%"] 10
           , Run Swap [] 10
           , Run Com "uname" ["-s","-r"] "" 36000
           , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%_XMONAD_LOG_0% } { %cpu% | %memory% | %enp34s0% | <fc=#ee9a00>%date%</fc>"
	}
