Config { font    = "xft:Ubuntu:weight=bold:pixelsize=16:antialias=true:hinting=true,Font Awesome 5 Free Regular:pixelsize=14"
       , additionalFonts = [ "xft:Mononoki Nerd Font:pixelsize=13:antialias=true:hinting=true"
                           , "xft:Font Awesome 6 Free Solid:pixelsize=13"
                           , "xft:Font Awesome 6 Free Regular:pixelsize=13"
                           ]
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
             Run UnsafeStdinReader --Run UnsafeXPropertyLog
           , Run Network "wlp2s0" [
               "-L","0","-H","32",
               "--normal","green","--high","red"] 10
           , Run Battery [
	       "-t", "<acstatus>: <left>% - <timeleft>",
	         "--",
	         "-O", "AC",
	         "-o", "Bat",
	         "-h", "green",
	         "-l", "red"
	       ] 100
           , Run Cpu ["-L","3","-H","50",
                      "--normal","green","--high","red"] 10
           , Run Memory ["-t","Mem: <usedratio>%"] 10
           , Run Swap [] 10
           , Run Com "uname" ["-s","-r"] "" 36000
           , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% } { %cpu% | %battery% | %memory% | %wlp2s0% | <fc=#ee9a00>%date%</fc>"
	}
