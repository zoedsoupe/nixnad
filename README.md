# dotfiles

**Powered by:**

![](https://img.shields.io/badge/-Gnu-informational?style=for-the-badge&logo=GNU&logoColor=white&color=A42E2B)
![](https://img.shields.io/badge/-Linux-informational?style=for-the-badge&logo=Linux&logoColor=white&color=000000)
![](https://img.shields.io/badge/-Arch-informational?style=for-the-badge&logo=Arch-Linux&logoColor=white&color=1793D1)
![](https://img.shields.io/badge/-Xorg-informational?style=for-the-badge&logo=X.Org&logoColor=white&color=F28834)
![](https://img.shields.io/badge/-ZSH-informational?style=for-the-badge&logo=gnu-bash&logoColor=white&color=C97E84)
![](https://img.shields.io/badge/-Vim-informational?style=for-the-badge&logo=vim&logoColor=white&color=019733)

# Dependencies

## Operating System

This setup could be reproduced on any GNU/Linux distribution using Systemd.
It could however not be fully reproduced on macOs or Bsd systems. 

An initial minimal installation of the operating system is recommended. This means an installation without a desktop environment (such as Xfce, Gnome or Kde) as the window manager and the hotkey deamon might interfere with the desktop environment (another reason why [ricers](https://www.reddit.com/r/unixporn/wiki/themeing/dictionary#wiki_rice) use Arch BTW). 

## Programs

| Role  | Program |
| ------------- | ------------- |
| Display Server  | X11 ([Xorg](https://wiki.archlinux.org/index.php/Xorg)) |
| Window Manager  | [i3](https://i3wm.org/)  |
|RandR | [xorg-xrandr](https://www.archlinux.org/packages/?name=xorg-xrandr)|
|Compositor | [Picom](https://github.com/yshui/picom)|
| Bars | [i3bar](https://i3wm.org/i3bar/)|
|Terminal Emulator | [Kitty](https://github.com/kovidgoyal/kitty)|
| Shell | Zsh|
| Music Player Server | [MPD](https://wiki.archlinux.org/index.php/Music_Player_Daemon)|
| MPD client (Cli) | [ncmpcpp](https://wiki.archlinux.org/index.php/Ncmpcpp)|
| Lockscreen | [ly](https://github.com/nullgemm/ly)|
| Wallpaper setter for X | [feh](https://wiki.archlinux.org/index.php/feh)|
| Dmenu replacement (Window switcher, application launcher, etc.) | [rofi](https://github.com/davatorium/rofi)|
| Audio visualizer | [Glava](https://github.com/jarcode-foss/glava)|
| File manager (Cli) | [nnn](https://github.com/jarun/nnn) | 
| Document viewer | [Zathura](https://pwmt.org/projects/zathura/) |
| Clipboard manager | [clipit](https://github.com/CristianHenzel/ClipIt) |
| Notification daemon | [dunst](https://dunst-project.org/) |
| Text editor | [Neovim](https://neovim.io/) |
