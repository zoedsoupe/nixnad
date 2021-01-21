module Matthew.TreeSelect where

import XMonad
import Data.Tree
import qualified Data.Map as M
import qualified XMonad.Actions.TreeSelect as TS

import Matthew.Variables (myTerminal, myBrowser, myEditor, myFont, altMask)

-- Configuration options for the treeSelect menus.
-- Keybindings for treeSelect menus. Use h-j-k-l to navigate.
-- Use ‘o’ and ‘i’ to move forward/back in the workspace history.
-- Single KEY’s are for top-level nodes. SUPER+KEY are for the second-level nodes.
-- SUPER+OPTION+KEY are third-level nodes.

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd282c34
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffd0d0d0, 0xff1c1f24)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff282c34)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 100
                              , TS.ts_originY      = 100
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    , ((0, xK_a),        TS.moveTo ["+ Accessories"])
    , ((0, xK_e),        TS.moveTo ["+ Games"])
    , ((0, xK_g),        TS.moveTo ["+ Graphics"])
    , ((0, xK_i),        TS.moveTo ["+ Internet"])
    , ((0, xK_m),        TS.moveTo ["+ Multimedia"])
    , ((0, xK_o),        TS.moveTo ["+ Office"])
    , ((0, xK_p),        TS.moveTo ["+ Programming"])
    , ((0, xK_s),        TS.moveTo ["+ System"])
    , ((0, xK_b),        TS.moveTo ["+ Bookmarks"])
    , ((0, xK_c),        TS.moveTo ["+ Config Files"])
    , ((0, xK_r),        TS.moveTo ["+ Screenshots"])
    , ((mod4Mask, xK_l), TS.moveTo ["+ Bookmarks", "+ Linux"])
    , ((mod4Mask, xK_e), TS.moveTo ["+ Bookmarks", "+ Emacs"])
    , ((mod4Mask, xK_s), TS.moveTo ["+ Bookmarks", "+ Search and Reference"])
    , ((mod4Mask, xK_p), TS.moveTo ["+ Bookmarks", "+ Programming"])
    , ((mod4Mask, xK_v), TS.moveTo ["+ Bookmarks", "+ Vim"])
    , ((mod4Mask .|. altMask, xK_a), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Arch Linux"])
    , ((mod4Mask .|. altMask, xK_n), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Linux News"])
    , ((mod4Mask .|. altMask, xK_w), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Window Managers"])
    ]

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "+ Accessories" "Accessory applications" (return ()))
       [ Node (TS.TSNode "Calculator" "Gnome calculator" (spawn "gnome-calculator")) []
       , Node (TS.TSNode "Picom Toggle on/off" "Compositor for window managers" (spawn "killall picom; picom --experimental-backend")) []
       , Node (TS.TSNode "Boxes" "GNOME's virtualization program" (spawn "gnome-boxes")) []
       ]
   , Node (TS.TSNode "+ Internet" "internet and web programs" (return ()))
       [ Node (TS.TSNode "Brave" "A privacy-oriented web browser" (spawn "brave")) []
       , Node (TS.TSNode "Discord" "Chat and video chat platform" (spawn "discord")) []
       ]
   , Node (TS.TSNode "+ Multimedia" "sound and video applications" (return ()))
       [ Node (TS.TSNode "Alsa Mixer" "Alsa volume control utility" (spawn (myTerminal ++ " -e alsamixer"))) []
       , Node (TS.TSNode "VLC" "Multimedia player and server" (spawn "vlc")) []
       ]
   , Node (TS.TSNode "+ Office" "office applications" (return ()))
       [ Node (TS.TSNode "LibreOffice" "Open source office suite" (spawn "libreoffice")) []
       , Node (TS.TSNode "LibreOffice Calc" "Spreadsheet program" (spawn "localc")) []
       , Node (TS.TSNode "LibreOffice Writer" "Word processor" (spawn "lowriter")) []
       , Node (TS.TSNode "Zathura" "PDF Viewer" (spawn "zathura")) []
       ]
   , Node (TS.TSNode "+ Programming" "programming and scripting tools" (return ()))
       [ Node (TS.TSNode "+ Emacs" "Emacs is more than a text editor" (return ()))
           [ Node (TS.TSNode "Emacs Client" "Doom Emacs launched as client" (spawn "emacsclient -c -a emacs")) []
           , Node (TS.TSNode "M-x ibuffer" "Emacs buffer list" (spawn "emacsclient -c -a '' --eval '(ibuffer)'")) []
           , Node (TS.TSNode "M-x vterm" "Emacs" (spawn "emacsclient -c -a '' --eval '(+vterm/here nil))'")) []
           ]
        , Node (TS.TSNode "Elixir" "Elixir interactive prompt" (spawn (myTerminal ++ " -e iex"))) []
       ]
   , Node (TS.TSNode "+ System" "system tools and utilities" (return ()))
       [ Node (TS.TSNode "Alacritty" "GPU accelerated terminal" (spawn "alacritty")) []
       , Node (TS.TSNode "Ytop" "Terminal process viewer" (spawn (myTerminal ++ " -e ytop"))) []
       , Node (TS.TSNode "LXAppearance" "Customize look and feel; set GTK theme" (spawn "lxappearance")) []
       ]
   , Node (TS.TSNode "------------------------" "" (spawn "xdotool key Escape")) []
   , Node (TS.TSNode "+ Bookmarks" "a list of web bookmarks" (return ()))
       [ Node (TS.TSNode "+ Linux" "a list of web bookmarks" (return ()))
           [ Node (TS.TSNode "+ Arch Linux" "btw, i use arch!" (return ()))
               [ Node (TS.TSNode "Arch Linux" "Arch Linux homepage" (spawn (myBrowser ++ "https://www.archlinux.org/"))) []
               , Node (TS.TSNode "Arch Wiki" "The best Linux wiki" (spawn (myBrowser ++ "https://wiki.archlinux.org/"))) []
               , Node (TS.TSNode "AUR" "Arch User Repository" (spawn (myBrowser ++ "https://aur.archlinux.org/"))) []
               , Node (TS.TSNode "Arch Forums" "Arch Linux web forum" (spawn (myBrowser ++ "https://bbs.archlinux.org/"))) []
               ]
           , Node (TS.TSNode "+ Linux News" "linux news and blogs" (return ()))
               [ Node (TS.TSNode "DistroWatch" "Linux distro release announcments" (spawn (myBrowser ++ "https://distrowatch.com/"))) []
               , Node (TS.TSNode "LXer" "LXer linux news aggregation" (spawn (myBrowser ++ "http://lxer.com"))) []
               , Node (TS.TSNode "OMG Ubuntu" "Ubuntu news, apps and reviews" (spawn (myBrowser ++ "https://www.omgubuntu.co.uk"))) []
               ]
           , Node (TS.TSNode "+ Window Managers" "window manager documentation" (return ()))
               [ Node (TS.TSNode "+ XMonad" "xmonad documentation" (return ()))
                   [ Node (TS.TSNode "XMonad" "Homepage for XMonad" (spawn (myBrowser ++ "http://xmonad.org"))) []
                   , Node (TS.TSNode "XMonad GitHub" "The GitHub page for XMonad" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad"))) []
                   , Node (TS.TSNode "xmonad-contrib" "Third party extensions for XMonad" (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmonad-contrib"))) []
                   , Node (TS.TSNode "xmonad-contrib GitHub" "The GitHub page for xmonad-contrib" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad-contrib"))) []
                   , Node (TS.TSNode "Xmobar" "Minimal text-based status bar"  (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmobar"))) []
                   ]
               ]
           ]
       , Node (TS.TSNode "+ Emacs" "Emacs documentation" (return ()))
           [ Node (TS.TSNode "GNU Emacs" "Extensible free/libre text editor" (spawn (myBrowser ++ "https://www.gnu.org/software/emacs/"))) []
           , Node (TS.TSNode "Doom Emacs" "Emacs distribution with sane defaults" (spawn (myBrowser ++ "https://github.com/hlissner/doom-emacs"))) []
           , Node (TS.TSNode "r/emacs" "M-x emacs-reddit" (spawn (myBrowser ++ "https://www.reddit.com/r/emacs/"))) []
           , Node (TS.TSNode "EmacsWiki" "EmacsWiki Site Map" (spawn (myBrowser ++ "https://www.emacswiki.org/emacs/SiteMap"))) []
           , Node (TS.TSNode "Emacs StackExchange" "Q&A site for emacs" (spawn (myBrowser ++ "https://emacs.stackexchange.com/"))) []
           ]
       , Node (TS.TSNode "+ Search and Reference" "Search engines, indices and wikis" (return ()))
           [ Node (TS.TSNode "Google" "The evil search engine" (spawn (myBrowser ++ "http://www.google.com"))) []
           , Node (TS.TSNode "Thesaurus" "Lookup synonyms and antonyms" (spawn (myBrowser ++ "https://www.thesaurus.com/"))) []
           , Node (TS.TSNode "Wikipedia" "The free encyclopedia" (spawn (myBrowser ++ "https://www.wikipedia.org/"))) []
           ]
       , Node (TS.TSNode "+ Programming" "programming and scripting" (return ()))
           [ Node (TS.TSNode "+ Bash and Shell Scripting" "shell scripting documentation" (return ()))
               [ Node (TS.TSNode "GNU Bash" "Documentation for bash" (spawn (myBrowser ++ "https://www.gnu.org/software/bash/manual/"))) []
               , Node (TS.TSNode "r/bash" "Subreddit for bash" (spawn (myBrowser ++ "https://www.reddit.com/r/bash/"))) []
               , Node (TS.TSNode "r/commandline" "Subreddit for the command line" (spawn (myBrowser ++ "https://www.reddit.com/r/commandline/"))) []
               , Node (TS.TSNode "Learn Shell" "Interactive shell tutorial" (spawn (myBrowser ++ "https://www.learnshell.org/"))) []
               ]
         , Node (TS.TSNode "+ Elisp" "emacs lisp documentation" (return ()))
             [ Node (TS.TSNode "Emacs Lisp" "Reference manual for elisp" (spawn (myBrowser ++ "https://www.gnu.org/software/emacs/manual/html_node/elisp/"))) []
             , Node (TS.TSNode "Learn Elisp in Y Minutes" "Single webpage for elisp basics" (spawn (myBrowser ++ "https://learnxinyminutes.com/docs/elisp/"))) []
             , Node (TS.TSNode "r/Lisp" "Subreddit for lisp languages" (spawn (myBrowser ++ "https://www.reddit.com/r/lisp/"))) []
             ]
         , Node (TS.TSNode "+ Erlang" "erlang documentation" (return()))
             [ Node (TS.TSNode "Erlang.org" "Homepage for erlang" (spawn (myBrowser ++ "https://www.erlang.org/"))) []
             , Node (TS.TSNode "r/erlang" "Subreddit for erlang" (spawn (myBrowser ++ "https://www.reddit.com/r/erlang/"))) []
             , Node (TS.TSNode "Erlang on StackExchange" "Newest erlang topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/erlang"))) []
             ]
         , Node (TS.TSNode "+ Haskell" "haskell documentation" (return ()))
             [ Node (TS.TSNode "Haskell.org" "Homepage for haskell" (spawn (myBrowser ++ "http://www.haskell.org"))) []
             , Node (TS.TSNode "Hoogle" "Haskell API search engine" (spawn "https://hoogle.haskell.org/")) []
             , Node (TS.TSNode "r/haskell" "Subreddit for haskell" (spawn (myBrowser ++ "https://www.reddit.com/r/haskell/"))) []
             , Node (TS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/haskell"))) []
             ]
         , Node (TS.TSNode "+ Elixir" "elixir documentation" (return ()))
             [ Node (TS.TSNode "elixir-lang.org" "Homepage for elixir" (spawn (myBrowser ++ "https://www.elixir-lang.org/"))) []
             , Node (TS.TSNode "r/elixir" "Subreddit for elixir" (spawn (myBrowser ++ "https://www.reddit.com/r/elixir/"))) []
             , Node (TS.TSNode "Elixir on StackExchange" "Newest elixir topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/elixir"))) []
             ]
         ]
       , Node (TS.TSNode "+ Vim" "vim and neovim documentation" (return ()))
           [ Node (TS.TSNode "Vim.org" "Vim, the ubiquitous text editor" (spawn (myBrowser ++ "https://www.vim.org/"))) []
           , Node (TS.TSNode "r/Vim" "Subreddit for vim" (spawn (myBrowser ++ "https://www.reddit.com/r/vim/"))) []
           , Node (TS.TSNode "Vi/m StackExchange" "Vi/m related questions" (spawn (myBrowser ++ "https://vi.stackexchange.com/"))) []
           ]
       , Node (TS.TSNode "My Start Page" "Custom start page for browser" (spawn (myBrowser ++ "file://$HOME/.surf/html/homepage.html"))) []
       ]
   , Node (TS.TSNode "+ Config Files" "config files that edit often" (return ()))
       [ Node (TS.TSNode "+ doom emacs configs" "My doom emacs config files" (return ()))
         [ Node (TS.TSNode "Doom Emacs init.el" "doom emacs init" (spawn (myEditor ++ "$HOME/.doom.d/init.el"))) []
         , Node (TS.TSNode "Doom Emacs packages.el" "doom emacs packages" (spawn (myEditor ++ "$HOME/.doom.d/packages.el"))) []
         , Node (TS.TSNode "Doom Emacs eshell aliases" "the aliases for use in eshell" (spawn (myEditor ++ "$HOME/.doom.d/aliases"))) []
         ]
       , Node (TS.TSNode "+ xmobar configs" "My xmobar config files" (return ()))
           [ Node (TS.TSNode "xmobar" "status bar" (spawn (myEditor ++ "$HOME/.config/xmobar/xmobarrc"))) []
           ]
       , Node (TS.TSNode "+ xmonad configs" "My xmonad config files" (return ()))
           [ Node (TS.TSNode "xmonad.hs" "My XMonad Main" (spawn (myEditor ++ "$HOME/.xmonad/xmonad.hs"))) []
           , Node (TS.TSNode "xmonadctl.hs" "The xmonadctl script" (spawn (myEditor ++ "$HOME/.xmonad/xmonadctl.hs"))) []
           ]
       , Node (TS.TSNode "alacritty" "alacritty terminal emulator" (spawn (myEditor ++ "$HOME/.config/alacritty/alacritty.yml"))) []
       , Node (TS.TSNode "fishrc" "the friendly interactive shell" (spawn (myEditor ++ "$HOME/.config/fish/config.fish"))) []
       , Node (TS.TSNode "neovim init.vim" "neovim text editor" (spawn (myEditor ++ "$HOME/.config/nvim/init.vim"))) []
       ]
   , Node (TS.TSNode "+ Screenshots" "take a screenshot" (return ()))
       [ Node (TS.TSNode "Quick fullscreen" "take screenshot immediately" (spawn "scrot -d 1 ~/pics/screenshots/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
       , Node (TS.TSNode "Delayed fullscreen" "take screenshot in 5 secs" (spawn "scrot -d 5 ~/pics/screenshots/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
       , Node (TS.TSNode "Section screenshot" "take screenshot of section" (spawn "scrot -s ~/pics/screenshots/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
       ]
   ] 
