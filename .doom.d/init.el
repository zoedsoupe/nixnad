;;; init.el -*- lexical-binding: t; -*-

(doom!
       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life

       :ui
       deft              ; notational velocity for Emacs
       tabs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (ligatures +extra)         ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       neotree
       vc-gutter         ; vcs diff in the fringe
       zen               ; distraction-free coding or writing

       :editor
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       debugger          ; FIXME stepping through code, to help you add bugs
       (docker +lsp)
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp
       magit             ; a git porcelain for Emacs
       pdf               ; pdf enhancements
       rgb               ; creating color strings

       :os
       tty               ; improve the terminal Emacs experience

       :lang
       (clojure +lsp)           ; java with a lisp
       (elixir +lsp)            ; erlang done right
       emacs-lisp        ; drown in parentheses
       (erlang +lsp)            ; an elegant language for a more civilized age
       (haskell +dante
                +lsp)  ; a language that's lazier than I am
       json              ; At least it ain't XML
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       (ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (sh +fish
           +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       (web +html
            +css
            +lsp)
       yaml              ; JSON, but readable

       :config
       (default +bindings +smartparens))
