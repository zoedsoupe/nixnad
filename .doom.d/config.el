;  _ __ ___   __| |___ _ __
; | '_ ` _ \ / _` / __| '_ \
; | | | | | | (_| \__ \ |_) |
; |_| |_| |_|\__,_|___/ .__/
;                     |_|

(setq user-full-name "Matheus de Souza Pessanha"
      user-mail-address "mdsp@boosting.tech")

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-one)

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(setq centaur-tabs-mode t)
(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs on/off"
      "t c" #' centaur-tabs-local-mode)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g t") 'centaur-tabs-forward
                                               (kbd "g T")  'centaur-tabs-backward
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)

(setq projectile-enable-caching t)

(setq company-idle-delay 0.05)
(setq confirm-kill-emacs nil)

(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)
