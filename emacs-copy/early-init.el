; -------------------------------------------------------------------------- ;
; ~/.emacs.d/early-init.el                                                   ;
; -------------------------------------------------------------------------- ;
; History:                                                                   ;
; 02.Sep.2024    Initial setup.                                              ;
; -------------------------------------------------------------------------- ;

; Initialize installed packages.                   
(setq package-enable-at-startup t)

; Configure graphical elements.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

; No startup screen
(setq inhibit-startup-message t)

