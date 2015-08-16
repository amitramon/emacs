; my personal abbreviations
(define-abbrev-table 'global-abbrev-table '(
;; math/unicode symbols
    ("tin" "∈" nil 0)
    ("tnin" "∉" nil 0)
    ("tinf" "∞" nil 0)
    ("tluv" "♥" nil 0)
    ("tsmly" "☺" nil 0)

    ;; email
    ("twdy" "wordy-english@yahoogroups.com" nil 0)

    ;; computing tech
    ("twp" "Wikipedia" nil 0)

    ;; misc

    ("קעמ" "עמית" nil 0)
    ("קער" "עמית רמון" nil 0)
    ("קתר" "התחברות-תראבוט" nil 0)
    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode
(abbrev-mode 1)

