(require 'quail)

(defconst quail-keyboard-layout-standard-copied-from-quail-dot-el
  "\
                              \
  1!2@3#4$5%6^7&8*9(0)-_=+`~  \
  qQwWeErRtTyYuUiIoOpP[{]}    \
  aAsSdDfFgGhHjJkKlL;:'\"\\|    \
  zZxXcCvVbBnNmM,<.>/?        \
                              ")
(defconst quail-keyboard-layout-us-dvorak
  "\
                              \
  1!2@3#4$5%6^7&8*9(0)[{]}`~  \
  '\",<.>pPyYfFgGcCrRlL/?=+    \
  aAoOeEuUiIdDhHtTnNsS-_\\|    \
  ;:qQjJkKxXbBmMwWvVzZ        \
                              ")

(defconst quail-keyboard-layout-dvorak
  "\
                              \
`~1!2@3#4$5%6^7&8*9(0)[{]}    \
  '\",<.>pPyYfFgGcCrRlL/?=+\\|  \
  aAoOeEuUiIdDhHtTnNsS-_      \
  ;:qQjJkKxXbBmMwWvVzZ        \
                              ")

(push (cons "us-dvorak" quail-keyboard-layout-us-dvorak) 
      quail-keyboard-layout-alist)

