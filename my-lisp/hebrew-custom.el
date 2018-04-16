;;; hebrew-custom.el --- Quail package for inputting Hebrew characters  -*-coding: iso-2022-7bit;-*-

;; Keywords: multilingual, input method, Hebrew

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
;; Amit Ramon amit DOT ramon AT gmail DOT com June 2010

(require 'quail)

;; This is a custom Hebrew LyX input method. It is based on a keyboard
;; map from Debian GNU/Linux 'xkb/symbols/il' file, and the nikud
;; pattern is based on Dekel Tsur's Hebrew mapping for LyX.  It also
;; incorporates suggestions on the emacs-bidi list by Yair F.  I made
;; some slight modifications to the original LyX layout. All off them
;; are just filling vacancies in the first shift level. They are:
;;
;; - AD01 gershayim
;; - AD02 geresh
;; - AC03 euro
;; - AC04 en dash
;; - AB01 bullet
;;
(quail-define-package
 "hebrew-custom-lyx" "Hebrew" ",Hl(B" nil "Hebrew custom LyX input method.

Based on LyX keyboard layout.
Additional mappings for Rafe and Yiddish ligatures.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ;; TLDE AE09 - AE12
 ("`" ?\;)
 ;; 9 unchanged AE09
 ;; 0 unchanged AE10
 ;; ("[" ?-)
 ;; ("]" ?=)

 ;; Shifted
 ;; ~ unchanged TLDE
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("_" ?$,1,^(B)  ; maqaf
 ;; ("}" ?+)

 ;; AD01 - AD13
 ("q" ?/)
 ("w" ?\')
 ("e" ?,Hw(B)  ; qof
 ("r" ?,Hx(B)  ; resh
 ("t" ?,H`(B)  ; alef
 ("y" ?,Hh(B)  ; tet
 ("u" ?,He(B)  ; vav
 ("i" ?,Ho(B)  ; final nun
 ("o" ?,Hm(B)  ; final mem
 ("p" ?,Ht(B)  ; pe
 ("[" ?\]) ; mirroring
 ("]" ?\[) ; mirroring
 ;; \ unchanged

 ;; Shifted
 ("Q" ?$,1-4(B) ; gershayim (new)
 ("W" ?$,1-3(B)  ; geresh (new)
 ("E" ?$,1,X(B)  ; qamats
 ("R" ?$,1,_(B)  ; rafe (new)
 ("T" ?\u200E) ; LRM
 ("Y" ?\u200F) ; RLM
 ("U" ?$,1,Y(B)  ; holam
 ("I" ?$,1-2(B)  ; Yiddish double yod
 ("O" ?$,1-0(B)  ; Yiddish double vav
 ("P" ?$,1,W(B)  ; patah
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring
 ;; | unchanged

 ;; AC01 - AC11
 ("a" ?,Hy(B)  ; shin
 ("s" ?,Hc(B)  ; dalet
 ("d" ?,Hb(B)  ; gimel
 ("f" ?,Hk(B)  ; kaf
 ("g" ?,Hr(B)  ; ayin
 ("h" ?,Hi(B)  ; yod
 ("j" ?,Hg(B)  ; het
 ("k" ?,Hl(B)  ; lamed
 ("l" ?,Hj(B)  ; final kaf
 (";" ?,Hs(B)  ; final pe
 ("'" ?,)

 ;; Shifted
 ("A" ?$,1,P(B)  ; sheva
 ("S" ?$,1,\(B)  ; dagesh/shuruq or mapiq
 ("D" ?$,1tL(B)  ; euro (new)
 ("F"  ?\u2013) ;en dash (new)
 ("G" ?$,1,b(B)  ; sin dot
 ("H" ?$,1,a(B)  ; shin dot
 ("J" ?$,1,T(B)  ; hiriq
 ("K" ?$,1tJ(B)  ; new sheqel sign
 ("L" ?$,1-1(B)  ; Yiddish vav-yod
 (":" ?:)
 ;; ("\"" ?\")

 ;; AB01 - AB10
 ("z" ?,Hf(B)  ; zayin
 ("x" ?,Hq(B)  ; samekh
 ("c" ?,Ha(B)  ; bet
 ("v" ?,Hd(B)  ; he
 ("b" ?,Hp(B)  ; nun
 ("n" ?,Hn(B)  ; mem
 ("m" ?,Hv(B)  ; tsadi
 ("," ?,Hz(B)  ; tav
 ("." ?,Hu(B)  ; final tsadi
 ("/" ?.)  ; stop

 ;; Shifted
 ("Z" ?\u2022) ; bullet (new)
 ("X" ?$,1,V(B)  ; segol (point)
 ("C" ?$,1,[(B)  ; qubuts
 ("V" ?$,1,Q(B)  ; hataf segol
 ("B" ?$,1,R(B)  ; hataf patah
 ("N" ?$,1,S(B)  ; hataf qamats
 ("M" ?$,1,U(B)  ; tsere
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring
 ;; ("?" ??)
)


;; My home-made Hebrew Dvorak layout ;-)

(quail-define-package
 "hebrew-custom-dvorak" "Hebrew" ",Hc(B" nil "Hebrew Dvorak custom input method."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ;; TLDE AE09 - AE12
 ("`" ?\;)
 ;; 9 unchanged AE09
 ;; 0 unchanged AE10
 ("[" ?\])
 ("]" ?\[)

 ;; Shifted
 ;; ~ unchanged AE01
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("{" ?}) ; mirroring
 ("}" ?{) ; mirroring


 ("_" ?$,1,^(B)  ; maqaf
 ;; ("}" ?+)

 ;; AD01 - AD13
 ("q" ?')
 ("w" ?\,)
 ("e" ?.)  ; qof
 ("r" ?,Hk(B)  ; resh
 ("t" ?,Hg(B)  ; alef
 ("y" ?,Ho(B)  ; tet
 ("u" ?,Hm(B)  ; vav
 ("i" ?,Hr(B)  ; final nun
 ("o" ?,Hc(B)  ; final mem
 ("p" ?,Hp(B)  ; pe
 ("[" ?/) ; mirroring
 ("]" ?=) ; mirroring
 ;; \ unchanged

 ;; Shifted
 ("Q" ?\") ; gershayim (new)
 ("W" ?>)  ; geresh (new)
 ("E" ?<)  ; qamats
 ("R" ?$,1,_(B)  ; rafe (new)
 ("T" ?\u200E) ; LRM
 ("Y" ?\u200F) ; RLM
 ("U" ?$,1,Y(B)  ; holam
 ("I" ?$,1-2(B)  ; Yiddish double yod
 ("O" ?$,1-0(B)  ; Yiddish double vav
 ("P" ?$,1,W(B)  ; patah
 ("{" ??)  ; mirroring
 ("}" ?+)  ; mirroring
 ;; | unchanged

 ;; AC01 - AC11
 ("a" ?$,1,q(B)  ; shin
 ("s" ?,Hl(B)  ; dalet
 ("d" ?,Hi(B)  ; gimel
 ("f" ?,He(B)  ; kaf
 ("g" ?,Hn(B)  ; ayin
 ("h" ?,Hz(B)  ; yod
 ("j" ?,H`(B)  ; het
 ("k" ?,Hd(B)  ; lamed
 ("l" ?,Hx(B)  ; final kaf
 (";" ?,Hy(B)  ; final pe
 ("'" ?-)

 ;; Shifted
 ("A" ?$,1,P(B)  ; sheva
 ("S" ?$,1,\(B)  ; dagesh/shuruq or mapiq
 ("D" ?$,1tL(B)  ; euro (new)
 ("F"  ?\u2013) ;en dash (new)
 ("G" ?$,1,b(B)  ; sin dot
 ("H" ?$,1,a(B)  ; shin dot
 ("J" ?$,1,T(B)  ; hiriq
 ("K" ?$,1tJ(B)  ; new sheqel sign
 ("L" ?$,1-1(B)  ; Yiddish vav-yod
 (":" ?:)
 ("\"" ?$,1,^(B)

 ;; AB01 - AB10
 ("z" ?\;)  ; zayin
 ("x" ?,Hh(B)  ; samekh
 ("c" ?,Hj(B)  ; bet
 ("v" ?,Hf(B)  ; he
 ("b" ?,Ht(B)  ; nun
 ("n" ?,Hv(B)  ; mem
 ("m" ?,Hs(B)  ; tsadi
 ("," ?,Hw(B)  ; tav
 ("." ?,Hq(B)  ; final tsadi
 ("/" ?,Hb(B)  ; stop

 ;; Shifted
 ("Z" ?:) ; bullet (new)
 ("X" ?$,1,V(B)  ; segol (point)
 ("C" ?$,1,[(B)  ; qubuts
 ("V" ?$,1,Q(B)  ; hataf segol
 ("B" ?$,1,R(B)  ; hataf patah
 ("N" ?$,1,S(B)  ; hataf qamats
 ("M" ?$,1,U(B)  ; tsere
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring
 ;; ("?" ??)
)

;;; hebrew-custom.el ends here
