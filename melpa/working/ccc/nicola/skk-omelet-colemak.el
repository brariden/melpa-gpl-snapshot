;;; skk-omelet-colemak.el --- SKK $B$K(B omelet (Colemak) $BF~NO4D6-$rDs6!(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;      Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; $B$3$N%U%!%$%k$O(B NICOLA $B5,3J$K=`5r$7$?(B omelet $BFH<+(B Colemak $BG[Ns$H$=$l$r<B8=$9(B
;; $B$k$?$a$N%k!<%k$rDs6!$7$^$9!#(B

;;; Code:


;; omelet $BFH<+(B Colemak $BG[Ns(B

(defvar skk-kanagaki-omelet-colemak-base-rule-list
  '(("`" nil skk-nicola-insert)
    ;;
    ("1" nil skk-nicola-insert) ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert) ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("-" nil skk-nicola-insert)
    ("=" nil skk-nicola-insert)
    ;;
    ("q" nil skk-nicola-insert) ("w" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("r" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)
    ;;
    ("y" nil skk-nicola-insert) ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert) ("o" nil skk-nicola-insert)
    ("p" nil skk-nicola-insert) ("[" nil skk-nicola-insert)
    ("]" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert) ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert) ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert) ("'" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert) ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert) ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("@" nil skk-today)
    ("$" nil skk-display-code-for-char-at-point)
    ("Q" nil skk-set-henkan-point-subr)
    ("A" nil skk-latin-mode)
    ("L" nil skk-jisx0208-latin-mode)
    ("Z" nil skk-jisx0208-latin-mode)
    ("X" nil skk-purge-from-jisyo)
    ("C" nil skk-input-by-code-or-menu))
  "US Colemak $BG[Ns%-!<%\!<%I$G(B omelet $BF~NO$9$k$?$a$N4pK\%k!<%k!#(B")

(defconst skk-omelet-colemak-keymap-display 'dummy "\
$B0J2<$O!"(BUS Colemak $BG[Ns%-!<%\!<%I$G(B omelet $BF~NO$9$k$?$a$N%-!<G[Ns?^$G$9!#(B

$B(#(!(!($(B
$B("%u!1("(B
$B("(B` $B!.("(B
$B('(!(!(+(!(!(((!(!(((!(!(((!(!($(B $B(#(!(!(((!(!(((!(!(((!(!(((!(!(((!(!(((!(!($(B
$B("!*!)("!w!?("!t!A("!p!V("!s!W("(B $B("!!!N("!u!O("!I!H("!Z!J("![!K("!D!X("!\!Y("(B
$B("(B1 $B!!("(B2 $B!!("(B3 $B!!("(B4 $B!!("(B5 $B!!("(B $B("(B6 $B!!("(B7 $B!!("(B8 $B!!("(B9 $B!!("(B0 $B!!("(B- $B!]("(B= $B!a("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!($(B
$B("$p$!("$,$(("$@$j("$4$c("$6$l("(B $B("$Q$h("$B$K("$0$k("$E$^("$T$'("!P!7("!Q!,("!C!@("(B
$B("(BQ $B!#("(BW $B$+("(BF $B$?("(BP $B$3("(BG $B$5("(B $B("(BJ $B$i("(BL $B$A("(BU $B$/("(BY $B$D("(B; $B!$("(B[ $B!"("(B] $B!+("(B\\ $B!o("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(*(!(!(%(B
$B("%t$r("$8$"("$G$J("$2$e("$<$b("(B $B("$P$_("$I$*("$.$N("$]$g("%v$C("!'!F("(B
$B("(BA $B$&("(BR $B$7("(BS $B$F("(BT $B$1("(BD $B$;("(B $B("(BH $B$O("(BN $B$H("(BE $B$-("(BI $B$$("(BO $B$s("(B' $B!G("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(%(B
$B("$q$%("$S!<("$:$m("$V$d("$Y$#("(B $B("$W$L("$>$f("$Z$`("$\$o("$n$)("(B
$B("(BZ $B!%("(BX $B$R("(BC $B$9("(BV $B$U("(BB $B$X("(B $B("(BK $B$a("(BM $B$=("(B, $B$M("(B. $B$[("(B/ $B!&("(B
$B(&(!(!(*(!(!(*(!(!(*(!(!(*(!(!(%(B $B(&(!(!(*(!(!(*(!(!(*(!(!(*(!(!(%(B

$B3FOH$NJ8;z$O0J2<$N$h$&$K=q$+$l$F$$$^$9!#(B

 $B:82<(B $B!D(B ASCII $BJ8;z(B
 $B1&2<(B $B!D(B $B?F;X%7%U%H$7$J$$$GF~NO$5$l$k$Y$-J8;z(B ($BC1FHBG80(B)
 $B1&>e(B $B!D(B $BF1B&?F;X%7%U%H$K$h$jF~NO$5$l$k$Y$-J8;z(B (straight shift)
 $B:8>e(B $B!D(B $BH?BPB&?F;X%7%U%H$K$h$jF~NO$5$l$k$Y$-J8;z(B (cross shift)

$B$3$l$K4p$$$F0J2<$N(B 3 $B$D$N%k!<%k$,7hDj$5$l$^$9!#(B

 `skk-omelet-colemak-plain-rule-list'
 `skk-omelet-colemak-lshift-rule-list'
 `skk-omelet-colemak-rshift-rule-list'

")

(defvar skk-omelet-colemak-plain-rule-list
  '((?\` "$B!.(B")
    ;;
    (?1 "1") (?2 "2") (?3 "3") (?4 "4") (?5 "5")
    ;;
    (?6 "6") (?7 "7") (?8 "8") (?9 "9") (?0 "0") (?- "$B!](B") (?= "$B!a(B")
    ;;
    (?q "$B!#(B") (?w ("$B%+(B" . "$B$+(B")) (?f ("$B%?(B" . "$B$?(B")) (?p ("$B%3(B" . "$B$3(B"))
    (?g ("$B%5(B" . "$B$5(B"))
    ;;
    (?j ("$B%i(B" . "$B$i(B")) (?l ("$B%A(B" . "$B$A(B")) (?u ("$B%/(B" . "$B$/(B")) (?y ("$B%D(B" . "$B$D(B"))
    (?\; "$B!"(B") (?\[ "$B!"(B") (?\] skk-kanagaki-dakuten) (?\\ "$B!o(B")
    ;;
    (?a ("$B%&(B" . "$B$&(B")) (?r ("$B%7(B" . "$B$7(B")) (?s ("$B%F(B" . "$B$F(B")) (?t ("$B%1(B" . "$B$1(B"))
    (?d ("$B%;(B" . "$B$;(B"))
    ;;
    (?h ("$B%O(B" . "$B$O(B")) (?n ("$B%H(B" . "$B$H(B")) (?e ("$B%-(B" . "$B$-(B")) (?i ("$B%$(B" . "$B$$(B"))
    (?o ("$B%s(B" . "$B$s(B")) (?\' "$B!G(B")
    ;;
    (?z "$B!%(B") (?x ("$B%R(B" . "$B$R(B")) (?c ("$B%9(B" . "$B$9(B")) (?v ("$B%U(B" . "$B$U(B"))
    (?b ("$B%X(B" . "$B$X(B"))
    ;;
    (?k ("$B%a(B" . "$B$a(B")) (?m ("$B%=(B" . "$B$=(B")) (?, ("$B%M(B" . "$B$M(B")) (?. ("$B%[(B" . "$B$[(B"))
    (?/ "$B!&(B")
    ;;
    (?\  " "))
  "$BC1FHBG80;~$NF~NO%k!<%k!#(B")

(defvar skk-omelet-colemak-rshift-rule-list
  '((?` "$B%u(B")
    ;;
    (?1 "$B!*(B") (?2 "$B!w(B") (?3 "$B!t(B") (?4 "$B!p(B") (?5 "$B!s(B")
    ;;
    (?6 "$B!N(B") (?7 "$B!O(B") (?8 "$B!H(B") (?9 "$B!J(B") (?0 "$B!K(B") (?- "$B!X(B") (?= "$B!Y(B")
    ;;
    (?q ("$B%p(B" . "$B$p(B")) (?w ("$B%,(B" . "$B$,(B")) (?f ("$B%@(B" . "$B$@(B")) (?p ("$B%4(B" . "$B$4(B"))
    (?g ("$B%6(B" . "$B$6(B"))
    ;;
    (?j ("$B%h(B" . "$B$h(B")) (?l ("$B%K(B" . "$B$K(B")) (?u ("$B%k(B" . "$B$k(B")) (?y ("$B%^(B" . "$B$^(B"))
    (?\; ("$B%'(B" . "$B$'(B")) (?\[ "$B!7(B") (?\] skk-kanagaki-handakuten) (?\\ "$B!C(B")
    ;;
    (?a "$B%t(B") (?r ("$B%8(B" . "$B$8(B")) (?s ("$B%G(B" . "$B$G(B")) (?t ("$B%2(B" . "$B$2(B"))
    (?d ("$B%<(B" . "$B$<(B"))
    ;;
    (?h ("$B%_(B" . "$B$_(B")) (?n ("$B%*(B" . "$B$*(B")) (?e ("$B%N(B" . "$B$N(B")) (?i ("$B%g(B" . "$B$g(B"))
    (?o ("$B%C(B" . "$B$C(B")) (?' "$B!F(B")
    ;;
    (?z ("$B%q(B" . "$B$q(B")) (?x ("$B%S(B" . "$B$S(B")) (?c ("$B%:(B" . "$B$:(B")) (?v ("$B%V(B" . "$B$V(B"))
    (?b ("$B%Y(B" . "$B$Y(B"))
    ;;
    (?k ("$B%L(B" . "$B$L(B")) (?m ("$B%f(B" . "$B$f(B")) (?, ("$B%`(B" . "$B$`(B")) (?. ("$B%o(B" . "$B$o(B"))
    (?/ ("$B%)(B" . "$B$)(B"))
    ;;
    (?\  " "))
  "$B1&?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar skk-omelet-colemak-lshift-rule-list
  '((?\` "$B!1(B")
    ;;
    (?1 "$B!)(B") (?2 "$B!?(B") (?3 "$B!A(B") (?4 "$B!V(B") (?5 "$B!W(B")
    ;;
    (?6 "$B!N(B") (?7 "$B!u(B") (?8 "$B!I(B") (?9 "$B!Z(B") (?0 "$B![(B") (?- "$B!D(B") (?= "$B!\(B")
    ;;
    (?q ("$B%!(B" . "$B$!(B")) (?w ("$B%((B" . "$B$((B")) (?f ("$B%j(B" . "$B$j(B")) (?p ("$B%c(B" . "$B$c(B"))
    (?g ("$B%l(B" . "$B$l(B"))
    ;;
    (?j ("$B%Q(B" . "$B$Q(B")) (?l ("$B%B(B" . "$B$B(B")) (?u ("$B%0(B" . "$B$0(B")) (?y ("$B%E(B" . "$B$E(B"))
    (?\; ("$B%T(B" . "$B$T(B")) (?\[ "$B!P(B") (?\] "$B!Q(B") (?\\ "$B!d(B")
    ;;
    (?a ("$B%r(B" . "$B$r(B")) (?r ("$B%"(B" . "$B$"(B")) (?s ("$B%J(B" . "$B$J(B")) (?t ("$B%e(B" . "$B$e(B"))
    (?d ("$B%b(B" . "$B$b(B"))
    ;;
    (?h ("$B%P(B" . "$B$P(B")) (?n ("$B%I(B" . "$B$I(B")) (?e ("$B%.(B" . "$B$.(B")) (?i ("$B%](B" . "$B$](B"))
    (?o "$B!((B") (?' "$B!'(B")
    ;;
    (?z ("$B%%(B" . "$B$%(B")) (?x "$B!<(B") (?c ("$B%m(B" . "$B$m(B")) (?v ("$B%d(B" . "$B$d(B"))
    (?b ("$B%#(B" . "$B$#(B"))
    ;;
    (?k ("$B%W(B" . "$B$W(B")) (?m ("$B%>(B" . "$B$>(B")) (?, ("$B%Z(B" . "$B$Z(B")) (?. ("$B%\(B" . "$B$\(B"))
    (?/ ("$B%n(B" . "$B$n(B"))
    ;;
    (?\  " "))
  "$B:8?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(require 'skk-nicola)

(when skk-nicola-use-koyubi-functions
  (add-hook 'skk-mode-hook
	    #'(lambda ()
		(define-key skk-j-mode-map "'" 'skk-kanagaki-bs))))

(provide 'skk-omelet-colemak)

;;; skk-omelet-colemak.el ends here
