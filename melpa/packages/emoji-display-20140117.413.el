;;; emoji-display.el --- emoji displaying module -*- coding: utf-8-unix -*-

;; Copyright (C) 2011  Kazuhiro Ito

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; URL: https://github.com/ikazuhiro/emoji-display
;; Package-Version: 20140117.413
;; Keywords: emoji

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs 23 or later is required.
;; Get emoji images from FireMobileSimulator or EmojiPrint.
;;   FireMobileSimulator http://firemobilesimulator.org/
;;   EmojiPrint          http://www.takaaki.info/addon/emojiprint/
;;
;; 1. Place .el files to a loadable directory, and byte-compile as needed.
;; 2. Set emoji-display-image-directory variable appropriately or place
;; image directory to the same directory of .el files.
;; 3. Add your ~/.emacs as below code.
;;
;; (require 'emoji-display)
;; (emoji-display-mode)
;;
;; Known problems
;; * Resources are exhausted on MS Windows.
;; * Some animated images are corrupted.

;;; Code:

;; Customizable variables
(defgroup emoji-display nil
  "emoji image displaying features"
  :group 'convenience)

(defcustom emoji-display-image-directory
  (let (dir)
    (or (and (locate-library "emoji-display")
	     (setq dir (expand-file-name "image"
					 (file-name-directory
					  (locate-library "emoji-display"))))
	     (file-directory-p dir)
	     dir)
	(expand-file-name "emoji-display/image" data-directory)))
  "Directory for emoji image files."
  :group 'emoji-display
  :type 'directory)

(defcustom emoji-display-types
  '(docomo ezweb softbank)
  "Order of preferred emoji types."
  :group 'emoji-display
  :type '(repeat (choice (const softbank)
			 (const ezweb)
			 (const docomo))))

(defcustom emoji-display-animated-p
  (fboundp 'image-animate)
  "Non-nil means that emoji image is animated when possible."
  :group 'emoji-display
  :type 'boolean)

(defcustom emoji-display-animate-limit
  nil
  "Limit for image-animate."
  :group 'emoji-display
  :type '(choice (const t)
		 (const nil)
		 (number)))

;; Internal constants
(defvar emoji-display-type-subdir-alist
  (eval-when-compile
    (nreverse
     '(
       ;; The order must be the same as `emoji-display-table'.
       (docomo   . "i")
       (ezweb    . "e")
       (softbank . "s")
       ))))

(eval-and-compile
  (defvar emoji-display-table
    (eval-when-compile
    '(
      ("#⃣" "59104" "818" "17968")
      ("0⃣" "59115" "325" "17989")
      ("1⃣" "59106" "180" "17980")
      ("2⃣" "59107" "181" "17981")
      ("3⃣" "59108" "182" "17982")
      ("4⃣" "59109" "183" "17983")
      ("5⃣" "59110" "184" "17984")
      ("6⃣" "59111" "185" "17985")
      ("7⃣" "59112" "186" "17986")
      ("8⃣" "59113" "187" "17987")
      ("9⃣" "59114" "188" "17988")
      ("©" "59185" "81" "18030")
      ("®" "59190" "82" "18031")
      (" " nil "174" nil)
      (" " nil "173" nil)
      (" " nil "175" nil)
      ("‼" "59140" "734" nil)
      ("⁉" "59139" "733" nil)
      ("™" "59186" "54" "20823")
      ("ℹ" nil "11" nil)
      ("↔" "59196" "808" nil)
      ("↕" "59197" "809" nil)
      ("↖" "59031" "42" "18007")
      ("↗" "59000" "70" "18006")
      ("↘" "59030" "43" "18008")
      ("↙" "59045" "71" "18009")
      ("↩" "59098" "118" nil)
      ("↪" nil "117" nil)
      ("⌚" "59167" "25" nil)
      ("⌛" nil "57" nil)
      ("⏩" nil "8" "18012")
      ("⏪" nil "7" "18013")
      ("⏫" nil "35" nil)
      ("⏬" nil "34" nil)
      ("⏰" "59066" "46" nil)
      ("⏳" "59164" "58" nil)
      ("Ⓜ" "58972" nil nil)
      ("▪" nil "10" nil)
      ("▫" nil "9" nil)
      ("▶" nil "6" "18010")
      ("◀" nil "5" "18011")
      ("◻" nil "21" nil)
      ("◼" nil "22" nil)
      ("◽" nil "17" nil)
      ("◾" nil "18" nil)
      ("☀" "58942" "44" "18282")
      ("☁" "58943" "107" "18281")
      ("☎" "59015" "85" "18217")
      ("☑" nil "507" nil)
      ("☔" "58944" "95" "18283")
      ("☕" "58992" "93" "18277")
      ("☝" nil "284" "18223")
      ("☺" nil "68" "20532")
      ("♈" "58950" "192" "18015")
      ("♉" "58951" "193" "18016")
      ("♊" "58952" "194" "18017")
      ("♋" "58953" "195" "18018")
      ("♌" "58954" "196" "18019")
      ("♍" "58955" "197" "18020")
      ("♎" "58956" "198" "18021")
      ("♏" "58957" "199" "18022")
      ("♐" "58958" "200" "18023")
      ("♑" "58959" "201" "18024")
      ("♒" "58960" "202" "18025")
      ("♓" "58961" "203" "18026")
      ("♠" "59022" "314" "17966")
      ("♣" "59024" "316" "17967")
      ("♥" "59021" "414" "17964")
      ("♦" "59023" "315" "17965")
      ("♨" "59127" "224" "17731")
      ("♻" "59189" "807" nil)
      ("♿" "59035" "178" "17962")
      ("⚓" nil "211" nil)
      ("⚠" "59191" "1" "18034")
      ("⚡" "58946" "16" "17757")
      ("⚪" nil "23" nil)
      ("⚫" nil "24" nil)
      ("⚽" "58966" "219" "18232")
      ("⚾" "58963" "45" "18230")
      ("⛄" "58945" "191" "18280")
      ("⛅" nil "167" nil)
      ("⛎" nil "204" "18027")
      ("⛔" nil "98" nil)
      ("⛪" nil "340" "18263")
      ("⛲" nil "360" "17729")
      ("⛳" "58964" "306" "18228")
      ("⛵" "59043" "169" "18236")
      ("⛺" nil "361" "17730")
      ("⛽" "58987" "213" "18266")
      ("✂" "58997" "104" "20275")
      ("✅" nil "132" nil)
      ("✈" "58978" "168" "18237")
      ("✉" "59091" "108" nil)
      ("✊" "59027" "817" "18224")
      ("✋" "59029" "320" "18226")
      ("✌" "59028" "319" "18225")
      ("✏" "59161" "149" nil)
      ("✒" "59054" "508" nil)
      ("✔" nil "73" nil)
      ("✖" nil "55" nil)
      ("✨" "59130" "420" "20302")
      ("✳" nil "28" "17958")
      ("✴" nil "267" "17957")
      ("❄" nil "60" nil)
      ("❇" nil "76" nil)
      ("❌" nil "61" "20307")
      ("❎" nil "62" nil)
      ("❓" nil "3" "18240")
      ("❔" nil nil "20310")
      ("❕" nil nil "20311")
      ("❗" "59138" "2" "18241")
      ("❤" "59116" "51" "18242")
      ("➕" nil "26" nil)
      ("➖" nil "27" nil)
      ("➗" nil "66" nil)
      ("➡" nil "63" "18004")
      ("➰" "59146" "735" nil)
      ("⤴" "59125" "731" nil)
      ("⤵" "59136" "732" nil)
      ("⬅" nil "64" "18005")
      ("⬆" nil "29" "18002")
      ("⬇" nil "30" "18003")
      ("⬛" nil "39" nil)
      ("⬜" nil "38" nil)
      ("⭐" nil "69" "20303")
      ("⭕" nil "422" "20306")
      ("〰" "59145" nil nil)
      ("〽" nil nil "17740")
      ("㊗" nil "402" "20269")
      ("㊙" "59188" "279" "20277")
      ("🀄" nil "362" "17741")
      ("🃏" nil "797" nil)
      ("🅰" nil "724" "20818")
      ("🅱" nil "725" "20819")
      ("🅾" nil "726" "20821")
      ("🅿" "58988" "208" "17775")
      ("🆎" nil "727" "20820")
      ("🆑" "59099" "324" nil)
      ("🆒" nil "382" "17972")
      ("🆓" "59095" "299" nil)
      ("🆔" "59096" "385" "17993")
      ("🆕" "59101" "334" "17970")
      ("🆖" "59183" nil nil)
      ("🆗" "59147" "326" "18029")
      ("🆘" nil "270" nil)
      ("🆙" nil "303" "17971")
      ("🆚" nil "363" "17742")
      ("🇨🇳" nil "703" "20787")
      ("🇩🇪" nil "700" "20782")
      ("🇪🇸" nil "366" "20785")
      ("🇫🇷" nil "499" "20781")
      ("🇬🇧" nil "702" "20784")
      ("🇮🇹" nil "701" "20783")
      ("🇯🇵" nil "237" "20779")
      ("🇰🇷" nil "704" "20788")
      ("🇷🇺" nil "367" "20786")
      ("🇺🇸" nil "90" "20780")
      ("🈁" nil nil "17955")
      ("🈂" nil "384" "17992")
      ("🈚" nil nil "17974")
      ("🈯" nil "388" "17996")
      ("🈲" "59192" nil nil)
      ("🈳" "59193" "387" "17995")
      ("🈴" "59194" nil nil)
      ("🈵" "59195" "386" "17994")
      ("🈶" nil nil "17973")
      ("🈷" nil nil "17975")
      ("🈸" nil nil "17976")
      ("🈹" nil "383" "17991")
      ("🈺" nil "389" "17997")
      ("🉐" nil "285" "17990")
      ("🉑" nil "506" nil)
      ("🌀" "58947" "190" "20579")
      ("🌁" "58948" "305" nil)
      ("🌂" "58949" "481" "20572")
      ("🌃" "59059" "490" "20587")
      ("🌄" nil nil "18285")
      ("🌅" nil "493" "20585")
      ("🌆" nil "371" "17766")
      ("🌇" nil nil "20586")
      ("🌈" nil "491" "20588")
      ("🌉" nil "227" nil)
      ("🌊" "59199" "810" "20574")
      ("🌋" nil "769" nil)
      ("🌌" nil "781" nil)
      ("🌏" nil "332" nil)
      ("🌑" "59036" "321" nil)
      ("🌓" "59038" "323" nil)
      ("🌔" "59037" "322" nil)
      ("🌕" "59040" nil nil)
      ("🌙" "59039" "15" "18284")
      ("🌛" nil "47" nil)
      ("🌟" nil nil "20309")
      ("🌠" nil "75" nil)
      ("🌰" nil "742" nil)
      ("🌱" "59206" "811" nil)
      ("🌴" nil "255" "20263")
      ("🌵" nil "399" "20264")
      ("🌷" "59203" "113" "20260")
      ("🌸" "59208" "235" "18256")
      ("🌹" nil "339" "18258")
      ("🌺" nil "397" "20259")
      ("🌻" nil "256" "20261")
      ("🌼" nil "759" nil)
      ("🌽" nil "740" nil)
      ("🌾" nil nil "20580")
      ("🌿" nil "816" nil)
      ("🍀" "59201" "53" "17712")
      ("🍁" "59207" "133" "17720")
      ("🍂" nil "358" "17721")
      ("🍃" nil nil "20583")
      ("🍄" nil "741" nil)
      ("🍅" nil "436" "20329")
      ("🍆" nil "437" "20330")
      ("🍇" nil "738" nil)
      ("🍈" nil "736" nil)
      ("🍉" nil "238" "20328")
      ("🍊" nil "435" "20326")
      ("🍌" "59204" "739" nil)
      ("🍍" nil "737" nil)
      ("🍎" "59205" "434" "20325")
      ("🍏" nil "776" nil)
      ("🍑" nil "743" nil)
      ("🍒" "59202" "241" nil)
      ("🍓" nil "243" "20327")
      ("🍔" "58995" "245" "17728")
      ("🍕" nil "745" nil)
      ("🍖" nil "160" nil)
      ("🍗" nil "746" nil)
      ("🍘" nil "428" "20317")
      ("🍙" "59209" "244" "20322")
      ("🍚" nil "429" "20318")
      ("🍛" nil "431" "20321")
      ("🍜" "59212" "333" "20320")
      ("🍝" nil "430" "20319")
      ("🍞" "59213" "424" "20313")
      ("🍟" nil "426" "20315")
      ("🍠" nil "744" nil)
      ("🍡" nil "427" "20316")
      ("🍢" nil "432" "20323")
      ("🍣" nil "433" "20324")
      ("🍤" nil "798" nil)
      ("🍥" nil "275" nil)
      ("🍦" nil "425" "20314")
      ("🍧" nil "483" "20575")
      ("🍨" nil "760" nil)
      ("🍩" nil "761" nil)
      ("🍪" nil "762" nil)
      ("🍫" nil "763" nil)
      ("🍬" nil "764" nil)
      ("🍭" nil "765" nil)
      ("🍮" nil "772" nil)
      ("🍯" nil "775" nil)
      ("🍰" "59210" "239" "18278")
      ("🍱" nil "438" "20332")
      ("🍲" nil "439" "20333")
      ("🍳" nil "240" "17767")
      ("🍴" "58991" "146" "18275")
      ("🍵" "59166" "423" "20312")
      ("🍶" "59211" "400" "20267")
      ("🍷" "59222" "12" nil)
      ("🍸" "58993" "52" "18276")
      ("🍹" nil "748" nil)
      ("🍺" "58994" "65" "18279")
      ("🍻" nil "401" "20268")
      ("🎀" "59012" "312" "20276")
      ("🎁" "59013" "144" "17714")
      ("🎂" "59014" "313" "20331")
      ("🎃" nil "487" "20581")
      ("🎄" "59044" "234" "18259")
      ("🎅" nil "489" "20584")
      ("🎆" nil "357" "17719")
      ("🎇" nil "484" "20576")
      ("🎈" nil "404" "20272")
      ("🎉" nil "405" "20274")
      ("🎊" nil "230" nil)
      ("🎋" nil "747" nil)
      ("🎌" nil "370" "17763")
      ("🎍" nil "476" "20566")
      ("🎎" nil "477" "20568")
      ("🎏" nil "480" "20571")
      ("🎐" nil "486" "20578")
      ("🎑" nil "488" "20582")
      ("🎒" nil "479" "20570")
      ("🎓" nil "478" "20569")
      ("🎠" "59001" nil nil)
      ("🎡" nil "223" "17732")
      ("🎢" nil "475" "20563")
      ("🎣" nil "752" nil)
      ("🎤" "58998" "289" "18268")
      ("🎥" "58999" "110" "18269")
      ("🎦" nil nil "20775")
      ("🎧" "59002" "294" "20266")
      ("🎨" "59003" "309" "20770")
      ("🎩" "59004" "494" "20771")
      ("🎪" "59005" "311" nil)
      ("🎫" "59006" "106" "17733")
      ("🎬" "59052" "226" "20292")
      ("🎭" nil "310" nil)
      ("🎮" "59019" "232" nil)
      ("🎯" nil "231" "17744")
      ("🎰" nil "229" "17747")
      ("🎱" nil "470" "20556")
      ("🎲" nil "170" nil)
      ("🎳" nil "753" nil)
      ("🎴" nil "796" nil)
      ("🎵" "59126" "343" "18270")
      ("🎶" "59135" "291" "20294")
      ("🎷" nil nil "18272")
      ("🎸" nil "292" "18273")
      ("🎹" nil "750" nil)
      ("🎺" nil "469" "18274")
      ("🎻" nil "293" nil)
      ("🎼" nil "453" nil)
      ("🎽" "58962" nil nil)
      ("🎾" "58965" "220" "18229")
      ("🎿" "58967" "421" "18227")
      ("🏀" "58968" "307" "20554")
      ("🏁" "58969" "222" "17746")
      ("🏂" "59154" "221" nil)
      ("🏃" "59187" "218" "17717")
      ("🏄" nil "751" "18231")
      ("🏆" nil "364" "17745")
      ("🏈" nil "96" "20555")
      ("🏊" nil "471" "20557")
      ("🏠" "58979" "112" "18262")
      ("🏡" nil "514" nil)
      ("🏢" "58980" "156" "18264")
      ("🏣" "58981" "375" "17779")
      ("🏥" "58982" "376" "17781")
      ("🏦" "58983" "212" "17773")
      ("🏧" "58984" "205" "17780")
      ("🏨" "58985" "378" "17784")
      ("🏩" nil "492" "20769")
      ("🏪" "58986" "206" "17782")
      ("🏫" "59198" "377" "17783")
      ("🏬" nil "495" "20772")
      ("🏭" nil "498" "20776")
      ("🏮" nil "225" nil)
      ("🏯" nil "496" "20773")
      ("🏰" nil "497" "20774")
      ("🐌" "59214" "812" nil)
      ("🐍" nil "720" "20813")
      ("🐎" nil nil "17748")
      ("🐑" nil nil "20809")
      ("🐒" nil nil "20808")
      ("🐔" nil "721" "20814")
      ("🐗" nil "722" "20815")
      ("🐘" nil "717" "20806")
      ("🐙" nil "352" "17706")
      ("🐚" nil "485" "20577")
      ("🐛" nil "716" "20805")
      ("🐜" nil "253" nil)
      ("🐝" nil "773" nil)
      ("🐞" nil "774" nil)
      ("🐟" "59217" nil "18233")
      ("🐠" nil "715" "20802")
      ("🐡" nil "242" nil)
      ("🐢" nil "365" nil)
      ("🐣" nil "372" nil)
      ("🐤" "59215" "78" "20803")
      ("🐥" nil "804" nil)
      ("🐦" nil nil "20801")
      ("🐧" "59216" "252" "18293")
      ("🐨" nil "718" "20807")
      ("🐩" nil "74" nil)
      ("🐫" nil "723" "20816")
      ("🐬" nil "713" "20800")
      ("🐭" nil "347" "18291")
      ("🐮" nil "719" "20811")
      ("🐯" nil "345" "18288")
      ("🐰" nil "247" "20812")
      ("🐱" "59042" "251" "18287")
      ("🐲" nil "749" nil)
      ("🐳" nil "246" "18292")
      ("🐴" "59220" "248" "18234")
      ("🐵" nil "249" "17705")
      ("🐶" "59041" "134" "18290")
      ("🐷" "59221" "254" "17707")
      ("🐸" nil "250" "20817")
      ("🐹" nil nil "20804")
      ("🐺" nil nil "20810")
      ("🐻" nil "346" "18289")
      ("🐼" nil "756" nil)
      ("🐽" nil "758" nil)
      ("🐾" nil "276" nil)
      ("👀" "59025" "317" "20537")
      ("👂" "59026" "318" "20539")
      ("👃" nil "457" "20538")
      ("👄" nil "458" "20540")
      ("👅" nil "757" nil)
      ("👆" nil "390" "17998")
      ("👇" nil "391" "17999")
      ("👈" nil "140" "18000")
      ("👉" nil "141" "18001")
      ("👊" "59133" "281" "18221")
      ("👋" nil "463" "20542")
      ("👌" nil "461" "20544")
      ("👍" "59175" "287" "18222")
      ("👎" nil "462" "20545")
      ("👏" nil "460" "20543")
      ("👐" nil nil "20546")
      ("👑" "59162" "354" "17710")
      ("👒" nil "407" "20280")
      ("👓" "59034" "116" nil)
      ("👔" nil "396" "20258")
      ("👕" "59150" "335" "18214")
      ("👖" "59153" "805" nil)
      ("👗" nil "793" "20281")
      ("👘" nil "412" "20289")
      ("👙" nil "413" "20290")
      ("👚" nil "301" nil)
      ("👛" "59151" "290" nil)
      ("👜" "59010" "83" "20291")
      ("👝" "59053" nil nil)
      ("👞" nil "336" nil)
      ("👟" "59033" "729" "18215")
      ("👠" "58996" "124" "17758")
      ("👡" nil nil "20282")
      ("👢" nil "408" "20283")
      ("👣" "59032" "728" "20822")
      ("👤" "59057" nil nil)
      ("👦" nil nil "18209")
      ("👧" nil nil "18210")
      ("👨" nil "80" "18212")
      ("👩" nil "50" "18213")
      ("👪" nil "163" nil)
      ("👫" nil nil "20552")
      ("👮" nil "374" "17778")
      ("👯" nil "468" "20553")
      ("👰" nil "482" nil)
      ("👱" nil "705" "20789")
      ("👲" nil "706" "20790")
      ("👳" nil "707" "20791")
      ("👴" nil "708" "20792")
      ("👵" nil "709" "20793")
      ("👶" nil "710" "20794")
      ("👷" nil "711" "20795")
      ("👸" nil "712" "20796")
      ("👹" nil "754" nil)
      ("👺" nil "755" nil)
      ("👻" nil "236" "17723")
      ("👼" nil "344" "18286")
      ("👽" nil "302" "17708")
      ("👾" nil "274" "17739")
      ("👿" nil "277" "17722")
      ("💀" nil "286" "17724")
      ("💁" nil nil "18035")
      ("💂" nil nil "20798")
      ("💃" nil "714" "20799")
      ("💄" "59152" "295" "20284")
      ("💅" nil "409" "20285")
      ("💆" nil "297" "20286")
      ("💇" nil "410" "20287")
      ("💈" nil "411" "20288")
      ("💉" nil "304" "17755")
      ("💊" nil "403" "20271")
      ("💋" "59129" "273" "18211")
      ("💌" "59159" "806" nil)
      ("💍" "59163" "72" "18260")
      ("💎" nil nil "18261")
      ("💏" nil "355" "17713")
      ("💐" nil "398" "20262")
      ("💑" nil "467" "20549")
      ("💒" nil nil "20573")
      ("💓" "59117" "803" "20295")
      ("💔" "59118" "265" "18243")
      ("💕" "59119" "266" nil)
      ("💖" nil "415" nil)
      ("💗" nil nil "20296")
      ("💘" nil "272" "20297")
      ("💙" nil "416" "20298")
      ("💚" nil "417" "20299")
      ("💛" nil "418" "20300")
      ("💜" nil "419" "20301")
      ("💝" nil "770" "20567")
      ("💞" nil "328" nil)
      ("💟" nil nil "17956")
      ("💠" "59128" nil nil)
      ("💡" "59131" "77" "17711")
      ("💢" "59132" "262" "20308")
      ("💣" "59134" "268" "20273")
      ("💤" "59137" "261" "17756")
      ("💥" "59141" "329" nil)
      ("💦" "59142" "330" "20305")
      ("💧" "59143" "263" nil)
      ("💨" "59144" "282" "20304")
      ("💩" nil "283" "18298")
      ("💪" nil "271" "17772")
      ("💫" nil "778" nil)
      ("💬" nil "86" nil)
      ("💮" nil "278" nil)
      ("💯" nil "280" nil)
      ("💰" "59157" "233" "17743")
      ("💱" nil nil "17769")
      ("💲" nil "14" nil)
      ("💳" nil "87" nil)
      ("💴" "59094" "109" nil)
      ("💵" nil "139" nil)
      ("💸" nil "777" nil)
      ("💹" nil "373" "17770")
      ("💺" "59058" nil "17727")
      ("💻" "59158" "337" "18220")
      ("💼" nil "359" "17726")
      ("💽" nil "126" "20278")
      ("💾" nil "59" nil)
      ("💿" "59020" "300" "17734")
      ("📀" nil nil "17735")
      ("📁" nil "79" nil)
      ("📂" nil "84" nil)
      ("📃" nil "56" nil)
      ("📄" nil "103" nil)
      ("📅" nil "67" nil)
      ("📆" nil "105" nil)
      ("📇" nil "131" nil)
      ("📈" nil "128" nil)
      ("📉" nil "159" nil)
      ("📊" nil "127" nil)
      ("📋" nil "92" nil)
      ("📌" nil "137" nil)
      ("📍" nil "49" nil)
      ("📎" "59184" "143" nil)
      ("📏" nil "157" nil)
      ("📐" nil "158" nil)
      ("📑" nil "516" nil)
      ("📒" nil "142" nil)
      ("📓" nil "121" nil)
      ("📔" nil "91" nil)
      ("📕" nil "102" nil)
      ("📖" "59011" "122" "17768")
      ("📗" nil "97" nil)
      ("📘" nil "100" nil)
      ("📙" nil "101" nil)
      ("📚" nil "147" nil)
      ("📛" nil "145" nil)
      ("📜" nil "136" nil)
      ("📝" "59017" "395" "20257")
      ("📞" nil "155" nil)
      ("📟" "58970" "308" nil)
      ("📠" "59088" "166" "18219")
      ("📡" nil "210" "17771")
      ("📢" nil nil "17762")
      ("📣" nil nil "20279")
      ("📤" nil "153" nil)
      ("📥" nil "154" nil)
      ("📦" nil "165" nil)
      ("📧" nil "799" nil)
      ("📨" nil "151" nil)
      ("📩" "59087" "784" "17699")
      ("📪" nil "129" nil)
      ("📫" nil "515" "17697")
      ("📮" nil nil "17698")
      ("📰" nil "171" nil)
      ("📱" "59016" "161" "18218")
      ("📲" "59086" "513" "17700")
      ("📳" nil "393" "18032")
      ("📴" nil "394" "18033")
      ("📶" nil "381" "17963")
      ("📷" "59009" "94" "18216")
      ("📹" nil "111" nil)
      ("📺" "59018" "288" "17738")
      ("📻" nil "338" "17736")
      ("📼" nil "115" "17737")
      ("🔃" nil "518" nil)
      ("🔊" nil "13" "17761")
      ("🔋" nil "135" nil)
      ("🔌" nil "162" nil)
      ("🔍" "59100" "119" "17716")
      ("🔎" nil "510" nil)
      ("🔏" nil "517" nil)
      ("🔐" nil "501" nil)
      ("🔑" "59097" "120" "18271")
      ("🔒" nil "138" "17764")
      ("🔓" nil nil "17765")
      ("🔔" "59155" "48" "20293")
      ("🔖" nil "512" nil)
      ("🔗" nil "164" nil)
      ("🔘" nil "509" nil)
      ("🔙" nil "511" nil)
      ("🔚" "59065" nil nil)
      ("🔛" "59064" nil nil)
      ("🔜" "59063" nil nil)
      ("🔝" nil nil "18028")
      ("🔞" nil "380" "17959")
      ("🔟" nil "189" nil)
      ("🔠" nil "502" nil)
      ("🔡" nil "503" nil)
      ("🔢" nil "504" nil)
      ("🔣" nil "505" nil)
      ("🔤" nil "771" nil)
      ("🔥" nil "269" "17725")
      ("🔦" nil "130" nil)
      ("🔧" "59160" "152" nil)
      ("🔨" nil "356" "17718")
      ("🔩" nil "123" nil)
      ("🔪" nil "114" nil)
      ("🔫" nil "296" "17715")
      ("🔮" nil "392" nil)
      ("🔯" nil nil "18014")
      ("🔰" nil "179" "17961")
      ("🔱" nil nil "18257")
      ("🔲" nil nil "17978")
      ("🔳" nil nil "17979")
      ("🔴" nil "40" "17977")
      ("🔵" nil "41" nil)
      ("🔶" nil "36" nil)
      ("🔷" nil "37" nil)
      ("🔸" nil "19" nil)
      ("🔹" nil "20" nil)
      ("🔺" nil "88" nil)
      ("🔻" nil "89" nil)
      ("🔼" nil "33" nil)
      ("🔽" nil "32" nil)
      ("🕐" nil nil "18244")
      ("🕑" nil nil "18245")
      ("🕒" nil nil "18246")
      ("🕓" nil nil "18247")
      ("🕔" nil nil "18248")
      ("🕕" nil nil "18249")
      ("🕖" nil nil "18250")
      ("🕗" nil nil "18251")
      ("🕘" nil nil "18252")
      ("🕙" nil nil "18253")
      ("🕚" nil nil "18254")
      ("🕛" nil nil "18255")
      ("🗻" "59200" "342" "18267")
      ("🗼" nil "228" "20777")
      ("🗽" nil nil "20797")
      ("🗾" nil "214" nil)
      ("🗿" nil "794" nil)
      ("😁" "59219" "814" "20516")
      ("😂" nil "786" "20530")
      ("😃" "59120" "257" "18295")
      ("😄" nil nil "20533")
      ("😅" "59170" nil nil)
      ("😆" "59178" nil nil)
      ("😉" "59177" "348" "20517")
      ("😊" nil "454" "18294")
      ("😋" "59218" nil nil)
      ("😌" "59169" "446" "20522")
      ("😍" "59174" "349" "17702")
      ("😏" "59180" "440" "20514")
      ("😒" "59173" "450" "20526")
      ("😓" "59171" "351" "17704")
      ("😔" "59168" "441" "20515")
      ("😖" "59123" "444" "20519")
      ("😘" nil "456" "20536")
      ("😚" nil "455" "20535")
      ("😜" "59176" "264" "17701")
      ("😝" nil nil "20521")
      ("😞" "59122" nil "18296")
      ("😠" "59121" "258" "18297")
      ("😡" "59172" "779" "20534")
      ("😢" "59182" "791" "20531")
      ("😣" "59179" "443" "20518")
      ("😤" nil "442" nil)
      ("😥" nil nil "20513")
      ("😨" nil "447" "20523")
      ("😩" nil "789" nil)
      ("😪" nil "445" "20520")
      ("😫" nil "260" nil)
      ("😭" "59181" "259" "20529")
      ("😰" nil "452" "20527")
      ("😱" "59223" "350" "17703")
      ("😲" nil "451" "20528")
      ("😳" nil "449" "20525")
      ("😵" "59124" "327" nil)
      ("😷" nil "448" "20524")
      ("😸" nil "813" nil)
      ("😹" nil "785" nil)
      ("😺" nil "783" nil)
      ("😻" nil "787" nil)
      ("😼" nil "792" nil)
      ("😽" nil "782" nil)
      ("😾" nil "780" nil)
      ("😿" nil "790" nil)
      ("🙀" nil "788" nil)
      ("🙅" nil "464" "20547")
      ("🙆" nil "465" "20548")
      ("🙇" nil "466" "20550")
      ("🙈" nil "766" nil)
      ("🙉" nil "768" nil)
      ("🙊" nil "767" nil)
      ("🙋" nil "819" nil)
      ("🙌" nil "820" "20551")
      ("🙍" nil "821" nil)
      ("🙎" nil "822" nil)
      ("🙏" nil "459" "20541")
      ("🚀" nil "353" "17709")
      ("🚃" "58971" "172" "18238")
      ("🚄" "58973" nil "20565")
      ("🚅" nil "217" "18239")
      ("🚇" nil "341" "20564")
      ("🚉" nil "795" "18265")
      ("🚌" "58976" "216" "17785")
      ("🚏" nil "209" "17776")
      ("🚑" nil "473" "20561")
      ("🚒" nil "472" "20560")
      ("🚓" nil "474" "20562")
      ("🚕" nil nil "17786")
      ("🚗" "58974" "125" "18235")
      ("🚙" "58975" nil "20558")
      ("🚚" nil "148" "20559")
      ("🚢" "58977" "379" "17954")
      ("🚤" nil nil "17749")
      ("🚥" "58989" "99" "17774")
      ("🚧" nil "368" "17751")
      ("🚨" nil "801" nil)
      ("🚩" "59102" "730" nil)
      ("🚪" "59156" nil nil)
      ("🚫" nil "31" nil)
      ("🚬" "59007" "176" "20270")
      ("🚭" "59008" "177" "17960")
      ("🚲" "59165" "215" "17750")
      ("🚶" nil "800" "17953")
      ("🚹" nil nil "17752")
      ("🚺" nil nil "17753")
      ("🚻" "58990" "207" "17777")
      ("🚼" nil nil "17754")
      ("🚽" nil nil "17760")
      ("🚾" nil nil "20265")
      ("🛀" nil "369" "17759")
      ))))

(defvar emoji-display-emoji-regexp
  (eval-when-compile
    (regexp-opt (mapcar 'car emoji-display-table))))


;; Internal variables
(defvar emoji-display-create-image-function
  (if (fboundp 'create-animated-image) 'create-animated-image
    'create-image))

(defvar emoji-display-cache nil)

(defvar emoji-display-internal nil)


;; Internal functions.
(defsubst emoji-display-type-directory (type)
  (expand-file-name (cdr (assq type emoji-display-type-subdir-alist))
		    emoji-display-image-directory))

(defun emoji-display-display (start end length)
  "Hook function for `after-change-functions' to display emoji image."
  (unless emoji-display-internal
    (let ((emoji-display-internal t))
      (save-match-data
	(emoji-display-display-region start end)))))

(defun emoji-display-display-buffer ()
  "Hook function for `find-file-hook' to display emoji image."
  (unless emoji-display-internal
    (let ((emoji-display-internal t))
      (save-match-data
	(emoji-display-display-region (point-min) (point-max))))))

(defun emoji-display-display-region (start end)
  "Add emoji display properties to region."
  (save-excursion
    (goto-char start)
    (let ((inhibit-read-only t)
	  (modified (buffer-modified-p)))
      (while (re-search-forward emoji-display-emoji-regexp end t)
	(let ((image (emoji-display-get-image (match-string 0))))
	  (when image
	    (add-text-properties (match-beginning 0) (match-end 0)
				 `(display ,image emoji-display t))
	    (when (and emoji-display-animated-p
		       (fboundp 'image-animate))
	      (image-animate image nil emoji-display-animate-limit)))))
      (set-buffer-modified-p modified))))

(defun emoji-display-undisplay-region (start end)
  "Remove emoji display properties to region."
  (save-excursion
    (goto-char start)
    (let ((point start)
	  (inhibit-read-only t)
	  (modified (buffer-modified-p)))
      (while (null (eq point end))
	(goto-char (next-single-property-change point 'emoji-display nil end))
	(when (get-text-property point 'emoji-display)
	  (remove-list-of-text-properties point (point)
					  '(emoji-display display)))
	(setq point (point)))
      (set-buffer-modified-p modified))))

(defun emoji-display-get-image (string)
  (set-text-properties 0 (length string) nil string)
  (if (assoc string emoji-display-cache)
      (copy-sequence (cdr (assoc string emoji-display-cache)))
    (let ((list (assoc string emoji-display-table))
	  (types emoji-display-types)
	  name file image data)
      (while (and types (null image))
	(if (and (setq name
		       (nth (length
			     (memq (assq (car types)
					 emoji-display-type-subdir-alist)
				   emoji-display-type-subdir-alist))
			    list))
		 (file-readable-p
		  (setq file (expand-file-name
			      (format "%s.gif" name)
			      (emoji-display-type-directory (car types))))))
	    (progn
	      ;; (with-temp-buffer
	      ;; 	(set-buffer-multibyte nil)
	      ;; 	(insert-file-contents-literally file)
	      ;; 	(setq data (buffer-string)))
	      ;; (setq image (funcall emoji-display-create-image-function
	      ;; 			   data 'gif t :ascent 'center))
	      (setq image (funcall emoji-display-create-image-function
	       			   file 'gif nil :ascent 'center))
	      (setq emoji-display-cache
		    (cons (cons string image)
			  emoji-display-cache)))
	  (setq types (cdr types))))
      image)))


;; UI functions
;;;###autoload
(define-minor-mode emoji-display-mode
  "Toggle emoji displaying mode."
  :global t
  :group 'emoji-display
  :init-value nil
  (cond
   (emoji-display-mode
    (save-match-data
      (mapc (lambda (buffer)
	      (with-current-buffer buffer
		(save-restriction
		  (widen)
		  (emoji-display-display-region (point-min) (point-max)))))
	    (buffer-list))
      (add-hook 'after-change-functions 'emoji-display-display)
      (add-hook 'find-file-hook 'emoji-display-display-buffer)))
   (t
    (remove-hook 'after-change-functions 'emoji-display-display)
    (remove-hook 'find-file-hook 'emoji-display-display-buffer)
    (save-match-data
      (mapc (lambda (buffer)
	      (with-current-buffer buffer
		(save-restriction
		  (widen)
		  (emoji-display-undisplay-region (point-min) (point-max)))))
	    (buffer-list))))))

(defun emoji-display-cache-clear ()
  "Clear emoji-display image cache."
  (interactive)
  (setq emoji-display-cache nil))

(provide 'emoji-display)
;;; emoji-display.el ends here