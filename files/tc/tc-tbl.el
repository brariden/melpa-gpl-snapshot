;;; tc-tbl.el --- T-Code-dependent data

;; Copyright (C) 1989--2001 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>

;; $Id: tc-tbl.el,v 1.10 2002/12/18 02:03:31 kitajima Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Commentary:
;;
;; set following variables:
;;	tcode-input-method
;;	tcode-transparent-mode-indicator
;;	tcode-tcode-mode-indicator
;;	tcode-alnum-2byte-tcode-mode-indicator
;;	tcode-hiragana-mode-indicator
;;	tcode-katakana-mode-indicator
;;	tcode-tbl
;;	tcode-non-2-stroke-char-list
;;	tcode-another-table
;;	tcode-special-commands-alist
;;	tcode-mode-help-string
;;	tcode-stroke-file-name
;;      eelll-text

;;; Code:

(require 'tc)

(setq tcode-input-method 'tcode)

(setq tcode-transparent-mode-indicator "--"
      tcode-tcode-mode-indicator "TC"
      tcode-alnum-2byte-tcode-mode-indicator "Ｔ"
      tcode-hiragana-mode-indicator ""
      tcode-katakana-mode-indicator "")

(setq tcode-tbl [
"■■■■■■■■■■ヮヰヱヵヶ請境系探象ゎゐゑ■■盛革突温捕■■■■■依繊借須訳"
"■■■■■■■■■■丑臼宴縁曳尚賀岸責漁於汚乙穏■益援周域荒■■■■■織父枚乱香"
"■■■■■■■■■■鬼虚狭脅驚舎喜幹丘糖奇既菊却享康徒景処ぜ■■■■■譲ヘ模降走"
"■■■■■■■■■■孤誇黄后耕布苦圧恵固巧克懇困昏邦舞雑漢緊■■■■■激干彦均又"
"■■■■■■■■■■奉某貌卜■姿絶密秘押■■■■■衆節杉肉除■■■■■測血散笑弁"
"■■■■■■■■■■湖礼著移郷■■■■■償欧努底亜■■■■■禁硝樹句礎■■■■■"
"■■■■■■■■■■端飾郵塩群■星析遷宣紅傷豪維脱鼠曹奏尊■絹被源願臨■■■■■"
"■■■■■■■■■■刷寿順危砂庶粧丈称蒸舗充喫腕暴■■■■■批慶渉竜併■■■■■"
"■■■■■■■■■■震扱片札乞■乃如尼帳輪倒操柄魚■■■■■就駐揮丹鮮■■■■■"
"■■■■■■■■■■弘痛票訴遺欄龍略慮累則存倍牛釈■■■■■綱潟創背皮■■■■■"
"ヲ哀暇啓把酸昼炭稲湯果告策首農歩回務島開報紙館夜位給員ど代レ欠夏彼妻善相家的対歴"
"ゥ逢牙掲伐貿捜異隣旧概買詳由死キせ区百木音王放々応分よル千ア財針裏居差付プばュ作"
"ヴ宛壊携避攻焼闘奈夕武残両在! や出タ手保案曲情引職7 か( トれ従骨厚顔量内工八テ見"
"ヂ囲較劇卑盤帯易速拡風階能論増コ山者発立横興刺側覚きっ日国二適類御宇推九名川機チ"
"ヅ庵寒賢藩汽換延雪互細古利ペゃナ金マ和女崎白ぐ官球上く8 え年母奥因酒伸サ建パ第入"
"簡徴触宗植■索射濁慢害賃整軽評佐法数郎談服声任検豊美題井洋実爆仲茶率比昔短岩巨敗"
"承章候途複■冊需詑迷撃折追隊角接備最急験変審改昇芸宿制集安画陽構旅施曜遠ォ将ぞ塚"
"快否歯筆里■皿輯蓄戻浴秀糸春幸記朝知ワ送限研労統役セ運ツ特谷ァ導認健尾序振練念働"
"包納頼逃寝■賛瞬貯羊積程断低減モ資士費ィ逆企精ざ印神び打勤ャ殺負何履般耳授版効視"
"唱暮憲勉罪■■盾虫■故鉱提児敷無石屋解募令違装然確優公品語演券悪秋非便示即難普辺"
"ぱ慰我兼菱桜瀬鳥催障収際太園船中スもお定種岡結進真3 と〇てるヒ江別考権ッ人三京ち"
"ぴ為掛嫌紐典博筋忠乳若雄査ふ賞わラ東生ろ宅熟待取科ーした一が及久蔵早造ロク万方フ"
"ぷ陰敢顕描採謡希仏察指氏丸続ェう4 ) 十リ料土活ね参い、の5 1 投義算半県んまンつ四"
"ぺ隠甘牽憤君純副盟標ぎ格次習火あこ6 学月受予切育池。◆0 ・2 込沢軍青清けイす電地"
"ぽ胃患厳弊犯余堀肩療思術広門聞本さら高シ英ボ加室少ではになを転空性使級業時「長み"
"朱遅甲致汎■衰滋沈己病終起路越む南原駅物勢必講愛管要設水藤有素兵専親寮ホ共ブ平楽"
"陣鶴鹿貨絡■趨湿添已常張薬防得ケ式戦関男輸形助◇流連鉄教力ベ毛永申袋良私ゴ来信午"
"眼繁誌招季■垂甚徹巳寺質づ港条話座線ダ橋基好味宝争デ現エ他度等浅頃落命村ガ製校ご"
"執紹夢卸阿■粋■爪巴停領容玉右べ民ソ点遇足草築観言車成天世文板客師税飛ノ完重約各"
"岳刑弱雲窓■寸瞳陶■河置供試席期ゾ歳強係婦段衛額渋主映書可へ伝庭課着坂近外米ョ光"
"ぁ■瓦■■呼幅歓功盗徳渡守登退店持町所ほ件友卒初慣行ド円小ジヨ誤証含% 海道ず西げ"
"ぃ■■■■紀破郡抗幡械刊訪融雨全じ自議明宮伊求技写通カ社野同判規感値ギ当理メウグ"
"ぅ■■■■房績識属衣帝始了極熱バ部六経動局頭配黒院だり—め大済吉ゆ器照不合面政オ"
"ぇ■■■■去疑ぢ綿離読鈴恐督況後間場ニ産向府富直倉新」9 子五説週号葉派委化ビ目市"
"ぉ■■■■秒範核影麻族丁未才返問ム七住北割ぶ番望元事田会前そ休省央福毎気売下都株"
"欲巣茂述朗■■■■■帰庁昨跡ゲ洗羽個医静億録赤想消支協用表正図挙険ゼ波ヤ心界意今"
"迫災恋脳老■■■■■監寄裁達芝響忘討史環色貸販編仕先多商ハ交之末ぼ街免再ネ〜口台"
"留列刻豆看■■■■■竹注介具失司迎華許補左態花栄ザ調混ポ決ミ州払乗庫状団計夫食総"
"替沼? 辞献■■■■■ゅ修究答養復並浦ユ冷ぬ展警型誰組選党択体例満津準遊戸ひょ価与"
"還更占箱矢■■■■■志抜航層深担陸巻競護根様独止堂銀以ヌ営治字材過諸単身ピ勝反ズ"
])

(setq tcode-non-2-stroke-char-list
      (mapcar (function
	       (lambda (str)
		 (tcode-string-to-char str)))
	      '("■" "◆" "◇")))

(setq tcode-another-table nil)

(setq tcode-special-commands-alist
      '(((0 0) . (lambda () (tcode-show-tables nil nil)))
				       ; 11 : LL表の表示
	((0 9) . (lambda () (tcode-show-tables nil t)))
				       ; 10 : LR表の表示
	((9 0) . (lambda () (tcode-show-tables t nil)))
				       ; 01 : RL表の表示
	((9 9) . (lambda () (tcode-show-tables t t)))
				       ; 00 : RR表の表示
	((1 1) . tcode-start-jiscode)
				 ; 22 : JIS コード表入力
	((2 2) . tcode-toggle-alnum-mode)
				; 33 : 1-2バイト切り換え
	((2 1) . tcode-switch-variable)
				   ; 32 : 句読点のトグル
	((3 3) . (lambda ()
		   (tcode-display-stroke-sequence tcode-last-help-char-list)))
					; 44 : ヘルプ
	((4 4) . (lambda () (tcode-query-stroke (point))))
					; 55 : ヘルプ
	((6 6) . tcode-bushu-begin-alternate-conversion)
			     ; 77 : postfix 部首合成変換
	((7 7) . (lambda () (tcode-transpose-strokes nil)))
				; 88 : transpose-strokes
	((8 8) . tcode-clear)
		     ; 99 : 部首合成変換などのキャンセル
	((26 23) . tcode-bushu-begin-conversion) ; jf : 部首合成変換の開始
	((25 23) . tcode-kuten)
	((26 22) . tcode-touten)
	((23 26) . tcode-mazegaki-begin-conversion)
				      ; fj: 交ぜ書き変換
	((9 8) . tcode-mazegaki-begin-alternate-conversion)
			  ; 前置・後置が逆の交ぜ書き変換
	;; 「18」で読み1文字の後置型交ぜ書き変換
	((0 7) . (lambda ()
		   (tcode-mazegaki-convert 1 current-prefix-arg)))
	;; 「28」で読み2文字の後置型交ぜ書き変換
	((1 7) . (lambda ()
		   (tcode-mazegaki-convert 2 current-prefix-arg)))

	;; 「38」で読み3文字の後置型交ぜ書き変換
	((2 7) . (lambda ()
		   (tcode-mazegaki-convert 3 current-prefix-arg)))

	;; 「48」で読み4文字の後置型交ぜ書き変換
	((3 7) . (lambda ()
		   (tcode-mazegaki-convert 4 current-prefix-arg)))

     ;; 「58」で活用する語を対象とした後置型交ぜ書き変換
	((4 7) . (lambda () (tcode-mazegaki-convert nil t)))

	;; 「29」で読み2文字の活用する語を対象とした
	;; 後置型交ぜ書き変換
	((1 8) . (lambda () (tcode-mazegaki-convert 2 t)))

	;; 「39」で読み3文字の活用する語を対象とした
	;; 後置型交ぜ書き変換
	((2 8) . (lambda () (tcode-mazegaki-convert 3 t)))

	;; 「49」で読み4文字の活用する語を対象とした
	;; 後置型交ぜ書き変換
	((3 8) . (lambda () (tcode-mazegaki-convert 4 t)))

	;; 「59」で読み5文字の活用する語を対象とした
	;; 後置型交ぜ書き変換
	((4 8) . (lambda () (tcode-mazegaki-convert 5 t)))))

(setq tcode-mode-help-string "\
Tコードモード中のキー操作は次のとおり。
   jf : 部首合成変換モードに入る。jfを打ち続けると再帰的に部首合成変換を
	行うことができる(see variable `tcode-use-postfix-bushu-as-default')。
   fj : 交ぜ書き変換を行う(see variable `tcode-use-prefix-mazegaki')。
   00, 01, 10, 11 : Tコードの表を表示する(0が左、1が右)。
   22 : JIS コード一覧表による入力。
   32 : 、。と, . を切り替える。(see variable `tcode-switch-table-list')。
   33 : Tコード表にある英数字・記号の文字コードの1バイト・2バイト切り替え。
   44 : 直前に表示した打ち方を再表示する。
   55 : ポイント位置にある文字の打ち方を表示する。
   58 : 活用語を優先して交ぜ書き変換を行う。
   77 : ポイント前にある2文字で部首合成変換を行う。
   88 : ポイント位置にある文字を逆ストローク化する(例: 味->の)。
        行末ではポイントの直前の文字を変換する。
   99 : 交ぜ書き変換モードや部首合成変換モードにいた時に、
	それらを全部キャンセルする。また、ヘルプを消す。
   [1-4]8, [2-5]9: 文字数を指定して交ぜ書き変換を行う。
   \\[toggle-input-method] : Tコードモードを抜ける。

初めて起動された時には，`tcode-ready-hook' を実行する。
また、起動される度に`tcode-toggle-hook'を実行する。")

(setq tcode-stroke-file-name (concat tcode-data-directory "tcode.st"))

(setq eelll-text "EELLLTXT")

;;; tc-tbl.el ends here
