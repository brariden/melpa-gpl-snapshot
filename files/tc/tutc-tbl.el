;;; tutc-tbl.el --- do TUT-Code on T-Code driver tc.el

;; Copyright (C) 1997--2001 KITAJIMA Akira

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 4 Mar 1997
;; Version: $Id: tutc-tbl.el,v 1.11 2003/05/18 08:39:37 kitajima Exp $

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

;;; Code:

(require 'tc)

(setq tcode-input-method 'tutcode)

(setq tcode-transparent-mode-indicator "------"
      tcode-tcode-mode-indicator "TUT"
      tcode-alnum-2byte-tcode-mode-indicator "Ｔ "
      tcode-hiragana-mode-indicator " ひ"
      tcode-katakana-mode-indicator " カ")

;;  1  2  3  4	5    6	7  8  9	 0
;;  q  w  e  r	t    y	u  i  o	 p
;;  a  s  d  f	g    h	j  k  l	 ;
;;  z  x  c  v	b    n	m  ,  .	 /

;;  0  1  2  3	4    5	6  7  8	 9
;; 10 11 12 13 14   15 16 17 18 19
;; 20 21 22 23 24   25 26 27 28 29
;; 30 31 32 33 34   35 36 37 38 39

;;  ---> 一打目のキー
;; |
;; v 二打目のキー
; 1 2 3 4 5 6 7 8 9 0 q w e r t y u i o p a s d f g h j k l ; z x c v b n m , . /
(setq tcode-tbl [
nil nil nil nil nil nil nil nil nil nil ; 1-0
"■■■■■■■■■■並態両乗専興口洋船久悪病早糸試松安都与清伸羽静財始河越統秀油" ;q
"■■■■■■■■■■伝健待白港水産務駅頭根仕無案額動川給木佐裏短何積迎元蔵県築角" ;w
"■■■■■■■■■■青活結優最金地万建鉄天今各関運高国三八北類非補軽率崎宿可室渋" ;e
"■■■■■■■■■■整少信報太町七京理営寮広平音計山電五小原復遊施帰便半岡石休豊" ;r
"■■■■■■■■■■□□□□□第和区西富□□□□□名部業問近□□□□□林門格吉齢" ;t
"■■■■■■■■■■食持子出行□□□□□書場会上通□□□□□落情私取別□□□□□" ;y
"■■■■■■■■■■ゆむくうふ割及楽園終目すつぬる製種用界容好館調知切庁之旅仲極" ;u
"■■■■■■■■■■相みきいひ民表夫加能ゐしちにり連車以完技器進対要意然断険構郵" ;i
"■■■■■■■■■■解気者手的球院番常領期発人員生由局体薬段読求演術身字養撃司居" ;o
"■■■■■■■■■■配予議特法果等首毛券続総定女成級課育色巨達限応買感閣担将械護" ;p
"■■■■■■■■■■念右残赤形遇井島士坪深管勤増習藤付経正賞奏示児志未研葉倉菱浴" ;a
"■■■■■■■■■■税質団参基機野前百接毎福他米利代月田後商医単貸追挙丸卒朝農億" ;s
"■■■■■■■■■■黒資数品座学本大六株約有全自住東二日時不評庭改役花急海市支森" ;d
"■■■■■■■■■■警託公細費工事中九午直送保世現新年一同歩討具防素介光郎文沢馬" ;f
"■■■■■■■■■■□□□□□内千分立外□□□□□円四十面教□□□□□次宅村古袋" ;g
"■■■■■■■■■■主めけえへ□□□□□ゑせてねれ□□□□□判投戦組多□□□□□" ;h
"■■■■■■■■■■よもこおほ勝氏画心販をそとのろ力験間設号策備当来強良様省型故" ;j
"■■■■■■■■■■やまかあは歳度校銀転わさたなら化下回歴係若記委入実論値路装働" ;k
"■■■■■■■■■■□見□□□紙英題告勢□□□ん社夜物所際側任引党受思般編印敗授" ;l
"■■■■■■■■■■空線合家集王衛劇親爆歌明長方政位在式済究置争義査派末塁状即辺" ;;
"■■■■■■■■■■返競母熱退企服鋼停州星許街鈴帝精友収寺履雑推渡効宇振鉱敷融浦" ;z
"■■■■■■■■■■祭訪諸修版伊週階労板族従堂兵監浜戸料火坂刊徳登冷響津塚尾竹泉" ;x
"■■■■■■■■■■曇注免茶昨科屋映阪昇賃晴独観変南谷道共江酒御守程史草条栄庫波" ;c
"■■■■■■■■■■□□□□□央池制証浅□□□□□横台橋武宝□□□□□芝幸玉曜環" ;v
"■■■■■■■■■■□□□□□昭宮職雄春□□□□□軍土神造芸□□□□□岩城永秋陸" ;b
"■■■■■■■■■■必官治先初□□□□□助性話語点□□□□□負認愛着消□□□□□" ;n
"■■■■■■■■■■講府談協反□□□□□写募作交重□□□□□失想流検再□□□□□" ;m
"■■■■■■■■■■使真開決美隊飛航寄満権選売店男含聞件師低起命説婦言署害個巻雨" ;,
"■■■■■■■■■■左算指向放減陽払顔普申風打価込比止審殺例抜導席録張華量視彼才" ;.
"■■■■■■■■■■展得死輸味丁善材差洗確足考望規準難裁適図違声提答過厚督練況景" ;/
])

(setq tcode-non-2-stroke-char-list (list (tcode-string-to-char "■")
					 (tcode-string-to-char "□")))

(setq tcode-another-table
      [ nil  nil  nil  nil  nil	   nil	nil  nil  nil  nil
	"“" "〒" "ー" "【" "「"   "」" "】" "…" "・" "”"
	"‘" "☆" tcode-touten tcode-kuten "『"	  "』" "々" "※" "＼" "’"
	"〆" "§" "○" "÷" "←"   "→" "×" "《" "》" "／"])

(defconst tut-over-2-strokes-table
  '((10 ; q
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "智" "忘" "魅" "嫁" "嘆"
	     nil nil nil nil nil   "範" "救" "継" "詰" "寸"
	     nil nil nil nil nil   "奔" "娠" "胆" "飽" "筑"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "哲" "錦" "蒲" "誘" "匠"
	     nil nil nil nil nil   "尚" "詩" "快" "骨" "靴"
	     nil nil nil nil nil   "喰" "符" "怖" "餌" "隼"])
	(28 ; l
	 (16 . "ゅ") ; u
	 (26 . "ょ") ; j
	 (27 . "ゃ")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "桟" "訊" "嫌" "輔" "苫"
	     nil nil nil nil nil   "繕" "雰" "胞" "杏" "呪"
	     nil nil nil nil nil   "肋" "燕" "灸" "麹" "倦"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "璽" "葵" "悼" "睦" "樺"
	     nil nil nil nil nil   "葦" "芋" "鍋" "胤" "咳"
	     nil nil nil nil nil   "灼" "謬" "夷" "綻" "弄"]))

    (11 ; w
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "誠" "覚" "述" "損" "釈"
	     nil nil nil nil nil   "紹" "臨" "砂" "双" "軒"
	     nil nil nil nil nil   "痴" "揃" "貧" "楼" "韻"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "弾" "罪" "候" "剤" "淵"
	     nil nil nil nil nil   "憲" "沖" "麻" "荘" "奇"
	     nil nil nil nil nil   "窒" "廊" "埋" "穏" "傘"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "頃" "漂" "班" "匿" "殆"
	     nil nil nil nil nil   "似" "辻" "覧" "汁" "幽"
	     nil nil nil nil nil   "碧" "搾" "濡" "昏" "灘"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "尼" "忌" "滴" "恰" "甥"
	     nil nil nil nil nil   "某" "庄" "諮" "胡" "嵩"
	     nil nil nil nil nil   "叩" "賑" "淫" "碓" "捺"]))

    (12 ; e
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "訳" "秘" "令" "隣" "厳"
	     nil nil nil nil nil   "走" "妻" "革" "属" "脳"
	     nil nil nil nil nil   "威" "患" "略" "是" "削"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "炭" "績" "盛" "弁" "宗"
	     nil nil nil nil nil   "肉" "丘" "杉" "帯" "紅"
	     nil nil nil nil nil   "侵" "黄" "綱" "胃" "唄"])
	(28 ; l
	 (12 (25 . "ヶ")  ; e h
	     (27 . "ヵ"))  ; e k
	 (16 . "ぐ") ; u
	 (17 . "ぎ") ; i
	 (25 . "げ") ; h
	 (26 . "ご") ; j
	 (27 . "が")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "篇" "畜" "墨" "潔" "暫"
	     nil nil nil nil nil   "泰" "聴" "乃" "戒" "羊"
	     nil nil nil nil nil   "銑" "菩" "魂" "惟" "鞍"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "拝" "滞" "菌" "賄" "漬"
	     nil nil nil nil nil   "冊" "晩" "射" "髪" "揺"
	     nil nil nil nil nil   "頬" "丙" "廻" "圭" "宋"]))

    (13 ; r
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "源" "雪" "測" "慶" "惑"
	     nil nil nil nil nil   "香" "処" "請" "抗" "跡"
	     nil nil nil nil nil   "耐" "句" "克" "択" "孔"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "稲" "昼" "激" "歯" "鬼"
	     nil nil nil nil nil   "衣" "散" "捕" "捜" "窓"
	     nil nil nil nil nil   "墓" "脚" "贈" "聖" "罰"])
	(28 ; l
	 (13 (16 . "ヴ")) ; r u
	 (16 . "ぅ") ; u
	 (17 . "ぃ") ; i
	 (25 . "ぇ") ; h
	 (26 . "ぉ") ; j
	 (27 . "ぁ")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "猿" "没" "披" "肪" "壌"
	     nil nil nil nil nil   "搬" "祉" "瞬" "宵" "棺"
	     nil nil nil nil nil   "狐" "濫" "穣" "痕" "凪"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "弓" "烈" "伏" "鯨" "凄"
	     nil nil nil nil nil   "紋" "潜" "臓" "駆" "憾"
	     nil nil nil nil nil   "暢" "匡" "舌" "亮" "諦"]))

    (14 ; t
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "恐" "紳" "朗" "既" "憩"
	     nil nil nil nil nil   "層" "留" "亜" "伴" "狂"
	     nil nil nil nil nil   "侍" "笛" "謹" "凶" "巌"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "為" "熊" "批" "祥" "慣"
	     nil nil nil nil nil   "繁" "願" "貿" "逆" "掛"
	     nil nil nil nil nil   "粗" "慰" "獲" "僕" "臥"])
	(28 ; l
	 (14 ; t
	  (16 . "ぷ") ; u
	  (17 . "ぴ") ; i
	  (25 . "ぺ") ; h
	  (26 . "ぽ") ; j
	  (27 . "ぱ")) ; k
	 (16 . "ぶ") ; u
	 (17 . "び") ; i
	 (25 . "べ") ; h
	 (26 . "ぼ") ; j
	 (27 . "ば")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "析" "裂" "舶" "括" "疫"
	     nil nil nil nil nil   "迷" "琴" "縫" "雷" "轄"
	     nil nil nil nil nil   "脹" "喬" "姻" "諒" "舘"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "匂" "賓" "筈" "寡" "癌"
	     nil nil nil nil nil   "吐" "乏" "准" "姓" "只"
	     nil nil nil nil nil   "冥" "煎" "柏" "牌" "贋"]))

    ;;
    (15 ; y
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "玄" "氷" "植" "枝" "暗"	nil nil nil nil nil
	     "稚" "陶" "群" "否" "荷"	nil nil nil nil nil
	     "橘" "擬" "駄" "鎮" "膚"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "函" "殊" "弘" "巣" "摩"	nil nil nil nil nil
	     "淳" "慮" "紀" "輪" "老"	nil nil nil nil nil
	     "迅" "偉" "垣" "遂" "炉"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "歪" "謁" "隅" "扉" "膳"	nil nil nil nil nil
	     "嫡" "婿" "巴" "肺" "敢"	nil nil nil nil nil
	     "鋸" "峨" "洲" "萎" "鴎"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "股" "裸" "棋" "漢" "紫"	nil nil nil nil nil
	     "緯" "俗" "鐘" "鷹" "簿"	nil nil nil nil nil
	     "芹" "伶" "爺" "矯" "蔦"	nil nil nil nil nil]))

    (16 ; u
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "鑑" "茂" "去" "益" "訴"	nil nil nil nil nil
	     "賛" "暴" "苦" "察" "速"	nil nil nil nil nil
	     "綜" "姫" "煙" "慎" "昔"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "孝" "償" "延" "標" "旧"	nil nil nil nil nil
	     "阿" "乱" "賀" "純" "奈"	nil nil nil nil nil
	     "牲" "拓" "排" "潮" "驚"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "噂" "循" "往" "騰" "咲"	nil nil nil nil nil
	     "棟" "汗" "趣" "泣" "兆"	nil nil nil nil nil
	     "裟" "怜" "虞" "尭" "悌"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "薪" "坑" "圏" "幻" "芽"	nil nil nil nil nil
	     "崖" "妊" "忙" "訂" "欺"	nil nil nil nil nil
	     "雫" "鳳" "錘" "藍" "蘇"	nil nil nil nil nil]))

    (17 ; i
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "僚" "粉" "易" "鳥" "枚"	nil nil nil nil nil
	     "貨" "郡" "模" "希" "邸"	nil nil nil nil nil
	     "戯" "腸" "症" "拠" "栗"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "陣" "余" "彦" "押" "核"	nil nil nil nil nil
	     "刷" "房" "突" "糖" "干"	nil nil nil nil nil
	     "抑" "亭" "逃" "混" "雅"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "軸" "狭" "銃" "砲" "拒"	nil nil nil nil nil
	     "輩" "沈" "徴" "逮" "忍"	nil nil nil nil nil
	     "勃" "茎" "伺" "恨" "醜"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "顕" "遭" "衝" "妹" "搭"	nil nil nil nil nil
	     "幾" "芦" "桂" "弟" "棄"	nil nil nil nil nil
	     "怨" "磯" "軌" "繭" "呑"	nil nil nil nil nil]))

    (18 ; o
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "耳" "貯" "章" "呈" "巡"	nil nil nil nil nil
	     "鋭" "貴" "票" "背" "刺"	nil nil nil nil nil
	     "霞" "斬" "鼻" "滅" "詞"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "緩" "封" "樹" "梅" "了"	nil nil nil nil nil
	     "棒" "塗" "互" "竜" "夢"	nil nil nil nil nil
	     "弔" "涯" "孫" "征" "僧"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "喉" "嬉" "泡" "窟" "郭"	nil nil nil nil nil
	     "惣" "堕" "誉" "貌" "諾"	nil nil nil nil nil
	     "珊" "詣" "釘" "麺" "箔"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "倣" "賭" "岐" "縁" "蛇"	nil nil nil nil nil
	     "蹴" "湿" "磨" "柔" "腐"	nil nil nil nil nil
	     "肘" "麓" "呆" "笹" "甫"	nil nil nil nil nil]))

    (19 ; p
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "傍" "我" "超" "曹" "飼"	nil nil nil nil nil
	     "潤" "幼" "途" "沼" "束"	nil nil nil nil nil
	     "挿" "慨" "但" "疾" "懐"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "妥" "促" "鮮" "冬" "裕"	nil nil nil nil nil
	     "触" "措" "著" "絹" "複"	nil nil nil nil nil
	     "拐" "幣" "閑" "妨" "机"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "飴" "蛍" "錯" "帆" "撒"	nil nil nil nil nil
	     "梨" "飢" "款" "邪" "剖"	nil nil nil nil nil
	     "堆" "柑" "樽" "稜" "膝"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "痔" "朋" "蒼" "廿" "肢"	nil nil nil nil nil
	     "腎" "壱" "嘩" "壷" "糎"	nil nil nil nil nil
	     "俣" "蛋" "汲" "俄" "吠"	nil nil nil nil nil]))

    ;;
    (20 ; a
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "縄" "皆" "滝" "怪" "鶏"
	     nil nil nil nil nil   "翼" "折" "恋" "照" "旨"
	     nil nil nil nil nil   "唇" "漠" "禅" "鳩" "遵"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "納" "痛" "牛" "弥" "添"
	     nil nil nil nil nil   "倒" "維" "拡" "禁" "虫"
	     nil nil nil nil nil   "淑" "隻" "奪" "滋" "愚"])
	(28 (27 . "ゎ")) ; l k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "尉" "賜" "冠" "寂" "楠"
	     nil nil nil nil nil   "滑" "誰" "抱" "拾" "蓋"
	     nil nil nil nil nil   "豹" "牙" "弐" "洛" "祇"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "盾" "如" "袖" "巾" "渚"
	     nil nil nil nil nil   "騎" "猛" "弊" "衰" "墳"
	     nil nil nil nil nil   "柵" "耶" "梢" "叡" "樫"]))

    (21 ; s
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "探" "矢" "鹿" "焦" "譜"
	     nil nil nil nil nil   "占" "誌" "療" "銘" "凍"
	     nil nil nil nil nil   "嬢" "鎖" "郊" "薫" "墾"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "郷" "順" "織" "承" "却"
	     nil nil nil nil nil   "笑" "盤" "降" "底" "菜"
	     nil nil nil nil nil   "麦" "俳" "露" "壇" "倫"])
	(28 ; l
	 (16 . "ず") ; u
	 (17 . "じ") ; i
	 (25 . "ぜ") ; h
	 (26 . "ぞ") ; j
	 (27 . "ざ")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "軟" "犠" "粧" "霊" "濯"
	     nil nil nil nil nil   "詳" "戻" "更" "薦" "俵"
	     nil nil nil nil nil   "漏" "陥" "架" "馴" "亘"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "猫" "耕" "酵" "剰" "慌"
	     nil nil nil nil nil   "稿" "彰" "熟" "浄" "辛"
	     nil nil nil nil nil   "虹" "欣" "頒" "唾" "奄"]))

    (22 ; d
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "歓" "副" "固" "綿" "勉"
	     nil nil nil nil nil   "恵" "岸" "客" "漁" "脱"
	     nil nil nil nil nil   "網" "畑" "皮" "舟" "柱"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "奥" "周" "夏" "域" "幡"
	     nil nil nil nil nil   "婚" "幹" "供" "節" "異"
	     nil nil nil nil nil   "箱" "存" "乳" "亡" "控"])
	(28 ; l
	 (16 . "っ") ; u
	 (17 . "ぢ") ; i
	 (22 (16 . "づ")) ; d u
	 (25 . "で") ; h
	 (26 . "ど") ; j
	 (27 . "だ")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "辰" "隆" "頼" "桑" "悟"
	     nil nil nil nil nil   "駐" "礼" "倍" "兄" "厘"
	     nil nil nil nil nil   "摂" "宛" "雇" "后" "駈"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "漫" "緑" "迫" "芳" "曽"
	     nil nil nil nil nil   "刑" "揮" "影" "刻" "索"
	     nil nil nil nil nil   "椅" "偽" "升" "礁" "卯"]))

    (23 ; f
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "催" "幅" "堀" "則" "充"
	     nil nil nil nil nil   "均" "譲" "除" "桜" "唱"
	     nil nil nil nil nil   "虎" "浪" "鳴" "妙" "粋"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "又" "荒" "援" "君" "震"
	     nil nil nil nil nil   "絶" "舎" "圧" "焼" "努"
	     nil nil nil nil nil   "鴨" "菓" "看" "敵" "鉛"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "祖" "沿" "峰" "勲" "峡"
	     nil nil nil nil nil   "摘" "犬" "鎌" "陰" "筒"
	     nil nil nil nil nil   "煩" "迭" "窮" "洪" "臆"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "敬" "腕" "娘" "鍛" "洞"
	     nil nil nil nil nil   "宏" "傾" "欲" "誤" "廷"
	     nil nil nil nil nil   "尺" "廉" "刀" "雌" "靖"]))

    (24 ; g
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "需" "遅" "創" "踊" "緒"
	     nil nil nil nil nil   "列" "魚" "典" "宣" "毒"
	     nil nil nil nil nil   "惜" "黙" "微" "祈" "遷"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "筆" "錠" "盟" "幕" "灯"
	     nil nil nil nil nil   "障" "瀬" "繊" "移" "困"
	     nil nil nil nil nil   "慢" "弦" "腰" "凡" "懲"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "彫" "魔" "祝" "携" "斐"
	     nil nil nil nil nil   "濃" "暖" "珠" "杯" "叫"
	     nil nil nil nil nil   "桶" "累" "晶" "宰" "癒"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "唐" "脇" "貝" "怠" "翁"
	     nil nil nil nil nil   "笠" "銅" "胸" "脅" "濁"
	     nil nil nil nil nil   "芙" "溝" "蹟" "諏" "迦"]))

    ;;
    (25 ; h
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "匹" "皇" "湖" "仙" "描"	nil nil nil nil nil
	     "掲" "薄" "湯" "呼" "絡"	nil nil nil nil nil
	     "燈" "哀" "畳" "桃" "綾"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "寝" "就" "須" "操" "招"	nil nil nil nil nil
	     "兼" "飯" "責" "依" "盗"	nil nil nil nil nil
	     "胎" "梶" "脂" "鏡" "抽"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "斤" "嶺" "縦" "涼" "奮"	nil nil nil nil nil
	     "旺" "隔" "寛" "眠" "奨"	nil nil nil nil nil
	     "嵯" "勾" "叱" "昆" "蕗"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "斜" "炎" "淡" "佳" "挑"	nil nil nil nil nil
	     "苑" "揚" "還" "堅" "悩"	nil nil nil nil nil
	     "蓉" "拷" "狼" "愁" "嘱"	nil nil nil nil nil]))

    (26 ; j
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "旬" "飾" "採" "離" "功"	nil nil nil nil nil
	     "岳" "秒" "闘" "疑" "仏"	nil nil nil nil nil
	     "仰" "怒" "項" "吾" "伎"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "扱" "父" "布" "温" "血"	nil nil nil nil nil
	     "因" "象" "康" "紡" "識"	nil nil nil nil nil
	     "尊" "至" "儀" "飲" "賦"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "憶" "卵" "騒" "肌" "蓄"	nil nil nil nil nil
	     "蝶" "奉" "暮" "亀" "絵"	nil nil nil nil nil
	     "遥" "蛮" "皿" "矛" "冗"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "枕" "庶" "誇" "裳" "粛"	nil nil nil nil nil
	     "胴" "卓" "悲" "献" "童"	nil nil nil nil nil
	     "朔" "哉" "虐" "殴" "惰"	nil nil nil nil nil]))

    (27 ; k
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "潟" "博" "境" "借" "夕"	nil nil nil nil nil
	     "寿" "攻" "邦" "衆" "姿"	nil nil nil nil nil
	     "較" "染" "硝" "臣" "疲"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "豪" "謡" "舞" "密" "遠"	nil nil nil nil nil
	     "筋" "系" "針" "徒" "喜"	nil nil nil nil nil
	     "零" "宙" "喫" "欧" "致"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "嘉" "仮" "辞" "牧" "勧"	nil nil nil nil nil
	     "床" "浮" "片" "弱" "勇"	nil nil nil nil nil
	     "劾" "憤" "励" "惨" "隈"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "誕" "序" "被" "縮" "吹"	nil nil nil nil nil
	     "菅" "載" "危" "乞" "替"	nil nil nil nil nil
	     "茜" "俺" "涙" "握" "蘭"	nil nil nil nil nil]))

    (28 ; l
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "扇" "届" "併" "執" "菊"	nil nil nil nil nil
	     "獄" "像" "酸" "札" "丹"	nil nil nil nil nil
	     "仇" "於" "暑" "泥" "悦"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "眼" "災" "破" "舗" "豆"	nil nil nil nil nil
	     "湾" "傷" "締" "換" "犯"	nil nil nil nil nil
	     "赴" "糧" "謝" "穴" "姉"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "妓" "屈" "壁" "炊" "蓮"	nil nil nil nil nil
	     "庸" "憂" "律" "襲" "釣"	nil nil nil nil nil
	     "鹸" "妖" "衷" "襟" "覇"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "猟" "帽" "貫" "嵐" "貞"	nil nil nil nil nil
	     "尿" "懇" "躍" "欠" "栃"	nil nil nil nil nil
	     "謎" "儒" "訟" "租" "殉"	nil nil nil nil nil]))

    (29 ; ;
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "尻" "敏" "卸" "駒" "那"	nil nil nil nil nil
	     "冒" "輝" "柄" "甲" "避"	nil nil nil nil nil
	     "該" "碑" "斉" "栽" "偏"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "窯" "葬" "忠" "遺" "礎"	nil nil nil nil nil
	     "預" "柳" "債" "汽" "塩"	nil nil nil nil nil
	     "漸" "酢" "硬" "豚" "丈"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "馨" "賠" "酔" "唆" "沸"	nil nil nil nil nil
	     "拭" "膨" "紛" "寒" "瓦"	nil nil nil nil nil
	     "詮" "隙" "嚇" "寅" "堺"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "萩" "爵" "劣" "噴" "盲"	nil nil nil nil nil
	     "渥" "瞳" "獣" "溜" "浸"	nil nil nil nil nil
	     "匙" "挨" "肇" "斡" "廓"	nil nil nil nil nil]))
    ;;
    (30 ; z
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "亥" "髄" "宜" "畝" "莫"
	     nil nil nil nil nil   "勅" "霜" "槽" "粘" "鞄"
	     nil nil nil nil nil   "兜" "倶" "尖" "茅" "漕"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "培" "貢" "醸" "丞" "詔"
	     nil nil nil nil nil   "雀" "勘" "距" "磁" "陪"
	     nil nil nil nil nil   "芥" "佑" "鍾" "拶" "樋"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "砦" "杜" "桁" "髭" "蒙"
	     nil nil nil nil nil   "梱" "稔" "汐" "凱" "潅"
	     nil nil nil nil nil   "遡" "茸" "悶" "蕪" "袴"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "舷" "瞭" "槙" "楯" "椎"
	     nil nil nil nil nil   "串" "禎" "渦" "琶" "註"
	     nil nil nil nil nil   "堰" "屠" "酋" "醒" "鞭"]))

    (31 ; x
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "朽" "骸" "誓" "倹" "窪"
	     nil nil nil nil nil   "酷" "隠" "賢" "恥" "逝"
	     nil nil nil nil nil   "蟻" "羨" "爪" "脊" "稀"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "堪" "肝" "鋳" "瓶" "寧"
	     nil nil nil nil nil   "墜" "翌" "坊" "召" "肖"
	     nil nil nil nil nil   "把" "欽" "剃" "釧" "槍"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "闇" "綴" "娯" "郁" "箸"
	     nil nil nil nil nil   "閲" "附" "径" "喚" "俸"
	     nil nil nil nil nil   "峯" "雁" "閃" "腿" "壬"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "柿" "塔" "瑛" "粟" "鼠"
	     nil nil nil nil nil   "紗" "沙" "僅" "亦" "汰"
	     nil nil nil nil nil   "蕉" "蓬" "噌" "苅" "屑"]))

    (32 ; c
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "懸" "掘" "硫" "傑" "垂"
	     nil nil nil nil nil   "閉" "旗" "廃" "剣" "覆"
	     nil nil nil nil nil   "紐" "擦" "酪" "慧" "糊"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "浩" "俊" "雲" "啓" "斗"
	     nil nil nil nil nil   "銭" "里" "渉" "徹" "艦"
	     nil nil nil nil nil   "謄" "透" "麗" "耗" "凹"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "鍵" "購" "伯" "逢" "遮"
	     nil nil nil nil nil   "釜" "孤" "称" "溶" "腫"
	     nil nil nil nil nil   "榊" "妄" "勺" "湘" "蛾"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "鈍" "峠" "貼" "缶" "栓"
	     nil nil nil nil nil   "虚" "撤" "頁" "睡" "稽"
	     nil nil nil nil nil   "厭" "遼" "塑" "淀" "狽"]))

    (33 ; v
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "臭" "彩" "謙" "錬" "叔"
	     nil nil nil nil nil   "煮" "泊" "甘" "澄" "甚"
	     nil nil nil nil nil   "朕" "挟" "昂" "瑠" "粕"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "液" "契" "也" "恒" "媒"
	     nil nil nil nil nil   "帳" "掃" "簡" "汚" "概"
	     nil nil nil nil nil   "漆" "囚" "拳" "侮" "祐"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "渓" "裾" "遍" "緋" "舵"
	     nil nil nil nil nil   "凝" "厄" "憎" "旭" "庵"
	     nil nil nil nil nil   "撫" "芭" "魁" "卿" "曾"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "醤" "剛" "播" "嘘" "冨"
	     nil nil nil nil nil   "穫" "徐" "李" "辱" "痘"
	     nil nil nil nil nil   "采" "楚" "牢" "槻" "竿"]))

    (34 ; b
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "眉" "泌" "刈" "斥" "汝"
	     nil nil nil nil nil   "糾" "嗣" "朱" "崇" "坦"
	     nil nil nil nil nil   "斧" "椿" "盃" "禿" "逗"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "奴" "恩" "抵" "尽" "詠"
	     nil nil nil nil nil   "桐" "唯" "到" "喪" "疎"
	     nil nil nil nil nil   "楓" "紘" "帥" "幌" "燐"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "抄" "藩" "享" "貰" "嶋"
	     nil nil nil nil nil   "灰" "酬" "狩" "隷" "凸"
	     nil nil nil nil nil   "慾" "埼" "蜜" "鎚" "汀"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "柴" "罷" "毅" "榎" "乎"
	     nil nil nil nil nil   "晋" "梓" "婆" "麿" "牟"
	     nil nil nil nil nil   "膿" "伽" "尤" "耽" "饗"]))

    ;;
    (35 ; n
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "臼" "洩" "猶" "鼓" "枢"	nil nil nil nil nil
	     "猪" "吟" "艇" "暁" "賊"	nil nil nil nil nil
	     "塞" "憧" "佃" "曳" "蟹"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "叉" "擁" "撮" "陛" "其"	nil nil nil nil nil
	     "偶" "阻" "泳" "籍" "撲"	nil nil nil nil nil
	     "卦" "玩" "絞" "冴" "塀"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "勿" "頓" "亨" "彬" "畏"	nil nil nil nil nil
	     "罫" "岬" "衿" "絢" "瑞"	nil nil nil nil nil
	     "鵬" "竪" "溢" "腺" "鷲"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "妾" "吊" "燥" "蚕" "繍"	nil nil nil nil nil
	     "酉" "湧" "詐" "脈" "狙"	nil nil nil nil nil
	     "潰" "歎" "迂" "鮭" "杖"	nil nil nil nil nil]))

    (36 ; m
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "跳" "拘" "遣" "欄" "薩"	nil nil nil nil nil
	     "篭" "壮" "乾" "秩" "頂"	nil nil nil nil nil
	     "此" "玲" "旦" "曲" "挺"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "掌" "捨" "昌" "訓" "穂"	nil nil nil nil nil
	     "刃" "呉" "包" "息" "踏"	nil nil nil nil nil
	     "羅" "伐" "痢" "翻" "陵"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "阜" "粍" "践" "狗" "赦"	nil nil nil nil nil
	     "屯" "膜" "苗" "棚" "艶"	nil nil nil nil nil
	     "桓" "凧" "琵" "蛙" "糞"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "碇" "頻" "癖" "鈎" "鯉"	nil nil nil nil nil
	     "篠" "鮎" "朴" "縛" "戴"	nil nil nil nil nil
	     "硯" "檀" "餅" "蝿" "屍"	nil nil nil nil nil]))

    (37 ; ,
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "慕" "乙" "霧" "肩" "顧"	nil nil nil nil nil
	     "尋" "随" "緊" "仁" "吸"	nil nil nil nil nil
	     "詫" "侯" "砕" "伍" "肯"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "呂" "殿" "鶴" "燃" "診"	nil nil nil nil nil
	     "扶" "囲" "端" "季" "荻"	nil nil nil nil nil
	     "殻" "喧" "壊" "拍" "畔"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "聡" "稼" "暇" "恭" "崩"	nil nil nil nil nil
	     "喝" "坐" "諭" "殖" "粒"	nil nil nil nil nil
	     "讃" "茨" "藻" "禄" "姦"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "酌" "蚊" "腹" "衡" "拙"	nil nil nil nil nil
	     "盆" "閥" "据" "繰" "碁"	nil nil nil nil nil
	     "宍" "秦" "塾" "孟" "蔑"	nil nil nil nil nil]))

    (38 ; .
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "蜂" "悔" "紺" "悠" "窃"	nil nil nil nil nil
	     "鯛" "禍" "宴" "眺" "逐"	nil nil nil nil nil
	     "覗" "桧" "韓" "頚" "燭"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "馳" "虜" "珍" "暦" "蒸"	nil nil nil nil nil
	     "吏" "穀" "肥" "斎" "陳"	nil nil nil nil nil
	     "賎" "芯" "頑" "矩" "鉢"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "烹" "苔" "渇" "惇" "汎"	nil nil nil nil nil
	     "諺" "抹" "淋" "丑" "峻"	nil nil nil nil nil
	     "兎" "弛" "櫛" "迄" "姐"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "牡" "允" "云" "箇" "釦"	nil nil nil nil nil
	     "萌" "蔭" "叙" "妃" "愉"	nil nil nil nil nil
	     "諜" "苛" "帖" "戚" "薮"	nil nil nil nil nil]))

    (39 ; /
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "曙" "晃" "逓" "弧" "卑"	nil nil nil nil nil
	     "璃" "塊" "逸" "慈" "枯"	nil nil nil nil nil
	     "恕" "斑" "蝕" "畠" "狸"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "褐" "堤" "旋" "偵" "篤"	nil nil nil nil nil
	     "或" "謀" "己" "巳" "巧"	nil nil nil nil nil
	     "醍" "銚" "且" "敦" "咽"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "縞" "娼" "琢" "枠" "牝"	nil nil nil nil nil
	     "烏" "龍" "餓" "爾" "褒"	nil nil nil nil nil
	     "叶" "葛" "椀" "碍" "劫"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "獅" "溺" "畿" "塵" "瓜"	nil nil nil nil nil
	     "仔" "倭" "駿" "匁" "姑"	nil nil nil nil nil
	     "脆" "箕" "剥" "醐" "凌"	nil nil nil nil nil]))))

(setq tcode-special-commands-alist
  '(((0 0) . (lambda () (tcode-show-tables nil nil))) ; 11 : LL表の表示
    ((0 9) . (lambda () (tcode-show-tables nil t))) ; 10 : LR表の表示
    ((9 0) . (lambda () (tcode-show-tables t nil))) ; 01 : RL表の表示
    ((9 9) . (lambda () (tcode-show-tables t t))) ; 00 : RR表の表示
    ((1 1) . tcode-start-jiscode)	; 22 : JIS コード表入力
    ((2 2) . tcode-toggle-alnum-mode) ; 33 : 1-2バイト切り換え
    ((2 1) . tcode-switch-variable) ; 32 : 句読点のトグル
    ((3 3) . (lambda ()
	       (tcode-display-stroke-sequence tcode-last-help-char-list)))
					; 44 : ヘルプ
    ((4 4) . (lambda () (tcode-query-stroke (point))))
					; 55 : ヘルプ
    ((6 6) . tcode-bushu-begin-alternate-conversion)
					; 77 : postfix 部首変換
    ((7 7) . (lambda () (tcode-transpose-strokes nil)))
					; 88 : transpose-strokes
    ((8 8) . tcode-clear)
					; 99 : 部首合成変換・交ぜ書き変換などの
					; キャンセル
    ((20 28 20) . tcode-bushu-begin-conversion))) ; ala : 部首合成変換の開始


(setq tcode-mode-help-string "\
TUTコードモード中のキー操作は次のとおり。
   ala : 部首合成変換モードに入る。alaを打ち続けると再帰的に部首合成変換を
	行うことができる。
   alj : 交ぜ書き変換を行う(see variable `tcode-use-prefix-mazegaki')。
   00, 01, 10, 11 : TUTコードの2ストロークのストローク表を表示する。
			(0が「右」、1が「左」を意味している)
   22 : JIS コード一覧表による入力。
   32 : 、。と, . を切り替える。(see variable `tcode-switch-table-list')。
   33 : TUTコード表にある英数字・記号の文字コードの1バイト・2バイト切り替え。
   44 : 直前に表示した打ち方を再表示する。
   55 : ポイント位置にある文字の打ち方を表示する。
   58 : 活用語を優先して交ぜ書き変換を行う。
   77 : ポイント前にある2文字で部首合成変換を行う。
   88 : ポイント位置にある文字を逆ストローク化する(例: 年->の)。
	行末ではポイントの直前の文字を変換する。
   99 : 交ぜ書き変換モードや部首変換モードにいた時に、
	それらを全部キャンセルする。また、ヘルプを消す。
   [1-4]8, [2-5]9: 文字数を指定して交ぜ書き変換を行う。
   \\[toggle-input-method] : TUTコードモードを抜ける。

初めて起動された時には，`tcode-ready-hook' を実行する。
また、起動される度に`tcode-toggle-hook'を実行する。")

(defun tcode-make-special-for-tut (seq table)
  "TABLE を SEQ に基づき `tcode-special-commands-alist' 用に変換する。"
  (cond ((consp table)
	 (if (integerp (car table))
	     (tcode-make-special-for-tut (append seq (car table)) (cdr table))
	   (let (list)
	     (while table
	       (let* ((elm (car table))
		      (ret (tcode-make-special-for-tut
			    (append seq (list (car elm)))
			    (cdr elm))))
		 (and ret
		      (setq list (append ret list)))
		 (setq table (cdr table))))
	     list)))
	((vectorp table)
	 (list (cons seq table)))
	((stringp table)
	 (list (cons seq table)))))

(setq tcode-special-commands-alist
      (nconc (tcode-make-special-for-tut nil tut-over-2-strokes-table)
	     tcode-special-commands-alist
	     '(((20 28 26) . tcode-mazegaki-begin-conversion)
				     ; alj: 交ぜ書き変換
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
	       ((4 8) . (lambda () (tcode-mazegaki-convert 5 t))))))

(setq tcode-stroke-file-name (concat tcode-data-directory "tutcode.st"))

(setq eelll-text "EELLLTXT.tut")

;;; tutc-tbl.el ends here
