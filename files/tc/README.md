tc:  Japanese direct input method environment for emacs
========================================================

This repository provides tc(T-code input environment for emacs).  
tc enables you to input Japanese characters with T-code or TUT-code.  

T-code is a Japanese input method that does NOT use Kana-to-Kanji conversion.  
You can input Kanji characters directly in the same way of inputting Hiragana.  
TUT-code is one of the T-code alternatives.

## install方法

init.elに

	(require 'package)
	(add-to-list 'package-archives
	  '("melpa" . "http://melpa.milkbox.net/packages/") t)
	(package-initialize)

を追加します。
上記のelispをevalした後で

	M-x package-install
	tc

を実行します。package-installが終わったら

	M-x tcode-install

を実行してください。
tcの設定file作成場所についてなどが問われますがすべて
defaultで問題ありません。

最後にinit.elに

`(require 'tc-setup)`

の1行を追加し、emacsをrestartしてください。
restart後は
`M-x toggle-input-method`
を実行し、用いるinput-methodにT-codeを選択してください。

### Ubuntu Linuxでの注意事項

Ubuntu Linuxでuim-tcodeをご利用の方は /etc/emacs/site-start.d/50t-code.el の内容をすべてコメントアウトしてください。


## tcの利用方法

https://github.com/kanchoku/tc/blob/master/doc/manual.pdf?raw=true のpdfをご覧ください
