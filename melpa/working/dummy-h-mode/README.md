# dummy-h-mode.el --- switch major mode to c-/c++-/objc-mode on .h file

`dummy-h-mode` は C、C++、Objective-C のヘッダファイル (.h) のためのメジャーモードです。
`c-mode`、`c++-mode`、`objc-mode` の中から適したメジャーモードを判別し、そのメジャーモードに切り替えます。
その判別手順は以下のとおりです。

1. 対応するソースファイル (.c、.cc など) が存在するかを調べる。
2. ファイル中のキーワードを検索する。
3. ディレクトリ内のすべてのファイルの拡張子を調べる。

# 使い方

以下の 2 行を設定ファイルに追加してください。

    (add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
    (autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)

# カスタマイズ

デフォルトのメジャーモード (`c-mode` か `c++-mode` か `objc-mode`) を設定する。
デフォルト: `c-mode`。

    (add-hook 'dummy-h-mode-hook
              (lambda ()
                (setq dummy-h-mode-default-major-mode 'c++-mode)))

C のキーワードとその最小出現カウント数を追加設定する。

    (add-hook 'dummy-h-mode-hook
              (lambda ()
                (add-to-list 'dummy-h-mode-c-keywords
                             '("\*[ \t]*restrict" . 3))))

このカスタマイズ例では、`\*[ \t]*restrict` がファイル中に 3 回以上出現すると `c-mode` と判定されます。
C++ 向けの `dummy-h-mode-cc-keywords`、および Objective-C 向けの `dummy-h-mode-objc-keywords` が同様に設定可能です。

大きいサイズのファイルに対して、キーワード検索による判別に用いる文字数を設定する。
デフォルト: 30000 文字 (＝約 1000 行)。

    (add-hook 'dummy-h-mode-hook
              (lambda ()
                (setq dummy-h-mode-search-limit 60000)))

# バージョン

## 1.0.1 on 16 Sug. 2014

リファクタリング。

## 1.0.0 on 3 Mar. 2012

ファーストコミット。

# 既知の問題

## Lisp nesting exceeds `max-lisp-eval-depth'

ディレクトリ内のファイル数が多い (数百個以上) とき、無限再帰と判定されてエラーになることがあります。
以下のように `max-lisp-eval-depth` を大きめの数字に設定することで対処できます。

    (setq max-lisp-eval-depth 1000)
