# multipart-markdown

## 概要

multipart-markdownは複数のmarkdown文書と画像リソースを単一のmarkdown文書（以降multipart-markdown形式と呼びます）に変換するための実験的なコンバータです。



## 使用方法

QuickLispが導入されていることが前提です。

```sh
cd ~/quicklisp/local-projects
git clone https://github.com/singy15/multipart-markdown.git
cd multipart-markdown/sample
sbcl
(ql:quickload "multipart-markdown")

# unpack
# multipart-markdownを複数ファイルのmarkdown文書および画像リソースに展開します
(multipart-markdown:unpack "./sample.md")

# pack
# ルートのmarkdown文書を選択し、リンクされているmarkdown文書と画像をすべて含むmultipart-markdownを書き出します
(multipart-markdown:pack "./sample.md" "./sample-index.md")

```

