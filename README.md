Assembler
====
「神戸市立工業高等専門学校 電子計算機部 計算機製作プロジェクト」に於て製作為れし計算機群之換符系に候。

## 依存
*CmdArgs

## 翻訳
    cabal install cmdargs
    ghc Main.hs -o asm

## 実行例
    asm -m=TD4 -d=asm Flash-LED.asm
    asm -machine=Type1 -direction=DisASM -verbose=ON Fibonacci.bin
