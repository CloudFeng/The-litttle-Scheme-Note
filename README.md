# The-litttle-Scheme-Note
This repository contains the book of the little scheme source code and some notes. All of the codes can be loaded into scheme compiler to run. I use this on line [scheme compiler](https://repl.it/).

## 缘起
看这本书的缘由，说起来不好意思的。主要看scip看着头痛，看到第二章的时候，做里面的习题做着头疼脑热的，看看能否对scheme语言有一个简要的介绍就在网上看到此书，有人评论此书可以做为scip的基础书，我读后感受是最好在读完scip的第一章和第二章中关于list的概念之后就可以阅读了。毕竟这本书一上来就是各种lambda表达式、以及scheme中内置的函数。尽管书的前言说，此书适合无任何编程经验的学习。觉得自己在校学习果断和国外的差距很大。

> The Little Schemer is based on lecture notes from two-week "quickie"
> introduction 
to Scheme for Students with no previous programming experience.

## 概述
本来想花一周的时间将此书看完，不厚也就200多页。但是期间加班比较严重，后面自己玩游戏也比较多，导致拖到上周日才看完。此书以对话的形式介绍schemer，书中主要是讲解程序中递归的概念。在书的前言就作者就直接说：

> The goal of this book is to teach the reader to think recursively. 


下面简单说明一下这本书各章节内容：

- 第一章、第二章、第三章，简要介绍scheme中的基础函数比如，
连接函数cons、求第一个值函数car、后续值函数cdr，以及判断一个元素是否为原子。

- 第四章，数值计算，加减乘除等。

- 第五章，主要将前面两章的函数进行推广，以*做通配符。
- 第六章，以value函数的定义讲解求值的副作用。
- 第七章，定义集合的概念以及集合之间的操作。
- 第八章，将一些重复性的代码进行抽象，比如在第六章中的value函数的一个重构。

- 第九章，看着头痛，特别是在构造length函数的过程。虽然看了两遍还没有理解透，后面还需要重新读一遍才行。
此章主要以构建length函数来讲解图灵不动点问题和Y combinator。

- 第十章，如何一步一步构造出一个简单的scheme编译器。

## Scheme编程经验
作者在自问自答的方式中介绍5条原则和10条告诫。这些都是通过一个个实例进行论证说明的。这些经验性的概括十分简短，却十分有用。
下面看看1条原则和1条告诫吧。更多可以访问[此地](https://github.com/CloudFeng/The-litttle-Scheme-Note/blob/master/5RulesAnd10Comandments.md).

### The Law of Car
The primitive `car` is defined only for non-empty lists.

### The First Commandment
When recurring on a list of atoms, lat, ask two questions about it: `(null?
lat)` and `else`.
When recurring on a number, n, ask two questions about it:`(zero? n)` and
`else`;
When recurring on a list of S-expressions, l, ask three questions about
it:`(null? l)`,
 `(atom? (car l))`, and `else`.


## 如何阅读此书
对于这本小书，如何阅读，作者在阅读者指导中已经说得很明确。其实，给出的建议也适合很多其他的读本,下面摘抄如下，分享给大家。:)

> Do not rush through this book. Read carefully; valuable hints are scattered
> throughout the text. 
**Do not read the book is fewer than three setting.** Read systematically.
