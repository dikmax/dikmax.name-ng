---
title: "Ошибка в Google Closure Compiler"
date: "2012-06-30T15:28:56+03:00"
published: true
tags: [closure compiler, javascript, программирование, работа]
---

Обнаружил досадную ошибку в последнем [Google Closure Compiler](https://developers.google.com/closure/compiler/)
(версия от 30 апреля). Почему-то при анализе кода он считает все параметры функции опциональными. А потому выдает
замечания внутри функции, если пытаешься использовать параметр без проверки на undefined. И наоборот, не пишет ошибок,
когда передаешь меньше параметров в функцию, чем указано в ее определении.

Пришлось вернуться на [предыдущую версию](http://closure-compiler.googlecode.com/files/compiler-20120305.tar.gz).
