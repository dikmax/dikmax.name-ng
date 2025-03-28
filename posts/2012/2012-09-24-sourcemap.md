---
title: "Source map"
date: "2012-09-24T22:52:42+03:00"
published: true
tags: [closure compiler, css, google chrome, javascript, программирование]
thread: 857726543
---

Итак, у вас очень много JavaScript-кода. А значит, вы его сжимаете перед тем, как отдавать клиенту. Наверняка даже
используете [YUI Compressor](http://yuilibrary.com/projects/yuicompressor/) или
[Google Closure Compiler](https://developers.google.com/closure/compiler/). А может, вы пишете свой client-side код на
чем-нибудь модном: на [Dart](http://dartlang.org/) или [CoffeeScript](http://coffeescript.org/)? В любом случае вы
наверняка сталкивались с проблемой отладки и поиска ошибки в браузере. Попробуй угадать, почему в 3-й строчке на 100501
позиции значение переменной  `aAz` вдруг стало `undefined`. А ведь до компиляции скрипта всё работало как надо! Я в
таких случаях начинаю добавлять в код много инструкций `console.log`, чтобы хоть как-то проследить процесс
выполнения кода.

Ну что ж, у меня для вас хорошие новости. В Google знают о ваших проблемах и даже придумали решение\ --- source map.
Идея проста: компилятор должен создавать специальный файл с информацией о соответствии скомпилированного кода не
скомпилированному, а браузер должен брать информацию из этого файла и показывать красоту вместо абракадабры.

Расскажу, какие шаги мне пришлось проделать, чтобы source map заработал в этом блоге. Сразу предупреждаю, что на
рабочем сайте вы его не найдете, он существует только на моей локальной машине, но вы можете попробовать собрать всё
[из исходников](https://github.com/dikmax/haskell-blog) сами.

1. Включаем генерацию source map в Google Closure Compiler. Для этого используется опция компилятора
  `--create_source_map=./script.js.map`.
2. Файл получили. Но в нем скорее всего прописаны неправильные пути к исходным файлам, особенно если ваш корень сервера
  не совпадает с корнем проекта. Придется их поправить, например, вот такой unix-командой:
  `sed -i 's/"static\//"\//g' static/js/script.js.map`.
3. Чтобы браузер знал о наличии файла с картой, нужно добавить комментарий в конец скомпилированного js-фала:
  `echo "//@ sourceMappingURL=script.js.map" >> static/js/script.testing.js`.
4. Ну и последний этап, нужно включить поддержку source map в Google Chrome. Для этого открываем
  Developer Tools (Ctrl+Shift+I), нажимаем на кнопку настроек в правом нижнем углу и ставим галочку напротив
  “Enable source maps”.

Вот и всё, теперь можно отлаживать свои исходники даже несмотря на то, что у нас в браузер загружен упакованный файл.

А теперь ложка дегтя. Она одна, но зато большая: поддержка. Как это обычно бывает с новыми технологиями, не все
успевают или не все хотят реализовывать всякие экспериментальные поделки. Поэтому мы будем довольствоваться только
одним браузером\ --- Google Chrome. Ребята из Mozilla
[тоже трудятся](https://wiki.mozilla.org/DevTools/Features/SourceMap) над поддержкой source maps, но работающего
результата у них пока нет. А вот запрос “ie source maps” ожидаемо выдает 0 релевантных результатов.

Та же беда с Source Maps и в языках и компиляторах. Я нашел поддержку только в Google Closure Compiler и Dart, а так же
зачатки в CoffeeScript. Если знаете еще кого-нибудь, кто поддерживает\ --- добро пожаловать в комментарии.

Скажу, что спецификация так же предусматривает source map и для CSS файлов, но я пока не тестировал эту возможность. 

Ну и напоследок пара ссылок:
[на спецификацию](https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k/edit?hl=en_US&pli=1&pli=1)
и [на обзор](http://www.html5rocks.com/en/tutorials/developertools/sourcemaps/).
