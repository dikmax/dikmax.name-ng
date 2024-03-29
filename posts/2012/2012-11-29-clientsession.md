---
title: "Хранение сессии на клиенте"
date: "2012-11-29T09:43:46+03:00"
published: true
tags: [haskell, snap, безопасность, блог, вопрос, программирование]
thread: 949318485
---

Как вы, должно быть, знаете, хранить пользовательские данные на клиентской стороне плохо: их ведь может подделать
злостный хакер. Но сегодня я вам расскажу, как можно безопасно хранить сессионные данные, не волнуясь за их сохранность.
Расскажу на примере [Snap Framework](http://snapframework.com/), на котором и написан этот блог.

Суть метода\ --- шифрование (кто бы мог подумать). Snap для своих сессий использует библиотеку
[Web.ClientSession](https://github.com/yesodweb/clientsession), а поэтому что и как обрабатывается можно смело
подсмотреть в [документации](http://hackage.haskell.org/package/clientsession):

* Шифруем данные куки с помощью AES в режиме CTR. Это позволяет хранить важные данные на стороне клиента, не беспокоясь
  о том, что их кто-нибудь украдет.
* Подписываем зашифрованные данные с помощью Skein-MAC-512-256. Кроме определения потенциальных ошибок в хранении и
  передачи данных (целостность), MAC предотвращает изменения данных и подтверждает, что они действительно были
  сгенерированны этим сервером (подлинность).
* Кодируем результат с помощью Base64, таким образом избавляемся от различных непечатных символов и отдаем браузеру
  простую строку.

Изучая всякие википедии о [AES](http://en.wikipedia.org/wiki/Advanced_Encryption_Standard), я не особо понял, какие
преимущества нам даст режим CTR в плане безопасности, но судя по описанию коммитов к библиотеке в этом режиме результат
получается чуть более компактным и сам алгоритм чуть проще реализовывается. Про любой другой язык я могу сказать, что
почти наверняка есть библиотека, которая уже реализует этот алгоритм за вас, так что сильно думать не придется.

[Skein](http://en.wikipedia.org/wiki/Skein_(hash_function))\ --- это просто хороший алгоритм хеширования. К сожалению, он
не победил в [конкуре на замену SHA-2](http://en.wikipedia.org/wiki/NIST_hash_function_competition), и название SHA-3
получил другой алгоритм. Поэтому самое время заменить Skein на новоявленный SHA-3\ ---
[Keccak](http://en.wikipedia.org/wiki/SHA-3).

Ну и напоследок несколько мыслей. Во-первых, этот метод хранения несколько надежнее традиционных, т.к. никакой
форс-мажор на сервере не позволит данным потеряться. А если что-нибудь произойдет со стороны клиента и кука исчезнет,
то на сервере у нас не останется никаких мусорных данных об уже не существующей сессии. Во-вторых, таким образом
не получится хранить большие объемы данных, т.к. размер куки ограничен, да и передача каждый раз пары десятков-сотен
килобайт вполне может увеличить нагрузку на сервер. В-третьих, увести сессию можно точно так же, как и обыкновенную,
поэтому увеличения безопасности тут не будет.

В общем, идея имеет право на жизнь. Особенно на небольших, как в моем случае, объемах хранимых данных. Лично я храню
только одно поле: является ли текущий пользователь админом или нет.

Хозяйке на заметку: чтобы вашу сессию нельзя было увести с помощью JavaScript, добавьте в заголовок `Set-Cookie`
параметр `HttpOnly`. Куки с таким параметром не появляются в `document.cookie`.

Осталась одна загадка, связанная с моим сайтом. Кука `_session` передается как сессионная и должна удаляться
с закрытием браузера. Так и происходит в Opera, но не Google Chrome и Firefox. Может есть какая-то стандартизированная
или неофициальная фича, которая не дает сессионным кукам удалиться?
