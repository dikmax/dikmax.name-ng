---
title: "Sencha Architect"
date: "2014-01-03T21:25:00+03:00"
published: true
tags: [extjs, ide, sencha architect, sencha touch, программирование]
thread: 2089959669
---

Случилось мне тут по\ долгу службы столкнуться с\ [Sencha Architect]. Если кто не\ в\ курсе\ --- это IDE,
разрабатываемая Sencha для облегчения работы с\ её\ же фреймворками [Sencha Touch] и\ [ExtJS][extjs]. Из\ заявленных
“killer features” в\ первую очередь выделяются визуальный редактор компонентов и\ интеллектуальная работа с\ кодом.

В\ памяти совсем юных бойцов айтишного фронта уже вероятно нет такой среды разработки, как [Borland Delphi][delphi],
хотя программисты постарше должны её\ помнить. Так вот, Sencha Architect весьма на\ неё похожа. Так\ же можно накликать
себе приличный интерфейс и\ навешать всяких обработчиков событий на\ всевозможные его элементы. Но\ на\ этом сходство
заканчивается.

![Sencha Architect](/images/screenshots/sencha-architect-3-design.png "Sencha Architect")

Delphi, в\ отличие от\ Sencha Architect, позволяла сделать два шага в\ сторону, открыть редактор кода и\ сделать всё
то\ же самое, только не\ прикасаясь к\ мышке. А\ тут максимум, который тебе доступен,\ --- внутренности
функции-обработчика события. Ладно-ладно, можно кликнуть в\ том\ же дереве объектов (не\ в\ коде!) и\ добавить
произвольную функцию, но\ её\ заголовок и\ текст придётся править отдельно.

![Редактор кода](/images/screenshots/sencha-architect-3-code.png "Редактор кода")

Конечно\ же всё это накладывает заметные ограничения на\ навигацию. Если ты\ видишь в\ коде свойство, которое нужно
подправить, это совершенно не\ означает, что вот так сразу возьмёшь его и\ исправишь. Нет\ уж, будь добр, найди
соответствующий объект в\ дереве, потом найди свойство в\ списке свойств этого объекта и\ уже там пиши своё значение.
Возможно это и\ удобно для разработчика, который видит Sencha Touch первый раз в\ жизни: всё-таки не\ нужно долго
и\ упорно ковыряться в\ документации, выискивая, как пишутся нужные свойства. Но\ я\ гораздо быстрее открою IntelliJ
IDEA или WebStorm и\ сделаю всё то\ же самое, написав пару строк. Да, у\ меня не\ будет мгновенного визуального
представления, да, мне придётся нажимать F5 каждый раз, чтобы проверить изменения, и\ да, у меня будет открыта ещё
и\ вкладка с\ документацией в\ браузере, но\ это всё равно будет быстрее, чем работать с\ подобной системой.

Когда работаешь с\ IDE, специально заточенной под работу с\ определённым фреймворком, то\ ожидаешь, что эта IDE знает
фреймворк от\ и\ до, но\ в\ случае с\ Sencha Architect это не\ всегда так. Например, при переименовании идентификатора
связанные места не\ всегда обновляются автоматически, иногда придётся пробежаться ручками. Та\ же [IntelliJ IDEA]
понимает зависимости между идентификаторами не\ в\ пример лучше, что\ уж говорить про поддержку JavaScript в\ целом.

За\ всё это Sencha хочет [по\ 400 долларов с\ разработчика][price] в\ год. И\ в\ цену не\ входят лицензии на\ сами
фреймворки.

Итак, выводы: Sencha Architect\ --- неплохая среда для новичков или для быстрого создания набросков. Для более сложных
вещей даже [Sublime Text][sublime] может оказаться куда удобнее и\ продуктивнее.

[Sencha Architect]: https://www.sencha.com/products/architect/
[Sencha Touch]: http://www.sencha.com/products/touch/
[extjs]: http://www.sencha.com/products/extjs/
[delphi]: http://en.wikipedia.org/wiki/Embarcadero_Delphi
[price]: https://www.sencha.com/store/architect/
[sublime]: http://www.sublimetext.com/
[IntelliJ IDEA]: http://www.jetbrains.com/idea/
