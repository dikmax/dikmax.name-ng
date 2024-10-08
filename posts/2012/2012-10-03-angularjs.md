---
title: "AngularJS"
date: "2012-10-03T16:02:41+03:00"
published: true
tags: [angularjs, javascript, программирование, работа]
thread: 869572547
---

Вы, наверное, слышали об этом JavaScript-фреймворке. Модный тренд, если позволите так выразиться.
Superheroic JavaScript MVW[^1] Framework, как утвержают сами разработчики. Не удержался и посмотрел на него,
тем более что выдалась возможность сделать это в рабочее время и за счет заказчика. Мы выбираем основу для будущего
проекта, вот и решили глянуть современные js фреймворки, и мне достался [AngularJS](http://angularjs.org/). Поделюсь
своими впечатлениями.

Не знаю как с производительностью, но с функциональностью у этого фреймворка полный порядок. Покажу на небольшом
примере. Итак, у нас есть кусочек разметки:

~~~~~html
<table class="table table-striped table-hover" ng-controller="TestController">
    <thead>
        <tr>
            <th>Action</th>
        </tr>
    </thead>
    <tbody>
        <tr ng-repeat="action in actions">
            <td>{{action.action}}</td>
        </tr>
    </tbody>
</table>
~~~~~

Дальше мы подключаем JavaScript:

~~~~~javascript
function TestController($scope) {
    $scope.actions = [
        {action: 'Action1'},
        {action: 'Action2'}
    ];
}
~~~~~

Ну и всё. У нас готова страница с таблицей, в которой 2 строки: “Action1” и “Action2”. Но это еще не все. Если мы потом
добавим в поле `actions` еще чего-нибудь, то оно тоже автоматически отобразится на странице.

Единственное, мне потребовалось некоторое время, чтобы разобраться с некоторыми более сложными вещами. Например,
создание независимых компонентов. В моей задаче требовалось, чтобы родительский компонент передавал данные для
дочернего. В результате оказалось, что на момент вызова родительского контроллера дочерний еще не инициализирован и
нужно немного подождать. И лучше всего использовать для этого событие `$viewContentLoaded`. Чтобы это понять,
официального сайта было недостаточно, и пришлось провести пару-тройку часов в обнимку с дебаггером и гуглом.

В общем, впечатление от фреймворка очень положительное, надо будет попробовать написать что-нибудь более-менее серьезное. 

Еще примеры можете оценить на [главной странице фреймворка](http://angularjs.org/#add-some-control), они там гораздо
лучше, чем я вам привел.

[^1]: Model-View-Whatever
