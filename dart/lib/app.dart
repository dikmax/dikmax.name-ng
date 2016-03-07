library app;

import 'dart:async';
import 'dart:html';

class App {
  App() {
    _setupNavigation();
  }

  void _setupNavigation() {
    querySelector('.navbar__menu').onClick.listen((e) {
      e.stopPropagation();
      e.preventDefault();
      querySelector('.sidebar').classes.add('sidebar_active');
      window.requestAnimationFrame((_) {
        querySelector('.sidebar__panel').classes.add('sidebar__panel_active');
      });
    });

    querySelector('.sidebar').onClick.listen((e) {
      e.stopPropagation();
      e.preventDefault();
      querySelector('.sidebar__panel')
          ..classes.remove('sidebar__panel_active')
          ..onTransitionEnd.first.then((e) {
            querySelector('.sidebar').classes.remove('sidebar_active');
          });
    });
  }
}