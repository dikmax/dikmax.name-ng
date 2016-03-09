goog.provide('dikmax.App');

goog.require('goog.dom');
goog.require('goog.dom.classlist');
goog.require('goog.events');
goog.require('goog.Timer');

dikmax.App = class {
  init() {
    this.setupNavigation_();
  }

  setupNavigation_() {
    const menuButton = goog.dom.getElementByClass('navbar__menu');
    const sidebar = goog.dom.getElementByClass('sidebar');
    const sidebarPanel = goog.dom.getElementByClass('sidebar__panel');

    goog.events.listen(menuButton, goog.events.EventType.CLICK, event => {
      event.preventDefault();
      goog.dom.classlist.add(sidebar, 'sidebar_active');
      goog.Timer.callOnce(() => {
        goog.dom.classlist.add(sidebarPanel, 'sidebar__panel_active');
      }, 10);
    });

    goog.events.listen(sidebar, goog.events.EventType.CLICK, event => {
      event.preventDefault();
      goog.dom.classlist.remove(sidebarPanel, 'sidebar__panel_active');
      goog.events.listenOnce(sidebarPanel, goog.events.EventType.TRANSITIONEND,
          () => {
            goog.dom.classlist.remove(sidebar, 'sidebar_active');
          }
      );
    });
  }
};
