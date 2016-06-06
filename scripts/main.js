goog.provide('dikmax.main');

goog.require('dikmax.App');
goog.require('dikmax.Higlight');
goog.require('dikmax.Map');
goog.require('goog.dom');

/**
 * Include highlighting
 *
 * @define {boolean}
 */
const HIGHLIGHT_JS = false;

/**
 * Include maps.
 *
 * @define {boolean}
 */
const MAP = false;

dikmax.main = function () {
  dikmax.App.init();
  if (HIGHLIGHT_JS) {
    dikmax.Higlight.init();
  }
  if (MAP) {
    new dikmax.Map();
  }
};

dikmax.main();
