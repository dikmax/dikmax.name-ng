goog.provide('dikmax.main');

goog.require('dikmax.App');
goog.require('dikmax.Map');
goog.require('goog.dom');

dikmax.main = function() {
  dikmax.App.init();
  dikmax.Map.init();
};

dikmax.main();
