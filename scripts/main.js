goog.provide('dikmax.main');

goog.require('dikmax.App');
goog.require('goog.dom');

dikmax.main = function() {
  const app = new dikmax.App();
  app.init();
};

dikmax.main();
