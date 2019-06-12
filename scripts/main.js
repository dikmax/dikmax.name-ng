goog.module('dikmax.main');

const App = goog.require('dikmax.App');
const Higlight = goog.require('dikmax.Higlight');
const dikmaxMap = goog.require('dikmax.Map');
const {HIGHLIGHT_JS, MAP} = goog.require('dikmax.defines');

const main = function () {
  App.init();
  if (HIGHLIGHT_JS) {
    Higlight.init();
  }
  if (MAP) {
    // eslint-disable-next-line no-new
    new dikmaxMap();
  }
};

main();
