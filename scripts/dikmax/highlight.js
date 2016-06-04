goog.provide('dikmax.Higlight');

/* global hljs */

goog.require('goog.array');

dikmax.Higlight.init = function () {
  const blocks = document.querySelectorAll('pre > code.sourceCode');
  goog.array.forEach(blocks, hljs.highlightBlock);
};
