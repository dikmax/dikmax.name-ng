goog.module('dikmax.Higlight');

/* global hljs */

const {forEach} = goog.require('goog.array');

exports = {
  init() {
    const blocks = document.querySelectorAll('pre > code.sourceCode');
    forEach(blocks, hljs.highlightBlock);
  }
};
