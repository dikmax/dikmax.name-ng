/* global hljs */

import {forEach} from 'goog:goog.array';

export default function init() {
  const blocks = document.querySelectorAll('pre > code.sourceCode');
  forEach(blocks, hljs.highlightElement);
}
