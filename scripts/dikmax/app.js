goog.provide('dikmax.App');

goog.require('goog.dom');
goog.require('goog.dom.classlist');
goog.require('goog.events');
goog.require('goog.style');
goog.require('goog.Timer');

dikmax.App.init = function() {
  dikmax.App.setupNavigation_();
  dikmax.App.scrollAndResizeTracker_();
  dikmax.App.setupHighlighting_();
};

dikmax.App.setupNavigation_ = function() {
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
    goog.dom.classlist.remove(sidebarPanel, 'sidebar__panel_active');
    goog.events.listenOnce(sidebarPanel, goog.events.EventType.TRANSITIONEND,
        () => {
          goog.dom.classlist.remove(sidebar, 'sidebar_active');
        }
    );
  });
};

dikmax.App.scrollAndResizeTracker_ = function() {
  // TODO calculate from center
  const blocks = goog.dom.getElementsByClass('post__block');
  let block = -1;
  let offset = 0;
  goog.events.listen(window, goog.events.EventType.SCROLL,
      () => {
        const scroll = goog.dom.getDocumentScroll();
        let index = goog.array.binarySelect(
            blocks, el => {
              const pageOffsetTop = goog.style.getPageOffsetTop(el);
              return scroll.y - pageOffsetTop;
            }
        );
        if (index < 0) {
          index = -index - 2;
        }

        block = index;
        if (index >= 0) {
          const pageOffsetTop = goog.style.getPageOffsetTop(blocks[index]);
          offset = scroll.y - pageOffsetTop;
        } else {
          offset = scroll.y;
        }
      }
  );

  goog.events.listen(window, goog.events.EventType.RESIZE,
      () => {
        if (block < 0) {
          return;
        }
        const blockOffset = goog.style.getPageOffsetTop(blocks[block]);
        const newOffset = blockOffset + offset;
        window.scrollTo(0, newOffset);
      }
  );
};

dikmax.App.setupHighlighting_ = function() {
  const blocks = document.querySelectorAll('pre > code.sourceCode');
  for (var i = 0; i < blocks.length; ++i) {
    var block = blocks[i];  // Calling myNodeList.item(i) isn't necessary in JavaScript
    console.log(block);
    hljs.highlightBlock(block);
  }
};
