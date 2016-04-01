goog.provide('dikmax.App');

/* global hljs */

goog.require('goog.array');
goog.require('goog.dom');
goog.require('goog.dom.classlist');
goog.require('goog.dom.dataset');
goog.require('goog.dom.ViewportSizeMonitor');
goog.require('goog.events');
goog.require('goog.style');
goog.require('goog.Timer');

dikmax.App.init = function() {
  dikmax.App.setupNavigation_();
  dikmax.App.scrollAndResizeTracker_();
  dikmax.App.setupHighlighting_();
  dikmax.App.setupLazyImages_();
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

  goog.events.listen(sidebar, goog.events.EventType.CLICK, () => {
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

  var vsm = new goog.dom.ViewportSizeMonitor();
  goog.events.listen(vsm, goog.events.EventType.RESIZE,
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
  goog.array.forEach(blocks, hljs.highlightBlock);
};

dikmax.App.setupLazyImages_ = function() {
  let images = goog.dom.getElementsByClass('post__figure-img_lazy');

  const vsm = new goog.dom.ViewportSizeMonitor();

  const resizeImages = function() {
    const size = vsm.getSize();
    const imageMaxWidth = size.width - 32;
    const imageMaxHeight = size.height - 60;

    goog.array.forEach(images, image => {
      let width = image.getAttribute('width');
      let height = image.getAttribute('height');
      if (width > imageMaxWidth) {
        height = height * imageMaxWidth / width;
        width = imageMaxWidth;
      }
      if (height > imageMaxHeight) {
        width = width * imageMaxHeight / height;
        height = imageMaxHeight;
      }
      goog.style.setWidth(image, width + 'px');
      goog.style.setHeight(image, height + 'px');
    });
  };

  var scrollHandler = () => {
    const scroll = goog.dom.getDocumentScroll();
    const size = vsm.getSize();
    const trackTop = scroll.y + size.height * 1.5;
    const trackBottom = scroll.y - size.height * 1.5;

    const newImages = [];

    goog.array.forEach(images, image => {
      const imageTop = goog.style.getPageOffsetTop(image);
      const imageBottom = imageTop +
          goog.style.getBorderBoxSize(image).height;

      if (imageTop < trackTop && imageBottom > trackBottom) {
        console.log('Reveal', image);
        goog.events.listenOnce(image, goog.events.EventType.LOAD, () => {
          goog.style.setWidth(image, 'auto');
          goog.style.setHeight(image, 'auto');
        });
        image.src = goog.dom.dataset.get(image, 'src');
        image.srcset = goog.dom.dataset.get(image, 'srcset');
        goog.dom.dataset.remove(image, 'src');
        goog.dom.dataset.remove(image, 'srcset');
      } else {
        newImages.push(image);
      }
    });
    images = newImages;
  };

  goog.events.listen(window, goog.events.EventType.SCROLL, scrollHandler);
  goog.events.listen(vsm, goog.events.EventType.RESIZE, () => {
    scrollHandler();
    resizeImages();
  });
  scrollHandler();
  resizeImages();
};
