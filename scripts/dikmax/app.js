goog.provide('dikmax.App');

goog.require('goog.array');
goog.require('goog.dom');
goog.require('goog.dom.classlist');
goog.require('goog.dom.dataset');
goog.require('goog.dom.ViewportSizeMonitor');
goog.require('goog.events');
goog.require('goog.html.TrustedResourceUrl');
goog.require('goog.math.Size');
goog.require('goog.net.jsloader');
goog.require('goog.string');
goog.require('goog.string.Const');
goog.require('goog.style');
goog.require('goog.Timer');

const LOAD_DELAY = 100;

dikmax.App.init = function () {
  dikmax.App.setupNavigation_();
  if (!MAP) {
    dikmax.App.setupPanoramas_();
    dikmax.App.setupLazyImages_();
    dikmax.App.setupLazyIframes_();
    dikmax.App.loadDisqus_();
  }
};

dikmax.App.setupNavigation_ = function () {
  const menuButton = goog.dom.getElementByClass('navbar__menu');
  const sidebar = goog.dom.getElementByClass('sidebar');
  const sidebarPanel = goog.dom.getElementByClass('sidebar__panel');

  goog.events.listen(menuButton, goog.events.EventType.CLICK, (event) => {
    event.preventDefault();
    goog.style.setStyle(sidebar, 'display', 'block');
    goog.Timer.callOnce(() => {
      goog.dom.classlist.add(sidebar, 'sidebar_active');
      goog.dom.classlist.add(sidebarPanel, 'sidebar__panel_active');
    }, 10);
  });

  goog.events.listen(sidebar, goog.events.EventType.CLICK, () => {
    goog.dom.classlist.remove(sidebarPanel, 'sidebar__panel_active');
    goog.dom.classlist.remove(sidebar, 'sidebar_active');
    goog.events.listenOnce(sidebar, goog.events.EventType.TRANSITIONEND,
        () => {
          goog.style.setStyle(sidebar, 'display', 'none');
        }
    );
  });
};

/**
 * @param {boolean} allowEnlarge allow making image bigger.
 * @param {?number} maxWidth Max width constrain if needed.
 * @param {?number} maxHeight Max height constrain if needed.
 * @param {HTMLImageElement} image Image to constrain.
 * @return {goog.math.Size} Resulting size.
 * @private
 */
dikmax.App.constrainImage_ = function (
  allowEnlarge, maxWidth, maxHeight, image
) {
  let result = new goog.math.Size(parseInt(image.getAttribute('width'), 10),
      parseInt(image.getAttribute('height'), 10));
  if (allowEnlarge) {
    let scale = 1;
    if (maxWidth) {
      scale = Math.max(scale, maxWidth / result.width);
    }
    if (maxHeight) {
      scale = Math.max(scale, maxHeight / result.height);
    }
    result = result.scale(scale);
  }
  if (maxWidth && result.width > maxWidth) {
    result = result.scale(maxWidth / result.width);
  }
  if (maxHeight && result.height > maxHeight) {
    result = result.scale(maxHeight / result.height);
  }

  return result;
};

dikmax.App.setupLazyImages_ = function () {
  /** @type {[HTMLImageElement, boolean]} */
  let images = goog.array.map(
    goog.dom.getElementsByClass('post__figure-img_lazy'),
    image => [image, false]
  );

  const vsm = new goog.dom.ViewportSizeMonitor();

  const resizeImages = function () {
    const size = vsm.getSize();
    const imageMaxWidth = size.width - 32;
    const imageMaxHeight = size.height - 60;

    goog.array.forEach(images, (image) => {
      const newSize = dikmax.App.constrainImage_(
          false, imageMaxWidth, imageMaxHeight, image[0]);
      goog.style.setSize(image[0], `${newSize.width}px`, `${newSize.height}px`);
    });
  };

  const observer = new IntersectionObserver((entries) => {
    goog.array.forEach(entries, (entry) => {
      const image = entry.target;
      let pair;
      goog.array.forEach(images, (i) => {
        if (i[0] === image) {
          // eslint-disable-next-line no-param-reassign
          i[1] = entry.isIntersecting;
          pair = i;
        }
      });

      if (entry.isIntersecting) {
        // Delay image load for small time in case of fast scroll.
        setTimeout(() => {
          if (!pair[1]) {
            return;
          }

          images = goog.array.filter(images, i => i !== pair);
          observer.unobserve(image);

          // Load
          goog.events.listenOnce(image, goog.events.EventType.LOAD, () => {
            goog.style.setSize(image, '', '');
          });
          image.src = goog.dom.dataset.get(image, 'src');
          image.srcset = goog.dom.dataset.get(image, 'srcset');
          goog.dom.dataset.remove(image, 'src');
          goog.dom.dataset.remove(image, 'srcset');
        }, LOAD_DELAY);
      }
    });
  });

  goog.array.forEach(images, (image) => {
    observer.observe(image[0]);
  });

  goog.events.listen(vsm, goog.events.EventType.RESIZE, () => {
    resizeImages();
  });
  resizeImages();
};

dikmax.App.setupLazyIframes_ = function () {
  const iframes = goog.dom.getElementsByClass('post__embed-lazy');

  const observer = new IntersectionObserver((entries) => {
    goog.array.forEach(entries, (entry) => {
      if (entry.isIntersecting) {
        const iframe = entry.target;
        observer.unobserve(iframe);

        // Load
        iframe.src = goog.dom.dataset.get(iframe, 'src');
        goog.dom.dataset.remove(iframe, 'src');
      }
    });
  });
  goog.array.forEach(iframes, (image) => {
    observer.observe(image);
  });
};

dikmax.App.setupPanoramas_ = function () {
  const images = goog.dom.getElementsByClass('post__figure-img');

  const vsm = new goog.dom.ViewportSizeMonitor();
  const size = vsm.getSize();
  const imageMaxWidth = size.width - 32;
  const imageMaxHeight = size.height - 60;

  goog.array.forEach(images, (image) => {
    if (!(image instanceof HTMLImageElement)) {
      return;
    }
    const smallSize = dikmax.App.constrainImage_(
        false, imageMaxWidth, imageMaxHeight, image);
    const largeSize = dikmax.App.constrainImage_(
        true, null, imageMaxHeight, image);
    if (!goog.math.Size.equals(smallSize, largeSize)) {
      // There's no point to introduce panorama if size wouldn't change.
      const src = goog.dom.dataset.get(image, 'src');
      if (src !== null && goog.string.endsWith(src, '-pano.jpg')) {
        dikmax.App.handlePanorama_(image);
      }
    }
  });
};

const SVG_NS = 'http://www.w3.org/2000/svg';
const PATH_TAG = 'path';

/**
 * @return {!Element} icon.
 * @private
 */
dikmax.App.getZoomInIcon_ = function () {
  /*
   <svg fill="#000000" height="24" viewBox="0 0 24 24" width="24" xmlns="http://www.w3.org/2000/svg">
   <path d="M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5
   3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49
   19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"/>
   <path d="M0 0h24v24H0V0z" fill="none"/>
   <path d="M12 10h-2v2H9v-2H7V9h2V7h1v2h2v1z"/>
   </svg>
   */

  const svg = document.createElementNS(SVG_NS, 'svg');
  svg.setAttribute('viewBox', '0 0 24 24');
  svg.setAttribute('role', 'img');

  const path1 = document.createElementNS(SVG_NS, PATH_TAG);
  path1.setAttribute('d',
      'M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 ' +
      '9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28' +
      'v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5' +
      'S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z'
  );
  svg.appendChild(path1);

  const path2 = document.createElementNS(SVG_NS, PATH_TAG);
  path2.setAttribute('d', 'M0 0h24v24H0V0z');
  path2.setAttribute('fill', 'none');
  svg.appendChild(path2);

  const path3 = document.createElementNS(SVG_NS, PATH_TAG);
  path3.setAttribute('d', 'M12 10h-2v2H9v-2H7V9h2V7h1v2h2v1z');
  svg.appendChild(path3);

  return svg;
};

/**
 * @param {!HTMLImageElement} _image Element to setup pano.
 * @private
 */
dikmax.App.handlePanorama_ = function (_image) {
  const image = _image;
  let expanded = false;
  const inner = goog.dom.getParentElement(image);
  goog.style.setStyle(inner, 'cursor', 'zoom-in');

  // Adding overlay
  const overlay = goog.dom.createDom('div', 'post__figure-pano-overlay',
      dikmax.App.getZoomInIcon_());

  goog.dom.appendChild(inner, overlay);

  // Get names of bigger images
  const smallSrc = goog.dom.dataset.get(image, 'src');
  const smallSrcSet = goog.dom.dataset.get(image, 'srcset');
  const largeSrc = smallSrc.replace(/-pano\.jpg$/, '-pano-full.jpg');
  let largeSrcSet = null;
  if (smallSrcSet) {
    largeSrcSet = smallSrcSet.replace(/-pano\.jpg/g, '-pano-full.jpg');
  }
  goog.events.listen(inner, goog.events.EventType.CLICK, () => {
    expanded = !expanded;
    goog.dom.classlist.enable(image, 'post__figure-img_pano', expanded);
    goog.dom.classlist.enable(inner, 'post__figure-inner_pano', expanded);
    goog.style.setStyle(inner, 'cursor', expanded ? 'zoom-out' : 'zoom-in');
    goog.style.setElementShown(overlay, !expanded);

    image.src = expanded ? largeSrc : smallSrc;
    if (largeSrcSet && smallSrcSet) {
      image.srcset = expanded ? largeSrcSet : smallSrcSet;
    }

    if (expanded) {
      const imageTop = goog.style.getPageOffsetTop(image);
      window.scrollTo(0, imageTop - 60);

      // Scrolling image to center
      const vsm = new goog.dom.ViewportSizeMonitor();
      const size = vsm.getSize();
      const imageMaxHeight = size.height - 60;
      const largeSize = dikmax.App.constrainImage_(
          true, null, imageMaxHeight, image);

      const scrollLeft = (largeSize.width - size.width) / 2;
      if (scrollLeft >= 0) {
        inner.scrollLeft = scrollLeft;
      }
    }
  });
};

const DISQUS_URL =
  goog.html.TrustedResourceUrl.fromConstant(
    goog.string.Const.from('//dikmax.disqus.com/embed.js'));

dikmax.App.loadDisqus_ = function () {
  const observer = new IntersectionObserver((entries) => {
    goog.array.forEach(entries, (entry) => {
      if (entry.isIntersecting) {
        observer.disconnect();
        // Load script.
        goog.net.jsloader.safeLoad(DISQUS_URL);
      }
    });
  });
  const element = goog.dom.getElement('disqus_thread');
  if (element) {
    observer.observe(element);
  }
};
