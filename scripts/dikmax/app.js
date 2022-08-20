import {forEach as arrayForEach} from 'goog:goog.array';
import {getElementByClass, getElementsByClass, getParentElement, createDom, appendChild} from 'goog:goog.dom';
import classlist from 'goog:goog.dom.classlist';
import dataset from 'goog:goog.dom.dataset';
import ViewportSizeMonitor from 'goog:goog.dom.ViewportSizeMonitor';
import events from 'goog:goog.events';
import Size from 'goog:goog.math.Size';
import style from 'goog:goog.style';
import asserts from 'goog:goog.asserts';
import Timer from 'goog:goog.Timer';
import {MAP} from './defines';

function setupNavigation() {
  const menuButton = getElementByClass('navbar__menu');
  const sidebar = getElementByClass('sidebar');
  const sidebarPanel = getElementByClass('sidebar__panel');

  events.listen(menuButton, events.EventType.CLICK, (event) => {
    event.preventDefault();
    style.setStyle(sidebar, 'display', 'block');
    Timer.callOnce(() => {
      classlist.add(sidebar, 'sidebar_active');
      classlist.add(sidebarPanel, 'sidebar__panel_active');
    }, 10);
  });

  events.listen(sidebar, events.EventType.CLICK, () => {
    classlist.remove(sidebarPanel, 'sidebar__panel_active');
    classlist.remove(sidebar, 'sidebar_active');
    events.listenOnce(sidebar, events.EventType.TRANSITIONEND,
      () => {
        style.setStyle(sidebar, 'display', 'none');
      });
  });
}

/**
 * @param {boolean} allowEnlarge allow making image bigger.
 * @param {?number} maxWidth Max width constrain if needed.
 * @param {?number} maxHeight Max height constrain if needed.
 * @param {HTMLImageElement} image Image to constrain.
 * @return {Size} Resulting size.
 * @private
 */
function constrainImage(
  allowEnlarge, maxWidth, maxHeight, image
) {
  let result = new Size(
    parseInt(image.getAttribute('width'), 10),
    parseInt(image.getAttribute('height'), 10)
  );
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
}

function setupLazyIframes() {
  const iframes = getElementsByClass('post__embed-lazy');

  const observer = new IntersectionObserver((entries) => {
    arrayForEach(entries, ({target, isIntersecting}) => {
      const iframe = /** @type !HTMLIFrameElement */ (target);
      if (isIntersecting) {
        observer.unobserve(iframe);

        // Load
        iframe.src = asserts.assert(dataset.get(iframe, 'src'));
        dataset.remove(iframe, 'src');
      }
    });
  });
  arrayForEach(iframes, (iframe) => {
    observer.observe(iframe);
  });
}

const PANO_TEST_REGEXP = /-pano\.jpg(\.webp|\.avif)?$/;

function setupPanoramas () {
  const images = document.querySelectorAll('picture>.post__figure-img');

  const vsm = new ViewportSizeMonitor();
  const size = vsm.getSize();
  const imageMaxWidth = size.width - 32;
  const imageMaxHeight = size.height - 60;

  arrayForEach(images, (image) => {
    asserts.assert(image instanceof HTMLImageElement);
    const smallSize = constrainImage(
      false, imageMaxWidth, imageMaxHeight, image
    );
    const largeSize = constrainImage(
      true, null, imageMaxHeight, image
    );
    if (!Size.equals(smallSize, largeSize)) {
      // There's no point to introduce panorama if size wouldn't change.
      const src = image.src;
      if (src !== null && PANO_TEST_REGEXP.test(src)) {
        handlePanorama(image);
      }
    }
  });
}

const SVG_NS = 'http://www.w3.org/2000/svg';
const PATH_TAG = 'path';

/**
 * @return {!Element} icon.
 * @private
 */
function getZoomInIcon() {
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
    'M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 '
      + '9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28'
      + 'v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5'
      + 'S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z');
  svg.appendChild(path1);

  const path2 = document.createElementNS(SVG_NS, PATH_TAG);
  path2.setAttribute('d', 'M0 0h24v24H0V0z');
  path2.setAttribute('fill', 'none');
  svg.appendChild(path2);

  const path3 = document.createElementNS(SVG_NS, PATH_TAG);
  path3.setAttribute('d', 'M12 10h-2v2H9v-2H7V9h2V7h1v2h2v1z');
  svg.appendChild(path3);

  return svg;
}

/**
 * @param {!HTMLImageElement} image Element to setup pano.
 * @private
 */
function handlePanorama(image) {
  const picture = image.parentElement;
  let expanded = false;
  const inner = getParentElement(picture);
  style.setStyle(inner, 'cursor', 'zoom-in');

  // Adding bigger image
  const bigPicture = picture.cloneNode(true);
  style.setElementShown(bigPicture, false);
  arrayForEach(bigPicture.children, (child) => {
    if (child.src) {
      child.src = child.src.replace(/-pano\.jpg/, '-pano-full.jpg')
    }
    if (child.srcset) {
      child.srcset = child.srcset.replace(/-pano\.jpg/g, '-pano-full.jpg')
    }
    if (child instanceof HTMLImageElement) {
      classlist.add(child, 'post__figure-img_pano');
    }
  });

  appendChild(inner, bigPicture);

  // Adding overlay
  const overlay = createDom('div', 'post__figure-pano-overlay',
    getZoomInIcon());

  appendChild(inner, overlay);

  // Add click handler
  events.listen(inner, events.EventType.CLICK, () => {
    expanded = !expanded;
    classlist.enable(inner, 'post__figure-inner_pano', expanded);
    style.setStyle(inner, 'cursor', expanded ? 'zoom-out' : 'zoom-in');
    style.setElementShown(overlay, !expanded);
    style.setElementShown(bigPicture, expanded);
    style.setElementShown(picture, !expanded);

    if (expanded) {
      const imageTop = style.getPageOffsetTop(bigPicture);
      window.scrollTo(0, imageTop - 60);

      // Scrolling image to center
      const vsm = new ViewportSizeMonitor();
      const size = vsm.getSize();
      const imageMaxHeight = size.height - 60;
      const largeSize = constrainImage(
        true, null, imageMaxHeight, image
      );

      const scrollLeft = (largeSize.width - size.width) / 2;
      if (scrollLeft >= 0) {
        inner.scrollLeft = scrollLeft;
      }
    }
  });
}

export default function init() {
  setupNavigation();
  if (!MAP) {
    setupPanoramas();
    setupLazyIframes();
  }
}
