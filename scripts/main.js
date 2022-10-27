import appInit from './dikmax/app';
import {HIGHLIGHT_JS, MAP} from './dikmax/defines';
import higlightInit from './dikmax/highlight';
import mapInit from './dikmax/map';
import initCookies from './dikmax/cookies';

const main = function () {
  initCookies();
  appInit();
  if (HIGHLIGHT_JS) {
    higlightInit();
  }
  if (MAP) {
    mapInit();
  }
};

main();
