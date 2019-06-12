import appInit from './dikmax/app';
import {HIGHLIGHT_JS, MAP} from './dikmax/defines';
import higlightInit from './dikmax/highlight';
import mapInit from './dikmax/map';

const main = function () {
  appInit();
  if (HIGHLIGHT_JS) {
    higlightInit();
  }
  if (MAP) {
    mapInit();
  }
};

main();
