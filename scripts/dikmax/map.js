goog.provide('dikmax.Map');

/* global L, topojson */

goog.require('goog.dom');
goog.require('goog.net.XhrIo');

class TopoJSON extends L.GeoJSON {
  constructor() { // eslint-disable-line
    super();
  }
  addData(jsonData) {
    if (jsonData['type'] === 'Topology') {
      for (const key in jsonData['objects']) {
        if (jsonData['objects'].hasOwnProperty(key)) {
          const geojson = topojson.feature(jsonData, jsonData['objects'][key]);
          super.addData(geojson);
        }
      }
    } else {
      super.addData(jsonData);
    }
  }
}

dikmax.Map.init = function () {
  const map = L.map(goog.dom.getElementByClass('map__view'))
    .setView([51.505, -0.09], 5);
  goog.net.XhrIo.send('/data/world.json', (e) => {
    const xhr = e.target;
    const json = xhr.getResponseJson();
    const topoLayer = new TopoJSON();
    topoLayer.addData(json);
    topoLayer.addTo(map);
  });
};
