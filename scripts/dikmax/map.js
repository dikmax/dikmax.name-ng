goog.provide('dikmax.Map');

/* global L, topojson, greinerHormann */

goog.require('goog.array');
goog.require('goog.dom');
goog.require('goog.Promise');
goog.require('goog.net.XhrIo');

goog.require('dikmax.graticule');

class TopoJSON extends L.GeoJSON {
  constructor(geojson, options) { // eslint-disable-line
    super(geojson, options);
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

const colors = {
  'blue': '#a3cec5',
  'green': '#d3e46f',
  'orange': '#fdc663',
  'pink': '#f3c1d3',
  'purple': '#ceb5cf',
  'red': '#fdaf6b',
  'turquoise': '#aadb78',
  'yellow': '#fae364'
};

dikmax.Map = class {
  constructor() {
    this.world = {};
    this.data = {};

    const crs = new L.Proj.CRS('ESRI:53009',
      '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs',
      {
        'resolutions': [65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256]
      });
    this.map = L.map(goog.dom.getElementByClass('map__view'), {'crs': crs})
      .fitWorld();

    const frame = dikmax.graticule({
      sphere: true,
      style: {
        'color': '#333',
        'weight': 2,
        'opacity': 1,
        'fillColor': '#ddeeff',
        'fillOpacity': 1
      }
    });
    frame.addTo(this.map);

    const graticule = dikmax.graticule({
      style: {
        'fill': false,
        'color': '#000',
        'opacity': 0.3,
        'weight': 1,
        'dashArray': '5,5'
      }
    });
    graticule.addTo(this.map);

    this.loadData();
  }

  loadData() {
    const worldPromise = new goog.Promise((resolve, reject) => {
      goog.net.XhrIo.send('/data/world.json', (e) => {
        const xhr = e.target;
        if (!xhr.isSuccess()) {
          reject(xhr);
          return;
        }
        const json = xhr.getResponseJson();
        resolve(json);
      });
    });

    const dataPromise = new goog.Promise((resolve, reject) => {
      goog.net.XhrIo.send('/data/map.json', (e) => {
        const xhr = e.target;
        if (!xhr.isSuccess()) {
          reject(xhr);
          return;
        }
        const json = xhr.getResponseJson();
        resolve(json);
      });
    });

    goog.Promise.all([worldPromise, dataPromise]).then(([world, data]) => {
      this.world = world;
      this.data = data;

      this.setupData();
    }).thenCatch((err) => {
      console.error(err);
    });
  }

  setupData() {
    const topoLayer = new TopoJSON(null, {
      style: this.countryStyle.bind(this),
      /**
       * @param {Object} feature
       * @param {L.Marker|L.FeatureGroup|L.Polyline|L.Polygon} layer
       */
      onEachFeature: this.onEachFeature.bind(this)
    });
    topoLayer.addData(this.world);
    topoLayer.addTo(this.map);

    const frame = dikmax.graticule({
      sphere: true,
      style: {
        'color': '#333',
        'weight': 2,
        'opacity': 1,
        'fill': false
      }
    });
    frame.addTo(this.map);
  }

  countryStyle(feature) {
    let country = feature['id'];
    let region = null;
    if (country.indexOf('-') !== -1) {
      region = country;
      country = country.split('-')[0];
    }
    if (this.data[country]) {
      if (region && this.data[country]['regions'][region] || !region) {
        return {
          'fillColor': colors[this.data[country]['color']],
          'color': '#226688',
          'opacity': 1,
          'fillOpacity': 1,
          'weight': 1
        };
      }
    }
    return {
      'fillColor': '#ffffdd',
      'color': '#226688',
      'opacity': 1,
      'fillOpacity': 1,
      'weight': 1
    };
  }

  /**
   * @param {Object} feature
   * @param {L.Marker|L.FeatureGroup|L.Polyline|L.Polygon} layer
   */
  onEachFeature(feature, layer) {
    this.bindLayer(layer, feature);
    if (!goog.isFunction(layer.getBounds)) {
      return;
    }
    const bounds = layer.getBounds();
    if (360 - (bounds.getEast() - bounds.getWest()) > 10) {
      return;
    }

    if (!goog.isFunction(layer.getLatLngs)) {
      return;
    }

    if (feature['id'] === 'ATA') { // Antarctica
      /** @type {Array<Array<L.LatLng>>} */
      const latlngs = layer.getLatLngs();
      goog.array.forEach(latlngs, (shape) => {
        const s = shape;
        if (360 - Math.abs(s[0].lng - s[s.length - 1].lng) < 5) {
          for (let l = s[0].lat; l > -90; --l) {
            s.unshift(new L.LatLng(l, s[0].lng < 0 ? -179.9999 : 179.9999));
          }
          s.unshift(new L.LatLng(-90, s[0].lng < 0 ? -179.9999 : 179.9999));
          for (let l = s[s.length - 1].lat; l > -90; --l) {
            s.push(new L.LatLng(l, s[s.length - 1].lng < 0 ? -179.9999 : 179.9999));
          }
          s.push(new L.LatLng(-90, s[s.length - 1].lng < 0 ? -179.9999 : 179.9999));
        }
      });

      layer.setLatLngs(latlngs);
      return;
    }

    /** @type {Array<Array<L.LatLng>>} */
    const latlngs = layer.getLatLngs();
    const result = [];
    // Wrapping and splitting polygons that intersects antimeridian around.
    goog.array.forEach(latlngs, (shape) => {
      const polygon = new L.Polygon(shape);
      const polygonBounds = polygon.getBounds();
      if (360 - (polygonBounds.getEast() - polygonBounds.getWest()) > 10) {
        result.push(shape);
        return;
      }
      const pointsA = goog.array.map(shape, (latlng) => ({
        x: latlng.lng < 0 ? latlng.lng + 360 : latlng.lng,
        y: latlng.lat
      }));
      const pointsB = goog.array.map(shape, (latlng) => ({
        x: latlng.lng > 0 ? latlng.lng - 360 : latlng.lng,
        y: latlng.lat
      }));
      const resA = greinerHormann.intersection(pointsA,
        [{x: 0, y: 90},
          {x: 179.9999, y: 90},
          {x: 179.9999, y: -90},
          {x: 0, y: -90}]);
      const resB = greinerHormann.intersection(pointsB,
        [{x: 0, y: 90},
          {x: -179.9999, y: 90},
          {x: -179.9999, y: -90},
          {x: 0, y: -90}]);
      if (!goog.isNull(resA)) {
        goog.array.forEach(resA, (i) => {
          result.push(goog.array.map(i, (point) => new L.LatLng(point.y, point.x)));
        });
      }
      if (!goog.isNull(resB)) {
        goog.array.forEach(resB, (i) => {
          result.push(goog.array.map(i, (point) => new L.LatLng(point.y, point.x)));
        });
      }
    });
    layer.setLatLngs(result);
    layer.setStyle(this.countryStyle(feature));
  }

  bindLayer(layer, feature) {
    let country = feature['id'];
    let name = 'Неизведанная территория';
    if (country.indexOf('-') !== -1) {
      const region = country;
      country = country.split('-')[0];

      if (this.data[country]) {
        name = this.data[country]['name'];

        if (this.data[country]['regions'] &&
            this.data[country]['regions'][region]) {
          name += ` — ${this.data[country]['regions'][region]}`;
        }
      }
    } else {
      if (this.data[country]) {
        name = this.data[country]['name'];
      }
    }
    layer.bindPopup(name);
  }
};
