/* global L, topojson, greinerHormann */

import {
  forEach as arrayForEach,
  map as arrayMap,
  some as arraySome,
  defaultCompare,
  sort as arraySort
} from 'goog:goog.array';
import {getElementByClass} from 'goog:goog.dom';
import {forEach as objectForEach} from 'goog:goog.object';
import {getJson} from 'goog:goog.labs.net.xhr';
import mapGraticule from './graticule';

class TopoJSON extends L.GeoJSON {
  constructor(geojson, options) { // eslint-disable-line
    super(geojson, options);
  }

  addData(jsonData) {
    if (jsonData['type'] === 'Topology') {
      objectForEach(jsonData['objects'], (data) => {
        const geojson = topojson.feature(jsonData, data);
        super.addData(geojson);
      });
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

/**
 * @param {Object} city
 * @return {string}
 */
function getCityLinks(city) {
  const result = city['name'];

  if (!city['visits']) {
    return result;
  }

  const links = [];
  arrayForEach(city['visits'], (v) => {
    if (v['link']) {
      links.push(v['link']);
    }
  });

  if (links.length === 0) {
    return result;
  }
  if (links.length === 1) {
    return `<a href="${links[0]}">${result}</a>`;
  }
  return arrayMap(links,
    (v, i) => `<a href="${v}">${result} (${i + 1})</a>`).join(', ');
}

/**
 * @param {Object} city
 * @return {boolean} True if blue, false grey.
 */
function getIconStyle(city) {
  if (!city['visits']) {
    return false;
  }

  return arraySome(city['visits'], v => !!v['link']);
}

class MapHandler {
  constructor() {
    this.world = {};
    this.data = {};

    L.Icon.Default.imagePath = '/images/';
    const GreyIcon = L.Icon.Default.extend({
      options: {
        'iconUrl': 'marker-icon-grey.png',
        'iconRetinaUrl': 'marker-icon-2x-grey.png'
      }
    });
    this.greyIcon = new GreyIcon();

    const crs = new L.Proj.CRS('ESRI:53009',
      '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs',
      {
        'resolutions': [65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256]
      });
    this.map = L.map(getElementByClass('map__view'), {
      'crs': crs,
      'maxZoom': 8
    }).fitWorld();

    const frame = mapGraticule({
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

    const graticule = mapGraticule({
      style: {
        'fill': false,
        'color': '#000',
        'opacity': 0.3,
        'weight': 1,
        'dashArray': '5,5'
      }
    });
    graticule.addTo(this.map);

    L.easyButton('<strong style="font-size:1.5em;position:relative;top:-2px">A</strong>', () => {
      document.location = '/map/list/';
    }).addTo(this.map);

    this.loadData();
  }

  loadData() {
    Promise.all([
      getJson('/data/world.json'),
      getJson('/data/map.json')
    ]).then(([world, data]) => {
      this.world = world;
      this.data = data;

      this.setupData();
    }).catch(console.error);
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

    const frame = mapGraticule({
      sphere: true,
      style: {
        'color': '#333',
        'weight': 2,
        'opacity': 1,
        'fill': false
      }
    });
    frame.addTo(this.map);

    const markersLayer = new L.LayerGroup();

    objectForEach(this.data, (country) => {
      // eslint-disable-next-line prefer-destructuring
      const cities = country['cities'];
      arrayForEach(cities, (city) => {
        const options = {};
        if (!getIconStyle(city)) {
          // If city have no blog posts about it, show grey marker.
          options['icon'] = this.greyIcon;
        }
        const marker = new L.Marker([city['lat'], city['lon']], options);
        marker.bindPopup(getCityLinks(city));
        marker.addTo(markersLayer);
      });
    });

    this.map.on('zoomend', () => {
      const zoom = this.map.getZoom();
      if (zoom > 3) {
        this.map.addLayer(markersLayer);
      } else {
        this.map.removeLayer(markersLayer);
      }
    });
  }

  countryStyle(feature) {
    let country = feature['id'];
    let region = null;
    if (country.indexOf('-') !== -1) {
      region = country;
      [country] = country.split('-');
    }
    if (this.data[country]) {
      if ((region && this.data[country]['regions'][region]) || !region) {
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
      arrayForEach(latlngs, (shape) => {
        const s = shape;
        if (360 - Math.abs(s[0].lng - s[s.length - 1].lng) < 5) {
          for (let l = s[0].lat; l > -90; l -= 1) {
            s.unshift(new L.LatLng(l, s[0].lng < 0 ? -179.9999 : 179.9999));
          }
          s.unshift(new L.LatLng(-90, s[0].lng < 0 ? -179.9999 : 179.9999));
          for (let l = s[s.length - 1].lat; l > -90; l -= 1) {
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
    arrayForEach(latlngs, (shape) => {
      const polygon = new L.Polygon(shape);
      const polygonBounds = polygon.getBounds();
      if (360 - (polygonBounds.getEast() - polygonBounds.getWest()) > 10) {
        result.push(shape);
        return;
      }
      const pointsA = arrayMap(shape, latlng => ({
        x: latlng.lng < 0 ? latlng.lng + 360 : latlng.lng,
        y: latlng.lat
      }));
      const pointsB = arrayMap(shape, latlng => ({
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
        arrayForEach(resA, (i) => {
          result.push(arrayMap(i, point => new L.LatLng(point.y, point.x)));
        });
      }
      if (!goog.isNull(resB)) {
        arrayForEach(resB, (i) => {
          result.push(arrayMap(i, point => new L.LatLng(point.y, point.x)));
        });
      }
    });
    layer.setLatLngs(result);
    layer.setStyle(this.countryStyle(feature));
  }


  bindLayer(layer, feature) {
    let country = feature['id'];
    let content = 'Неизведанная территория';
    let countryData;
    if (country.indexOf('-') !== -1) {
      const region = country;
      [country] = country.split('-');

      countryData = this.data[country];
      if (countryData) {
        content = countryData['name'];

        if (countryData['regions']
          && countryData['regions'][region]
        ) {
          content += ` — ${countryData['regions'][region]}`;
        }
      }
    } else {
      countryData = this.data[country];
      if (countryData) {
        content = countryData['name'];
      }
    }

    if (countryData) {
      arraySort(countryData['cities'],
        (a, b) => defaultCompare(a['name'], b['name']));
      const cities = arrayMap(countryData['cities'], getCityLinks);
      content = `<h1>${content}</h1><p>${cities.join(', ')}</p>`;
    }
    layer.bindPopup(content);
  }
}

export default function init() {
  // eslint-disable-next-line no-new
  new MapHandler();
}
