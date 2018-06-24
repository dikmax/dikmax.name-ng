/* eslint-disable */

var L = {};

/**
 * @interface
 */
var ILayer = class {};

L.Class = class {};
/**
 * @type {Object}
 */
L.Class.prototype.options = {};

L.Util = {
  /**
   * @param {Object} obj
   * @param {Object=} options
     */
  setOptions(obj, options) {}
};

L.Point = class {
  /**
   * @param {number} x
   * @param {number} y
   * @param {boolean=} round
   */
  constructor(x, y, round) {};
};
/**
 * @type {number}
 */
L.Point.prototype.x = 0;
/**
 * @type {number}
 */
L.Point.prototype.y = 0;

L.Bounds = class {
  /**
   * @param {L.Point|Array<number>} a
   * @param {L.Point|Array<number>} b
   */
  constructor(a, b) {};
};
/**
 * @type {L.Point}
 */
L.Bounds.prototype.min = null;
/**
 * @type {L.Point}
 */
L.Bounds.prototype.max = null;


/**
 * @param {L.Point|Array<number>} a
 * @param {L.Point|Array<number>} b
 * @return {L.Bounds}
 */
L.bounds = function(a, b) {};

L.LatLng = class {
  /**
   * @param {number} lat
   * @param {number} lng
   * @param {number=} alt
   */
  constructor(lat, lng, alt){}
};

/**
 * @type {number}
 */
L.LatLng.prototype.lat = 0;
/**
 * @type {number}
 */
L.LatLng.prototype.lng = 0;
/**
 * @type {?number}
 */
L.LatLng.prototype.alt = null;

L.LatLngBounds = class {
  /**
   * @return {number}
   * @nosideeffects
   */
  	getWest() {}

  /**
   * @return {number}
   * @nosideeffects
   */
  getSouth() {}

  /**
   * @return {number}
   * @nosideeffects
   */
  getEast() {}

  /**
   * @return {number}
   * @nosideeffects
   */
  getNorth() {}
};

L.Projection = class {
  /**
   * @param {L.LatLng} latlng
   * @return {L.Point}
   * @nosideeffects
   */
  project(latlng) {};

  /**
   * @param {L.Point} point
   * @return {L.LatLng}
   * @nosideeffects
   */
  unproject(point) {};

  /**
   * @return {L.Bounds}
   */
  bounds() {};
};

L.Proj = {};

L.Proj.CRS = class extends L.Class {
  /**
   * @param {string} a
   * @param {string} b
   * @param {Object=} c
   */
  constructor(a, b, c) {}
};

L.Icon = class {};

L.Icon.Default = {
  /**
   * @type {string}
   */
  imagePath: '',

  extend(extra) {}
};

/**
 * @implements ILayer
 */
L.LayerGroup = class extends L.Class {
  constructor() {};

  /**
   * @param {ILayer} map
   * @return this
   */
  addTo (map) {};
};

L.Popup = class extends L.Class {};

L.Marker = class extends L.Class {
  /**
   * @param {ILayer} map
   * @return this
   */
  addTo(map) {};

  /**
   * @param {string|HTMLElement|L.Popup} content
   * @param {Object=} options
   */
  bindPopup(content, options) {};
};

L.Path = class extends L.Class {

  /**
   * @param {string|HTMLElement|L.Popup} content
   * @param {Object=} options
     */
  bindPopup(content, options) {};

  /**
   * @param {Object} style
   */
  setStyle(style) {};
};

L.Polyline = class extends L.Path {
  /**
   * @return {L.LatLngBounds}
   * @nosideeffects
   */
  getBounds() {}

  /**
   * @return {Array<Array<L.LatLng>>}
   * @nosideeffects
   */
  getLatLngs() {}

  /**
   * @param {Array<Array<L.LatLng>>} latlngs
   */
  setLatLngs(latlngs) {}
};

L.Polygon = class extends L.Polyline {
  /**
   * @param {Array<L.LatLng>} latlngs
   * @param {Object=} options
     */
  constructor(latlngs, options) {};
};

L.FeatureGroup = class extends L.LayerGroup {
  /**
   * @return {L.LatLngBounds}
   * @nosideeffects
   */
  getBounds() {}
};

L.GeoJSON = class extends L.FeatureGroup {
  /**
   * @param {Object=} geojson
   * @param {Object=} options
   */
  constructor(geojson, options) {};

  /**
   * @param {Object} geojson
   * @return this
   */
  addData (geojson) {};
};
L.GeoJSON.prototype.options = {
  /**
   * @param {Object} feature
   * @param {L.Marker|L.FeatureGroup|L.Polyline|L.Polygon} layer
   */
  onEachFeature(feature, layer) {}
};

/**
 * @implements ILayer
 */
L.Map = class extends L.Class {
  /**
   * @param {string|Element} id
   * @param {Object=} options
   */
  constructor(id, options) {};

  /**
   * @param {Array<number>} center
   * @param {number} zoom
   * @return {L.Map}
   */
  setView(center, zoom) {};

  /**
   * @return this
   */
  fitWorld() {};

  /**
   * @return {number}
   */
  getZoom() {};

  /**
   * @param {L.LayerGroup} layer
   */
  addLayer(layer) {};

  /**
   * @param {L.LayerGroup} layer
   */
  removeLayer(layer) {};

  /**
   * @param {string} event
   * @param {function()} handler
   */
  on(event, handler) {};
};


/**
 * @param {string|Element} id
 * @param {Object=} options
 * @return {L.Map}
 */
L.map = function (id, options) {};

/**
 * @param {string} content
 * @param {function()} handler
 */
L.easyButton = function (content, handler) {};
