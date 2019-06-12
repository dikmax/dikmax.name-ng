/* eslint-disable */

var L = {};

/**
 * @interface
 */
var ILayer = function() {};

/** @constructor */
L.Class = function () {};

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

/**
 * @param {number} x
 * @param {number} y
 * @param {boolean=} round
 * @constructor
 */
L.Point = function(x, y, round) {};
/**
 * @type {number}
 */
L.Point.prototype.x = 0;
/**
 * @type {number}
 */
L.Point.prototype.y = 0;

/**
 * @param {L.Point|Array<number>} a
 * @param {L.Point|Array<number>} b
 * @constructor
 */
L.Bounds = function(a, b) {};
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

/**
 * @param {number} lat
 * @param {number} lng
 * @param {number=} alt
 * @constructor
 */
L.LatLng = function(lat, lng, alt){};
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

/** @constructor */
L.LatLngBounds = function() {};
/**
 * @return {number}
 * @nosideeffects
 */
L.LatLngBounds.prototype.getWest = function() {};
/**
 * @return {number}
 * @nosideeffects
 */
L.LatLngBounds.prototype.getSouth = function() {};
/**
 * @return {number}
 * @nosideeffects
 */
L.LatLngBounds.prototype.getEast = function() {};
/**
 * @return {number}
 * @nosideeffects
 */
L.LatLngBounds.prototype.getNorth = function() {};

/** @constructor */
L.Projection = function(){};
/**
 * @param {L.LatLng} latlng
 * @return {L.Point}
 * @nosideeffects
 */
L.Projection.prototype.project = function (latlng) {};
/**
 * @param {L.Point} point
 * @return {L.LatLng}
 * @nosideeffects
 */
L.Projection.prototype.unproject = function (point) {};
/**
 * @return {L.Bounds}
 */
L.Projection.prototype.bounds = function () {};

L.Proj = {};

/**
 * @param {string} a
 * @param {string} b
 * @param {Object=} c
 * @constructor
 * @extends {L.Class}
 */
L.Proj.CRS = function(a, b, c) {};

/** @constructor */
L.Icon = function () {};

L.Icon.Default = {
  /**
   * @type {string}
   */
  imagePath: '',

  extend(extra) {}
};

/**
 * @constructor
 * @extends {L.Class}
 * @implements ILayer
 */
L.LayerGroup = function() {};
/**
 * @param {ILayer} map
 * @return this
 */
L.LayerGroup.prototype.addTo = function(map) {};

/**
 * @constructor
 * @extends {L.Class}
 */
L.Popup = function() {};

/**
 * @constructor
 * @extends {L.Class}
 */
L.Marker = function() {};
/**
 * @param {ILayer} map
 * @return this
 */
L.Marker.prototype.addTo = function(map) {};

/**
 * @param {string|HTMLElement|L.Popup} content
 * @param {Object=} options
 */
L.Marker.prototype.bindPopup = function(content, options) {};

/**
 * @constructor
 * @extends {L.Class}
 */
L.Path = function() {};
/**
 * @param {string|HTMLElement|L.Popup} content
 * @param {Object=} options
   */
L.Path.prototype.bindPopup = function(content, options) {};

/**
 * @param {Object} style
 */
L.Path.prototype.setStyle = function(style) {};

/**
 * @constructor
 * @extends {L.Path}
 */
L.Polyline = function() {};
/**
 * @return {L.LatLngBounds}
 * @nosideeffects
 */
L.Polyline.prototype.getBounds = function () {};

/**
 * @return {Array<Array<L.LatLng>>}
 * @nosideeffects
 */
L.Polyline.prototype.getLatLngs = function () {};

/**
 * @param {Array<Array<L.LatLng>>} latlngs
 */
L.Polyline.prototype.setLatLngs = function (latlngs) {};

/**
 * @param {Array<L.LatLng>} latlngs
 * @param {Object=} options
 * @constructor
 * @extends {L.Polyline}
 */
L.Polygon = function(latlngs, options) {};

/**
 * @constructor
 * @extends {L.LayerGroup}
 */
L.FeatureGroup = function() {};
/**
 * @return {L.LatLngBounds}
 * @nosideeffects
 */
L.FeatureGroup.prototype.getBounds = function() {};

/**
 * @param {Object=} geojson
 * @param {Object=} options
 * @constructor
 * @extends {L.FeatureGroup}
 */
L.GeoJSON = function(geojson, options) {};
/**
 * @param {Object} geojson
 * @return this
 */
L.GeoJSON.prototype.addData = function(geojson) {};

L.GeoJSON.prototype.options = {
  /**
   * @param {Object} feature
   * @param {L.Marker|L.FeatureGroup|L.Polyline|L.Polygon} layer
   */
  onEachFeature(feature, layer) {}
};

/**
 * @param {string|Element} id
 * @param {Object=} options
 * @constructor
 * @extends {L.Class}
 * @implements ILayer
 */
L.Map = function (id, options) {};
/**
 * @param {Array<number>} center
 * @param {number} zoom
 * @return {L.Map}
 */
L.Map.prototype.setView = function(center, zoom) {};

/**
 * @return this
 */
L.Map.prototype.fitWorld = function() {};

/**
 * @return {number}
 */
L.Map.prototype.getZoom = function() {};

/**
 * @param {L.LayerGroup} layer
 */
L.Map.prototype.addLayer = function(layer) {};

/**
 * @param {L.LayerGroup} layer
 */
L.Map.prototype.removeLayer = function(layer) {};

/**
 * @param {string} event
 * @param {function()} handler
 */
L.Map.prototype.on = function(event, handler) {};


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
