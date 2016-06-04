/* eslint-disable */

var L = {};

/**
 * @constructor
 */
L.Class = function () {};

/**
 * @extends {L.Class}
 * @constructor
 */
L.LayerGroup = function () {};

/**
 * @param {L.Map} map
 * @return this
 */
L.LayerGroup.prototype.addTo = function (map) {};

/**
 * @extends {L.LayerGroup}
 * @constructor
 */
L.FeatureGroup = function () {};

/**
 * @param {Object=} geojson
 * @param {Object=} options
 * @extends {L.FeatureGroup}
 * @constructor
 */
L.GeoJSON = function (geojson, options) {};

/**
 * @param {Object} geojson
 * @return this
 */
L.GeoJSON.prototype.addData = function (geojson) {};

/**
 * @param {string|Element} id
 * @param {Object=} options
 * @extends {L.Class}
 * @constructor
 */
L.Map = function (id, options) {};

/**
 * @param {Array<number>} center
 * @param {number} zoom
 * @return {L.Map}
 */
L.Map.prototype.setView = function (center, zoom) {};

/**
 * @param {string|Element} id
 * @param {Object=} options
 * @return {L.Map}
 */
L.map = function (id, options) {};
