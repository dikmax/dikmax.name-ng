/* eslint-disable */

var d3 = {
  'version': "3.5.16",
  'behavior': {},
  'geo': {
    'graticule': {}
  }
};

/**
 * @constructor
 * @struct
 */
var d3Selection = function () {};

/**
 * @param {string} name
 * @param {*=} opt_value
 * @return {*}
 */
d3Selection.prototype.property = function(name, opt_value) {};

/**
 * @constructor
 * @struct
 */
var d3ZoomBehavior = function() {};

/**
 * @param {Array<number>} scaleExtent
 * @return {!d3ZoomBehavior}
 */
d3ZoomBehavior.prototype.scaleExtent = function(scaleExtent) {};
/**
 * @param {Array<number>} size
 * @return {!d3ZoomBehavior}
 */
d3ZoomBehavior.prototype.size = function(size) {};
/**
 * @param {!string} event
 * @param {Function} handler
 * @return {!d3ZoomBehavior}
 */
d3ZoomBehavior.prototype.on = function(event, handler) {};

/**
 * @struct
 * @constructor
 */
var d3GeoGraticule = function () {};

/**
 * @param {Array<number>} arr
 * @return {d3GeoGraticule}
 */
d3GeoGraticule.prototype.minorStep = function (arr) {};

/**
 * @struct
 * @constructor
 */
var d3GeoPath = function () {};

/**
 * @param {Object} projection
 * @return {d3GeoPath}
 */
d3GeoPath.prototype.projection = function(projection) {};

/**
 * @param {Node|string} node
 * @returns d3Selection
 */
d3.select = function(node) {};

/**
 * @return {!d3ZoomBehavior}
 */
d3.behavior.zoom = function() {};

/**
 * @return {d3GeoGraticule}
 */
d3.geo.graticule = function () {};
