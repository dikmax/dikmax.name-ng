goog.provide('dikmax.Graticule');
goog.provide('dikmax.graticule');

goog.require('goog.object');

/* global L */

/**
 * @param {number} lng
 * @returns {number}
 */
function lngFix(lng) {
  if (lng >= 180) return 179.999999;
  if (lng <= -180) return -179.999999;
  return lng;
}

/**
 * @param {number} lat
 * @returns {Array<number>}
 * @private
 */
function getParallel(lat) {
  const coords = [];
  for (let lng = -180; lng <= 180; lng += 1) {
    coords.push([lngFix(lng), lat]);
  }
  return coords;
}

/**
 * @param {number} lng
 * @returns {Array<number>}
 * @private
 */
function getMeridian(lng) {
  const longitude = lngFix(lng);
  const coords = [];
  for (let lat = -90; lat <= 90; lat += 1) {
    coords.push([longitude, lat]);
  }
  return coords;
}

/**
 * @return {Object}
 * @private
 */
function getFrame() {
  return {
    'type': 'Polygon',
    'coordinates': [
      getMeridian(-180).concat(getMeridian(180).reverse())
    ]
  };
}

/**
 * @param {Array<number>} coords
 * @param {Object} prop
 * @returns {Object}
 * @private
 */
function getFeature(coords, prop) {
  return {
    'type': 'Feature',
    'geometry': {
      'type': 'LineString',
      'coordinates': coords
    },
    'properties': prop
  };
}

/**
 * @param {number} interval
 * @returns {Object}
 * @private
 */
function getGraticule(interval) {
  const features = [];

  // Meridians
  for (let lng = 0; lng <= 180; lng += interval) {
    features.push(getFeature(getMeridian(lng), {
      'name': (lng) ? `${lng.toString()}° E` : 'Prime meridian'
    }));
    if (lng !== 0) {
      features.push(getFeature(getMeridian(-lng), {
        'name': `${lng.toString()}° W`
      }));
    }
  }

  // Parallels
  for (let lat = 0; lat <= 90; lat += interval) {
    features.push(getFeature(getParallel(lat), {
      'name': (lat) ? `${lat.toString()}° N` : 'Equator'
    }));
    if (lat !== 0) {
      features.push(getFeature(getParallel(-lat), {
        'name': `${lat.toString()}° S`
      }));
    }
  }

  return {
    'type': 'FeatureCollection',
    'features': features
  };
}


/*
 Graticule plugin for Leaflet powered maps.
 based on https://github.com/ablakey/Leaflet.SimpleGraticule
 */

dikmax.Graticule = class extends L.GeoJSON {

  /**
   * @param {Object=} opts
   */
  constructor(opts) {
    const options = {
      style: {
        color: '#333',
        weight: 1
      },
      interval: 20,
      sphere: false
    };
    goog.object.extend(options, opts);

    super(null, options);

    if (this.options.sphere) {
      this.addData(getFrame());
    } else {
      this.addData(getGraticule(this.options.interval));
    }
  }
};

/**
 * @param {Object=} options
 * @returns {dikmax.Graticule}
 */
dikmax.graticule = function (options) {
  return new dikmax.Graticule(options);
};
