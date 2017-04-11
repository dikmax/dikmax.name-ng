goog.provide('dikmax.Graticule');
goog.provide('dikmax.graticule');

goog.require('goog.object');

/* global L */

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
      this.addData(this._getFrame());
    } else {
      this.addData(this._getGraticule());
    }
  }

  /**
   * @return {Object}
   * @private
     */
  _getFrame() {
    return {
      'type': 'Polygon',
      'coordinates': [
        this._getMeridian(-180).concat(this._getMeridian(180).reverse())
      ]
    };
  }

  /**
   * @returns {Object}
   * @private
     */
  _getGraticule() {
    const features = [];
    const interval = this.options.interval;

    // Meridians
    for (let lng = 0; lng <= 180; lng = lng + interval) {
      features.push(this._getFeature(this._getMeridian(lng), {
        'name': (lng) ? `${lng.toString()}° E` : 'Prime meridian'
      }));
      if (lng !== 0) {
        features.push(this._getFeature(this._getMeridian(-lng), {
          'name': `${lng.toString()}° W`
        }));
      }
    }

    // Parallels
    for (let lat = 0; lat <= 90; lat = lat + interval) {
      features.push(this._getFeature(this._getParallel(lat), {
        'name': (lat) ? `${lat.toString()}° N` : 'Equator'
      }));
      if (lat !== 0) {
        features.push(this._getFeature(this._getParallel(-lat), {
          'name': `${lat.toString()}° S`
        }));
      }
    }

    return {
      'type': 'FeatureCollection',
      'features': features
    };
  }

  /**
   * @param {number} lng
   * @returns {Array<number>}
   * @private
     */
  _getMeridian(lng) {
    const longitude = this._lngFix(lng);
    const coords = [];
    for (let lat = -90; lat <= 90; lat++) {
      coords.push([longitude, lat]);
    }
    return coords;
  }

  /**
   * @param {number} lat
   * @returns {Array<number>}
   * @private
     */
  _getParallel(lat) {
    const coords = [];
    for (let lng = -180; lng <= 180; lng++) {
      coords.push([this._lngFix(lng), lat]);
    }
    return coords;
  }

  /**
   * @param {Array<number>} coords
   * @param {Object} prop
   * @returns {Object}
   * @private
   */
  _getFeature(coords, prop) {
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
   * @param {number} lng
   * @returns {number}
   * @private
   */
  _lngFix(lng) {
    if (lng >= 180) return 179.999999;
    if (lng <= -180) return -179.999999;
    return lng;
  }
};

/**
 * @param {Object=} options
 * @returns {dikmax.Graticule}
 */
dikmax.graticule = function (options) {
  return new dikmax.Graticule(options);
};
