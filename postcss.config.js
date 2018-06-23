/* eslint-disable global-require,import/no-extraneous-dependencies */
/* eslint-env node */

const browsers = '> 2%';

module.exports = {
  plugins: [
    require('postcss-import')({}),
    require('postcss-mixins')({}),
    require('postcss-preset-env')({
      browsers,
      features: {
        'custom-properties': {
          preserve: false
        }
      }
    }),
    require('colorguard')({
      'ignore': ['#000000', '#ffffff', '#212121', '#b3b3b3']
    }),
    require('postcss-quantity-queries')({}),
    require('postcss-nested')({}),
    require('postcss-custom-media')({}),
    require('lost')({}),
    require('postcss-responsive-type')({}),
    require('postcss-short')({
      'font-size': {
        'disable': true
      }
    }),
    require('postcss-color-function')({}),
    require('stylehacks')({
      browsers
    }),

    // Minify
    require('postcss-discard-comments')({}),
    require('css-mqpacker')({}),
    require('cssnano')({
      'autoprefixer': false,
      'zindex': false
    }),
  ]
};
