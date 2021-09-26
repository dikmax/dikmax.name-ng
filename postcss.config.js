/* eslint-disable global-require,import/no-extraneous-dependencies */
/* eslint-env node */

const browsers = '> 2%';

module.exports = {
  plugins: [
    require('postcss-import')({}),
    require('postcss-mixins')({}),
    require('postcss-custom-properties')({
      preserve: false,
    }),
    require('postcss-custom-media')({}),
    require('postcss-preset-env')({
      browsers,
    }),
    require('colorguard')({
      'ignore': ['#000000', '#ffffff', '#212121', '#b3b3b3']
    }),
    require('postcss-quantity-queries')({}),
    require('postcss-nested')({}),
    require('postcss-responsive-type')({}),
    require('postcss-short')({
      'font-size': {
        'disable': true
      }
    }),
    require('postcss-color-mod-function')({
      transformVars: true,
    }),
    require('stylehacks')({
      browsers
    }),

    // Minify
    require('postcss-discard-comments')({}),
    require('cssnano')({
      'autoprefixer': false,
      'zindex': false
    }),
  ]
};
