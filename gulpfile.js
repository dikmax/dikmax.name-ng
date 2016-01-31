var gulp = require('gulp');
var rename = require('gulp-rename');
var postcss = require('gulp-postcss');
var import_ = require('postcss-import');
var mixins = require('postcss-mixins');
var cssnext = require('postcss-cssnext');
var short = require('postcss-short');

var lost = require('lost');
var responsiveType = require('postcss-responsive-type');
var colorguard = require('colorguard');
var normalize = require('postcss-normalize');
var reporter = require('postcss-reporter');
var quantitiesQueries = require('postcss-quantity-queries');

// Minifiers
var discardEmpty = require('postcss-discard-empty');
var discardDuplicates = require('postcss-discard-duplicates');
var calc = require('postcss-calc');
var minifySelectors = require('postcss-minify-selectors');
var mergeLonghand = require('postcss-merge-longhand');
var colormin = require('postcss-colormin');
var mergeRules = require('postcss-merge-rules');
var zIndex = require('postcss-zindex');

var cssnano = require('cssnano');

var paths = {
    styles: 'styles/*.pcss'
};

gulp.task('styles', function () {
    var processors = [
        import_(),
        mixins(),
        cssnext({
            "browsers": "> 5%",
            "compress": false
        }),
        short(),
        lost(),
        responsiveType(),
        quantitiesQueries(),
        //colorguard(),
        normalize(),

        /*discardEmpty(),
        discardDuplicates(),
        calc(),
        minifySelectors(),
        mergeLonghand(),
        colormin(),
        mergeRules(),
        zIndex(),*/

        //cssnano(),

        reporter()
    ];
    return gulp.src('styles/styles.pcss')
        .pipe(postcss(processors))
        .pipe(rename({
            extname: '.css'
        }))
        .pipe(gulp.dest('css'));
});

gulp.task('watch', function() {
    gulp.watch(paths.styles, ['styles']);
});

gulp.task('default', ['watch', 'styles']);
