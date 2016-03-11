var gulp = require('gulp');
var closureCompiler = require('gulp-closure-compiler');

gulp.task('default', function() {
    return gulp.src(['scripts/main.js', 'scripts/bower_components/google-closure-library/closure/goog/**/*.js'])
        .pipe(closureCompiler({
            compilerPath: 'node_modules/google-closure-compiler/compiler.jar',
            fileName: 'build.js',
            compilerFlags: {
                entry_point: 'app.main',
                compilation_level: 'ADVANCED_OPTIMIZATIONS',
                only_closure_dependencies: true,
                warning_level: 'VERBOSE',
                formatting: 'PRETTY_PRINT'
            }
        }))
        .pipe(gulp.dest('_build/scripts'));
});