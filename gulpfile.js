var gulp       = require('gulp');
var purescript = require('gulp-purescript');
var foreach    = require('gulp-foreach');
var mocha      = require('gulp-mocha');

var path       = require('path');

var bowerPurs = 'bower_components/purescript-*/src/**/*.purs';
var bowerFfi = 'bower_components/purescript-*/src/**/*.js';
var sources = [bowerPurs, 'src/**/*.purs'];
var ffi = [bowerFfi, 'src/**/*.js'];

gulp.task('psc', function(){
  return purescript.psc({
    src: sources,
    ffi: ffi
  });
});

gulp.task('compile', ['psc'], function() {
  return purescript.pscBundle({
    src: 'output/**/*.js',
    output: 'app/examples.js',
    module: [ 'Data.JSON']
  });
});

gulp.task('dotPsci', function(){
  return purescript.psci({
    src: sources,
    ffi: ffi
  });
});

gulp.task('pscDocs', function(){
  return gulp
    .src('src/**/*.purs')
    .pipe(foreach(function(stream, file){
      var p = path.resolve(
        'docs',
        path.dirname(file.relative),
        path.basename(file.relative, ".purs") + ".md")
      return stream
        .pipe(purescript.pscDocs())
        .pipe(gulp.dest(p));
    }));
});

gulp.task('buildtest', function(){
  return purescript.psc({
    src: sources.concat('tests/Test.purs'),
    ffi: ffi
  });
});

gulp.task('test', ["buildtest"], function(){
  purescript.pscBundle({
    src: 'output/**/*.js',
    output: 'tmp/test.js',
    module: [ 'Test.Main'],
    main: 'Test.Main'
  });
  return gulp.src('tmp/test.js')
    .pipe(mocha());
});

gulp.task('default', ['compile', 'dotPsci', 'test']);
