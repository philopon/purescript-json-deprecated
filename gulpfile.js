var gulp       = require('gulp');
var purescript = require('gulp-purescript');
var mocha      = require('gulp-mocha');
var merge      = require('merge-stream');

var path       = require('path');

var sources =
    [ 'bower_components/purescript-*/src/**/*.purs'
    , 'src/**/*.purs'
    , 'tests/**/*.purs'
    , 'examples/**/*.purs'
    ];

var ffi =
    [ 'bower_components/purescript-*/src/**/*.js'
    , 'src/**/*.js'
    ];

gulp.task('psc', function(){
  return purescript.psc({
    src: sources,
    ffi: ffi
  });
});

gulp.task('psci', function(){
  return purescript.psci({
    src: sources,
    ffi: ffi
  });
});

gulp.task('pscDocs', function(){
    return purescript.pscDocs({
      src: sources,
      docgen: 'Data.JSON',
    }).pipe(gulp.dest('docs.md'));
});

gulp.task('test', ['psc'], function(){
  return purescript.pscBundle({
    src: 'output/**/*.js',
    module: 'Test.Main',
    main: 'Test.Main'
  }).pipe(gulp.dest('tmp/test.js'))
  .pipe(mocha());
});

gulp.task('examples', ['psc'], function(){
  return merge(['Simple', 'Complex'].map(function(example){
    return purescript.pscBundle({
      src: 'output/**/*.js',
      module: 'Examples.' + example,
      main: 'Examples.' + example
    }).pipe(gulp.dest('examples/' + example + '.js'));
  }));
});

gulp.task('default', ['compile', 'dotPsci', 'test']);
