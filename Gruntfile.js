module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],

    clean: {
      tests: ["tmp"],
      lib:   ["output"]
    },

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "docs/Module.md"
        }
    },

    psc: {
      tests: {
        options: {
          modules: ["Main"],
          main: true
        },
        src: ["tests/Test.purs", "<%=libFiles%>"],
        dest: "tmp/tests.js"
      },
      example_simple: {
         options: {
          modules: ["Main"],
          main: true
        },
        src: ["examples/Simple.purs", "<%=libFiles%>"],
        dest: "tmp/example_simple.js"
      },
      example_complex: {
         options: {
          modules: ["Main"],
          main: true
        },
        src: ["examples/Complex.purs", "<%=libFiles%>"],
        dest: "tmp/example_complex.js"
      }

    },

    execute: {
      tests: {
        src: "tmp/tests.js"
      },
      example_simple: {
        src: "tmp/example_simple.js"
      },
      example_complex: {
        src: "tmp/example_complex.js"
      }
    }

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test", ["clean:tests", "psc:tests", "execute:tests"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("example_simple",  ["psc:example_simple",  "execute:example_simple"]);
  grunt.registerTask("example_complex", ["psc:example_complex", "execute:example_complex"]);
  grunt.registerTask("default", ["test", "make"]);
};
