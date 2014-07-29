var fs = require('fs');


fs.readFile('./header.md', 'utf8', function(err, hdr){
  if ( err ) throw err;
  fs.readFile('./tmp/documentation.md', 'utf8', function(err, doc){
    if ( err ) throw err;
    fs.writeFile('README.md', hdr + doc, function(err){
      if ( err ) throw err;
      return 0;
    });
  });
});
