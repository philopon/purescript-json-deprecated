  /* global exports */
"use strict";

// module Data.JSON

exports.jsonParseImpl = function jsonParseImpl(left, right, string) {
    try       { return right(JSON.parse(string)); }
    catch (e) { return left(e.toString()); }
};

exports.jsonToValueImpl = function jsonToValueImpl(auxes, ctors) {
    var left   = auxes.left;
    var right  = auxes.right;
    var either = auxes.either;
    var insert = auxes.insert;
    var empty  = auxes.empty;
    var Null   = ctors.null;
    var Number = ctors.number;
    var Int    = ctors.int;
    var Bool   = ctors.bool;
    var String = ctors.string;
    var Array  = ctors.array;
    var Object = ctors.object;
    var parse  = function(json) {
        var typ = Object.prototype.toString.call(json).slice(8,-1);
        if (typ === 'Number') {
            return right((json | 0) === json ? Int(json) : Number(json));
        } else if (typ === 'Boolean') {
            return right(Bool(json));
        } else if (typ === 'String') {
            return right(String(json));
        } else if (typ === 'Null') {
            return right(Null);
        } else if (typ === 'Array') {
            var ary = [];
            for(var i = 0; i < json.length; i++) {
                either
                    (function(l){return left(l)})
                    (function(r){ary.push(r)})
                    (parse(json[i]))
            }
            return right(Array(ary));
        } else if (typ === 'Object') {
            var obj = empty;
            for(var k in json) {
                either
                    (function(l){return left(l)})
                    (function(r){obj = insert(k)(r)(obj)})
                    (parse(json[k]));
            }
            return right(Object(obj));
        } else {
            return left('unknown type: ' + typ);
        }
   };
   return parse;
};

exports.jsNull = null;

exports.unsafeCoerce = function unsafeCoerce(a) {
  return a;
};

exports.objToHash = function objToHash(valueToJSONImpl, fst, snd, obj) {
    var hash = {};
    for(var i = 0; i < obj.length; i++) {
        hash[fst(obj[i])] = valueToJSONImpl(snd(obj[i]));
    }
    return hash;
};

exports.jsonStringify = function jsonStringify(json) {
  return JSON.stringify(json);
};
