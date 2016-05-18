function main(a, b) {
  return a == null;
}

function foo () {
}

foo();

// check that the following jshint errors displayed.
//   test.js: line 2, col 12, Use '===' to compare with 'null'.
//   test.js: line 1, col 14, 'main' is defined but never used.
//   test.js: line 1, col 19, 'b' is defined but never used.

// check that the following gjslint errors displayed.
//   Line 5, E:0001: Extra space after "foo"
