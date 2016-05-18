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

// For information about File Variables, see the following URL.
// http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html#File-Variables

/* Local Variables:        */
/* flymake-gjshint: nil    */
/* End:                    */
