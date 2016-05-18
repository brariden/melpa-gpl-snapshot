function main(a, b) {
  return a == null;
}

function foo () {
}

foo();

// check that the following gjslint errors displayed.
//   Line 5, E:0001: Extra space after "foo"

// For information about File Variables, see the following URL.
// http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html#File-Variables

/* Local Variables:         */
/* flymake-gjshint: gjslint */
/* End:                     */
