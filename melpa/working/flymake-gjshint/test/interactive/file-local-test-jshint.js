function main(a, b) {
  return a == null;
}

function foo () {
}

foo();

// Check that error is not found, since flymake-gjshint was disabled
// by File Variables.
//
// For information about File Variables, see the following URL.
// http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html#File-Variables

/* Local Variables:         */
/* flymake-gjshint: jshint */
/* End:                     */
