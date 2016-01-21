function main(a, b) {
  return a == null;
}

function foo () {
}

foo();

// Check that error is not found, since flymake-gjshint was disabled
// by Directory Variables in .dir-locals.el.
//
// For information about Directory Variables, see the following URL.
// http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
