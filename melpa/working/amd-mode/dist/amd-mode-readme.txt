amd-mode provides convenience methods and keybindings for handling
AMD module definitions.

C-c C-d k: `amd-kill-buffer-path': Kill the path of the buffer's
file without its extension.

C-c C-d s: `amd-search-references': Search for modules that require
the buffer's file.

C-c C-d a: `amd-add-dependency': Prompt for a file to add as a
dependency.

C-c C-d o: `amd-find-module-at-point': Find a module named after
the node at point.

C-c C-d i: `amd-auto-insert': Insert an empty module definition.
