(require 'f)

(defvar pillar-support-path
  (f-dirname load-file-name))

(defvar pillar-features-path
  (f-parent pillar-support-path))

(defvar pillar-root-path
  (f-parent pillar-features-path))


(add-to-list 'load-path pillar-root-path)

(require 'pillar)
(require 'espuds)
(require 'ert)

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
