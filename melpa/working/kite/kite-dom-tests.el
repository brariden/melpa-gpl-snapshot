;;; kite-dom-tests.el --- Kite test suite for DOM module
;; Copyright (C) 2012 Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
;; Keywords: tools
;; Package: kite
;; Compatibility: GNU Emacs 24

;; This file is not part of GNU Emacs.

;; Kite is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Kite is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Kite.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Kite test suite for DOM module.
;;
;; It is part of Kite, a WebKit inspector front-end.


;;; Code:


(defmacro with--kite-dom-test-buffer (&rest body)
  `(with-temp-buffer
     (let* ((kite-session (make-kite-session))
            (kite-active-sessions
             (let ((result (make-hash-table)))
               (puthash nil kite-session result)
               result))
            (inhibit-read-only t))
       (flet ((kite--find-buffer (websocket-url type) (current-buffer))
              (kite-send (&rest ignore)))
         (kite-dom-mode)
         ,@body))))

(defun kite--equal-wildcard (o1 o2)
  (or (eq '* o1)
      (eq '* o2)
      (eq o1 o2)
      (and (listp o1)
           (listp o2)
           (kite--equal-wildcard (car o1) (car o2))
           (kite--equal-wildcard (cdr o1) (cdr o2)))
      (and (stringp o1)
           (stringp o2)
           (string= o1 o2))
      (and (numberp o1)
           (numberp o2)
           (= o1 o2))))

(defconst simple-element
  '(:attributes []
                :children [(:attributes []
                                        :childNodeCount 1
                                        :nodeValue nil
                                        :localName "head"
                                        :nodeName "HEAD"
                                        :nodeType 1
                                        :nodeId 89
                                        :children [(:attributes
                                                    ["href" "foo"]
                                                    :childNodeCount 0
                                                    :nodeValue nil
                                                    :localName "link"
                                                    :nodeName "LINK"
                                                    :nodeType 1
                                                    :nodeId 87)]
                                        )
                           (:attributes
                            []
                            :childNodeCount 0
                            :nodeValue nil
                            :localName "body"
                            :nodeName "BODY"
                            :nodeType 1
                            :nodeId 90)]

                :childNodeCount 2
                :nodeValue nil
                :localName "html"
                :nodeName "HTML"
                :nodeType 1
                :nodeId 88))

(defconst single-element
  (list :attributes []
        :children []
        :childNodeCount 0
        :nodeValue nil
        :localName "html"
        :nodeName "HTML"
        :nodeType 1
        :nodeId 1))

(ert-deftest kite-test-dom-simple ()
  "A very simple HTML document is rendered correctly"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "  -<head>\n"
                     "     <link href=\"foo\"></link>\n"
                     "   </head>\n"
                     "   <body></body>\n"
                     " </html>")))))

(ert-deftest kite-test-dom-node-id ()
  "A node ID is associated with each character in the buffer"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (let ((index (point-min))
         (size (point-max)))
     (while (< index size)
       (should-not (equal (cons index nil)
                          (cons index (get-text-property index 'kite-node-id))))
       (setq index (1+ index))))))


(ert-deftest kite-test-dom-textnodes ()
  "A simple document with text nodes is rendered correctly"

  (with--kite-dom-test-buffer

   (let ((inhibit-read-only t))
     (kite--dom-insert-element
      (kite--dom-create-node
       '(:attributes
         []
         :childNodeCount 2
         :nodeValue nil
         :localName "dummy"
         :nodeName "DUMMY"
         :nodeType 1
         :nodeId 1
         :children [(:attributes
                     []
                     :childNodeCount 1
                     :nodeValue nil
                     :localName "child1"
                     :nodeName "child1"
                     :nodeType 1
                     :nodeId 2
                     :children [(:attributes
                                 []
                                 :childNodeCount 0
                                 :nodeValue "test"
                                 :nodeType 3
                                 :nodeId 3)])
                    (:attributes
                     []
                     :childNodeCount 1
                     :nodeValue nil
                     :localName "child2"
                     :nodeName "child2"
                     :nodeType 1
                     :nodeId 4
                     :children [(:attributes
                                 []
                                 :childNodeCount 0
                                 :nodeValue "foobar"
                                 :nodeType 3
                                 :nodeId 5)])])
       nil)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<dummy>\n"
                     "  -<child1>test</child1>\n"
                     "  -<child2>foobar</child2>\n"
                     " </dummy>")))))

(ert-deftest kite-test-dom-children-not-loaded ()
  "A node whose children aren't loaded is rendered correctly"

  (with--kite-dom-test-buffer

   (flet ((kite-send (command &rest keyword-args)
                     (should (string= command "DOM.requestChildNodes"))
                     (should (equal (plist-get keyword-args :params)
                                    '(:nodeId 2)))))

     (let ((inhibit-read-only t))
       (kite--dom-insert-element
        (kite--dom-create-node
         '(:attributes []
                       :childNodeCount 2
                       :nodeValue nil
                       :localName "empty"
                       :nodeName "EMPTY"
                       :nodeType 1
                       :nodeId 2)
         nil)))

     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      "+<empty>...</empty>"))

     (flet ((kite--dom-buffer (websocket-url) (current-buffer)))
       (kite--dom-DOM-setChildNodes nil
                                    '(:parentId
                                      2
                                      :nodes [(:attributes
                                               []
                                               :childNodeCount 0
                                               :nodeValue nil
                                               :localName "foo"
                                               :nodeName "FOO"
                                               :nodeType 1
                                               :nodeId 2)])))

     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      (concat "-<empty>\n"
                              "   <foo></foo>\n"
                              " </empty>"))))))


(ert-deftest kite-test-reset-child-nodes ()
  "Setting child nodes of a node that already has children works"

  (with--kite-dom-test-buffer

   (flet ((kite-send (command &rest keyword-args)
                     (should (string= command "DOM.requestChildNodes"))
                     (should (equal (plist-get keyword-args :params)
                                    '(:nodeId 2)))))

     (let ((inhibit-read-only t))
       (kite--dom-insert-element
        (kite--dom-create-node
         '(:attributes []
                       :childNodeCount 2
                       :nodeValue nil
                       :localName "empty"
                       :nodeName "EMPTY"
                       :nodeType 1
                       :nodeId 2)
         nil)))

     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      "+<empty>...</empty>"))

     (flet ((kite--dom-buffer (websocket-url) (current-buffer)))
       (kite--dom-DOM-setChildNodes nil
                                    '(:parentId
                                      2
                                      :nodes [(:attributes
                                               ["href" "foo"]
                                               :childNodeCount 0
                                               :nodeValue nil
                                               :localName "foo"
                                               :nodeName "FOO"
                                               :nodeType 1
                                               :nodeId 3)]))
       (kite--dom-DOM-attributeModified "dummy"
                                        '(:value "frobnicate" :name "foobar1" :nodeId 3))
       (kite--dom-DOM-attributeModified "dummy"
                                        '(:value "frobnicate" :name "foobar2" :nodeId 3))
       (kite--dom-DOM-setChildNodes nil
                                    '(:parentId
                                      2
                                      :nodes [(:attributes
                                               ["href" "bar"]
                                               :childNodeCount 0
                                               :nodeValue nil
                                               :localName "foo"
                                               :nodeName "FOO"
                                               :nodeType 1
                                               :nodeId 4)])))

     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      (concat "-<empty>\n"
                              "   <foo href=\"bar\"></foo>\n"
                              " </empty>"))))))


(ert-deftest kite-test-dom-insert-only ()
  "DOM is mutated correctly when node is inserted in front"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-childNodeInserted "dummy"
                                      '(:node
                                        (:attributes
                                         []
                                         :childNodeCount 0
                                         :nodeValue nil
                                         :localName "div"
                                         :nodeName "DIV"
                                         :nodeType 1
                                         :nodeId 100)
                                        :previousNodeId 0
                                        :parentNodeId 90))

     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      (concat
                       "-<html>\n"
                       "  -<head>\n"
                       "     <link href=\"foo\"></link>\n"
                       "   </head>\n"
                       "  -<body>\n"
                       "     <div></div>\n"
                       "   </body>\n"
                       " </html>"))))))

(ert-deftest kite-test-dom-insert-before ()
  "DOM is mutated correctly when node is inserted before another node"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-childNodeInserted "dummy"
                                      '(:node
                                        (:attributes
                                         []
                                         :childNodeCount 0
                                         :nodeValue nil
                                         :localName "div"
                                         :nodeName "DIV"
                                         :nodeType 1
                                         :nodeId 100)
                                        :previousNodeId 0
                                        :parentNodeId 89)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "  -<head>\n"
                     "     <div></div>\n"
                     "     <link href=\"foo\"></link>\n"
                     "   </head>\n"
                     "   <body></body>\n"
                     " </html>")))))

(ert-deftest kite-test-dom-insert-after ()
  "DOM is mutated correctly when node is inserted after another node"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-childNodeInserted "dummy"
                                      '(:node
                                        (:attributes
                                         []
                                         :childNodeCount 0
                                         :nodeValue nil
                                         :localName "div"
                                         :nodeName "DIV"
                                         :nodeType 1
                                         :nodeId 100)
                                        :previousNodeId 87
                                        :parentNodeId 89)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "  -<head>\n"
                     "     <link href=\"foo\"></link>\n"
                     "     <div></div>\n"
                     "   </head>\n"
                     "   <body></body>\n"
                     " </html>")))))

(ert-deftest kite-test-dom-remove ()
  "DOM is mutated correctly when node is removed"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-childNodeRemoved "dummy"
                                     '(:nodeId 89 :parentNodeId 88)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "   <body></body>\n"
                     " </html>")))))

(ert-deftest kite-test-dom-modify-attribute ()
  "DOM is mutated correctly when attribute is modified"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-attributeModified "dummy"
                                      '(:value "frobnicate" :name "href" :nodeId 87)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "  -<head>\n"
                     "     <link href=\"frobnicate\"></link>\n"
                     "   </head>\n"
                     "   <body></body>\n"
                     " </html>")))
   (let* ((node-info (gethash 87 (kite-session-dom-nodes kite-session)))
          (attr-info (cdr (assoc "href" (kite-dom-node-attr-alist node-info)))))

     (should (eq (length "\"frobnicate\"")
                 (- (marker-position (kite-dom-attr-value-end attr-info))
                    (marker-position (kite-dom-attr-value-begin attr-info))))))))

(ert-deftest kite-test-dom-add-attribute ()
  "DOM is mutated correctly when attribute is modified"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-attributeModified "dummy"
                                      '(:value "bar" :name "baz" :nodeId 87)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "  -<head>\n"
                     "     <link href=\"foo\" baz=\"bar\"></link>\n"
                     "   </head>\n"
                     "   <body></body>\n"
                     " </html>")))

   (let* ((node-info (gethash 87 (kite-session-dom-nodes kite-session)))
          (attr-info (cdr (assoc "baz" (kite-dom-node-attr-alist node-info)))))
     (should (not (null attr-info))))))

(ert-deftest kite-test-dom-add-first-attribute ()
  "DOM is mutated correctly when first attribute is added"
  (with--kite-dom-test-buffer
   (kite--dom-insert-element
    (kite--dom-create-node single-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))
     (kite--dom-DOM-attributeModified "dummy"
                                      '(:value "bar" :name "baz" :nodeId 1)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    " <html baz=\"bar\"></html>"))))

(ert-deftest kite-test-set-child-count ()
  "DOM is mutated correctly when child count is modified"

  (with--kite-dom-test-buffer
   (kite--dom-insert-element
    (kite--dom-create-node single-element nil))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    " <html></html>"))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))
     (kite--dom-DOM-childNodeCountUpdated
      "dummy"
      '(:nodeId 1 :childNodeCount 1))

     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      "+<html>...</html>"))

     (kite--dom-DOM-childNodeCountUpdated
      "dummy"
      '(:nodeId 1 :childNodeCount 0)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    " <html></html>"))))

(ert-deftest kite-test-dom-remove-attribute ()
  "DOM is mutated correctly when attribute is removed"

  (with--kite-dom-test-buffer

   (kite--dom-insert-element
    (kite--dom-create-node simple-element nil))

   (flet ((kite--dom-buffer (websocket-url) (current-buffer)))

     (kite--dom-DOM-attributeRemoved "dummy"
                                     '(:name "href" :nodeId 87)))

   (should (string= (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    (concat
                     "-<html>\n"
                     "  -<head>\n"
                     "     <link></link>\n"
                     "   </head>\n"
                     "   <body></body>\n"
                     " </html>")))

   (let* ((node-info (gethash 87 (kite-session-dom-nodes kite-session)))
          (attr-info (cdr (assq 'href (kite-dom-node-attr-alist node-info)))))
     (should (null attr-info)))))

(ert-deftest kite-test-rgba ()
  "kite--rgba works as intended"

  (should (equal (kite--rgba 1 2 3 4)
                 '((r . 1)
                   (g . 2)
                   (b . 3)
                   (a . 4))))

  (should (equal (kite--rgba 4 3 2 1)
                 '((r . 4)
                   (g . 3)
                   (b . 2)
                   (a . 1)))))

(ert-deftest kite-dimmed-face-foreground ()
  "kite--dimmed-face-foreground works as intended"
  (unwind-protect
      (progn
        (defface ert-kite-test-face
          '((t :background "#111111"
               :foreground "#333333"))
          "test face 1")

        (should (equal
                 (color-name-to-rgb
                  (kite--dimmed-face-foreground
                   'ert-kite-test-face
                   0.5))

                 (mapcar* (lambda (x y)
                            (/ (+ x y ) 2))
                          (color-name-to-rgb "#333333")
                          (color-name-to-rgb "#111111")))))

    (put 'ert-kite-test-face 'face-defface-spec nil)))

(ert-deftest kite-dom-hide-highlight ()
  "kite-dom-hide-highlight sends message to server"
  (flet ((kite-send (command &optional params callback)
                    (should (string= command "DOM.hideHighlight"))))
    (kite-dom-hide-highlight)))

(ert-deftest kite-dom-highlight-node ()
  "kite-dom-hide-highlight sends message to server"

  (with-temp-buffer
    (let ((kite-session (make-kite-session)))
      (flet ((kite-send (&rest ignore)))
        (kite-dom-mode))
      (let ((inhibit-read-only t))
        (kite--dom-insert-element
         (kite--dom-create-node simple-element nil)))
      (flet ((kite-send (command &rest keyword-args)
                        (should (string= command "DOM.highlightNode"))
                        (should (kite--equal-wildcard
                                 (plist-get keyword-args :params)
                                 '(:nodeId
                                   88
                                   :highlightConfig
                                   ((showInfo . nil)
                                    (contentColor . ((r . *) (g . *) (b . *) (a . *)))
                                    (paddingColor . ((r . *) (g . *) (b . *) (a . *)))
                                    (borderColor . ((r . *) (g . *) (b . *) (a . *)))
                                    (marginColor . ((r . *) (g . *) (b . *) (a . *)))))))))
        (goto-char (point-min))
        (kite-dom-highlight-node)))))

(ert-deftest kite-test-dom-inspect ()
  (let (sent-packets
        (kite-session (make-kite-session)))
    (with-temp-buffer
      (flet ((kite--dom-buffer (websocket-url) (current-buffer))
             (kite-send (command &rest keyword-args)
                        (setq sent-packets (cons (list command
                                                       (plist-get keyword-args :params)
                                                       (plist-get keyword-args :success-function))
                                                 sent-packets))))
        (kite-dom-mode)))
    (should (kite--equal-wildcard sent-packets
                                  '(("CSS.enable" nil *))))))

(defun kite-run-tests ()
  (interactive)
  (ert-run-tests-interactively "^kite-"))

(defun kite-run-coverage ()
  (interactive)
  (testcover-start "kite-dom.el")
  (ert-run-tests-interactively "^kite-"))

(provide 'kite-dom-tests)

;;; kite-dom-tests.el ends here
