(require 'doom)

(when (file-readable-p "sample.xml")
  (message "Running tests")
  (let ((data (with-temp-buffer
                (insert-file-contents "sample.xml")
                (libxml-parse-xml-region (point-min) (point-max)))))
    
    (cl-assert (fboundp 'doom-node-name))
    (cl-assert (fboundp 'doom-document-name))
    (cl-assert (fboundp 'doom-element-name))
    (cl-assert (fboundp 'doom-attr-name))

    (let ((attr (doom-make-attribute-from-xml
                 (car (xml-node-attributes data)) 'none 'none)))
      (cl-assert (string= "id" (doom-node-name attr)))
      (cl-assert (string= "compiler" (doom-node-value attr)))
      (cl-assert (eq doom-attribute-node (doom-node-type attr))))

    (let ((element (doom-make-node-from-xml data 'no-owner)))
      (cl-assert (string= "book" (doom-node-name element)))
      (cl-assert (string= "id" (doom-node-name
                                (car (doom-node-attributes element)))))
      (cl-assert (string= "compiler"
                          (doom-node-value
                           (car (doom-node-attributes element)))))
      (cl-assert (string= "bookinfo"
                          (doom-node-name 
                           (first (doom-node-child-nodes element)))))
      (cl-assert (string= "chapter"
                          (doom-node-name
                           (second (doom-node-child-nodes element)))))
      (let ((title (first
                    (doom-node-child-nodes
                     (first
                      (doom-node-child-nodes
                       (first
                        (doom-node-child-nodes element))))))))
        (cl-assert (eq 'title (doom-node-name title)))
        (cl-assert (string= "My own book!"
                            (doom-node-value
                             (first (doom-node-child-nodes title)))))))

    (let ((doc (doom-make-document-from-xml data)))
      (cl-assert (eq doom-document-node-name (doom-document-name doc)))
      (cl-assert (string= "book" (doom-node-name (doom-document-element doc))))
      (cl-assert (eq (doom-node-parent-node
                      (first (doom-node-child-nodes (doom-document-element doc))))
                     (doom-document-element doc)))
      (cl-assert (eq (first (doom-node-child-nodes (doom-document-element doc)))
                     (doom-node-first-child (doom-document-element doc))))
      (cl-assert (eq (doom-node-next-sibling
                      (first (doom-node-child-nodes (doom-document-element doc))))
                     (second (doom-node-child-nodes (doom-document-element doc)))))
      (cl-assert (eq doc
                     (doom-node-owner-document
                      (doom-node-first-child (doom-document-element doc)))))
      (cl-assert (string= "chapter"
                          (doom-node-name
                           (doom-element-last-child
                            (doom-document-element doc)))))
      (cl-assert (eq nil (doom-node-previous-sibling (doom-document-element doc)))))

    (cl-assert (eq 3 (doom-node-list-length '(1 2 3))))

    (cl-assert (eq 2 (doom-node-list-item '(1 2 3) 1)))

    (let ((doc (doom-make-document-from-xml data)))
      (cl-assert (equal (mapcar 'doom-node-name
                                (doom-document-get-elements-by-tag-name
                                 doc '*))
                        '(book bookinfo bookbiblio title \#text edition
                               \#text authorgroup author firstname \#text
                               surname \#text chapter title \#text para
                               \#text)))
      (cl-assert (equal (mapcar 'doom-node-name
                                (doom-document-get-elements-by-tag-name
                                 doc 'title))
                        '(title title)))
      (cl-assert (equal (mapcar 'doom-node-name
                                (doom-element-get-elements-by-tag-name
                                 (doom-document-element doc) 'title))
                        '(title title)))
      (cl-assert (equal (mapcar (lambda (element)
                                  (doom-node-value
                                   (doom-element-first-child element)))
                                (doom-document-get-elements-by-tag-name
                                 doc 'title))
                        '("My own book!" "A very small chapter"))))

    (let* ((doc (doom-make-document-from-xml data))
           (ancestor (doom-document-element doc))
           (child (car (doom-document-get-elements-by-tag-name doc 'title))))
      (cl-assert (doom-node-ancestor-p child ancestor)))

    (let* ((doc (doom-make-document-from-xml data))
           (book (doom-document-element doc))
           (old-chapter (doom-element-last-child book))
           (new-chapter (doom-document-create-element doc 'chapter)))
      (cl-assert (string= (doom-node-name
                           (doom-element-append-child book new-chapter))
                          "chapter"))
      (cl-assert (equal (mapcar 'doom-element-name
                                (doom-element-child-nodes book))
                        '(bookinfo chapter chapter)))
      (cl-assert (eq (doom-element-last-child book) new-chapter))
      (cl-assert (not (eq (doom-element-last-child book) old-chapter)))
      (cl-assert (eq (doom-element-next-sibling old-chapter) new-chapter))
      (cl-assert (eq (doom-element-previous-sibling new-chapter) old-chapter))
      (cl-assert (eq (doom-element-parent-node new-chapter) book))
      (cl-assert (doom-node-ancestor-p new-chapter book))
      (cl-assert (not (eq t (condition-case var
                                (doom-element-append-child book new-chapter)
                              ('doom-hierarchy-request-err
                               t)))))
      (cl-assert (eq t (condition-case var
                           (doom-element-append-child new-chapter book)
                         ('doom-hierarchy-request-err
                          t)))))

    (let* ((doc (doom-make-document-from-xml data))
           (book (doom-document-element doc))
           (old-chapter (doom-element-last-child book))
           (new-chapter (doom-document-create-element doc 'chapter))
           (new-title (doom-document-create-element doc 'title))
           (text (doom-document-create-text-node doc "Test Chapter")))
      (cl-assert (eq text (doom-element-append-child
                           (doom-element-append-child
                            (doom-element-append-child book new-chapter)
                            new-title)
                           text)))
      (cl-assert (= 2 (length (doom-node-child-nodes old-chapter))))
      (cl-assert (= 1 (length (doom-node-child-nodes new-chapter))))
      (cl-assert (string= "title" (doom-node-name
                                   (car (doom-node-child-nodes new-chapter)))))
      (cl-assert (eq (car (doom-node-child-nodes new-chapter))
                     (doom-node-first-child new-chapter)))
      (cl-assert (eq new-title
                     (doom-node-first-child new-chapter)))
      (cl-assert (eq text
                     (doom-node-first-child new-title)))
      (cl-assert (equal
                  (mapcar (lambda (node)
                            (doom-node-value
                             (doom-node-first-child node)))
                          (doom-document-get-elements-by-tag-name doc 'title))
                  '("My own book!" "A very small chapter" "Test Chapter"))))

    (let* ((doc (doom-make-document-from-xml data))
           (book (doom-document-element doc))
           (copy (doom-node-clone-node book)))
      (cl-assert (not (eq book copy)))
      (cl-assert (eq (doom-node-child-nodes book)
                     (doom-node-child-nodes copy)))
      (cl-assert (eq (car (doom-node-child-nodes book))
                     (car (doom-node-child-nodes copy))))
      (cl-assert (eq (doom-node-first-child book)
                     (doom-node-first-child copy)))
      (cl-assert (eq (doom-node-last-child book)
                     (doom-node-last-child copy)))
      (cl-assert (not (eq (doom-node-attributes book)
                          (doom-node-attributes copy))))
      (cl-assert (eq (doom-node-name (car (doom-node-attributes book)))
                     (doom-node-name (car (doom-node-attributes copy)))))
      (cl-assert (not (eq (doom-node-value (car (doom-node-attributes book)))
                          (doom-node-value (car (doom-node-attributes copy))))))
      (cl-assert (equal (doom-node-value (car (doom-node-attributes book)))
                        (doom-node-value (car (doom-node-attributes copy))))))

    (let* ((doc (doom-make-document-from-xml data))
           (book (doom-document-element doc))
           (deepcopy (doom-node-clone-node book t)))
      (cl-assert (not (eq book deepcopy)))
      (cl-assert (equal (doom-node-attributes book)
                        (doom-node-attributes deepcopy)))
      (cl-assert (not (eq (doom-node-attributes book)
                          (doom-node-attributes deepcopy))))
      (cl-assert (equal
                  (mapcar 'doom-node-name
                          (doom-element-get-elements-by-tag-name book '*))
                  (mapcar 'doom-node-name
                          (doom-element-get-elements-by-tag-name deepcopy '*))))
      (cl-assert (equal
                  (mapcar 'doom-node-value
                          (doom-element-get-elements-by-tag-name book '*))
                  (mapcar 'doom-node-value
                          (doom-element-get-elements-by-tag-name deepcopy '*))))
      (cl-assert (not (eq (car (doom-element-get-elements-by-tag-name
                                book 'firstname))
                          (car (doom-element-get-elements-by-tag-name
                                deepcopy 'firstname)))))
      (cl-assert (not (eq (doom-text-value
                           (third (doom-element-get-elements-by-tag-name
                                   book '\#text)))
                          (doom-text-value
                           (third (doom-element-get-elements-by-tag-name
                                   deepcopy '\#text))))))
      (cl-assert (string= (doom-text-value
                           (third (doom-element-get-elements-by-tag-name
                                   book '\#text)))
                          (doom-text-value
                           (third (doom-element-get-elements-by-tag-name
                                   deepcopy '\#text)))))
      (cl-assert (not (eq (doom-text-value
                           (third (doom-element-get-elements-by-tag-name
                                   book '\#text)))
                          (doom-text-value
                           (third (doom-element-get-elements-by-tag-name
                                   deepcopy '\#text)))))))
    
    (let* ((doc (doom-make-document-from-xml data))
           (book (doom-document-element doc))
           (old-chapter (doom-element-last-child book))
           (new-chapter (doom-document-create-element doc 'chapter)))
      (cl-assert (eq (doom-node-name (doom-element-insert-before book new-chapter))
                     'chapter))
      (cl-assert (equal (mapcar 'doom-element-name
                                (doom-element-child-nodes book))
                        '(bookinfo chapter chapter)))
      (cl-assert (eq new-chapter (doom-element-insert-before 
                                  book new-chapter
                                  (doom-element-first-child book))))
      (cl-assert (equal (mapcar 'doom-element-name
                                (doom-element-child-nodes book))
                        '(chapter bookinfo chapter)))
      (let ((new-bookinfo (doom-document-create-element doc 'bookinfo)))
        (doom-element-insert-before book new-bookinfo old-chapter))
      (cl-assert (equal (mapcar 'doom-element-name
                                (doom-element-child-nodes book))
                        '(chapter bookinfo bookinfo chapter))))

    ;; FIXME: some more tests for `doom-node-remove-child' and
    ;; `doom-node-replace-child' would be nice...  :)
    (let* ((doc (doom-make-document-from-xml data))
           (book (doom-document-element doc))
           (old-chapter (doom-element-last-child book))
           (new-chapter (doom-document-create-element doc 'chapter)))
      (doom-node-remove-child book old-chapter)
      (cl-assert (equal (mapcar 'doom-node-name (doom-node-child-nodes book))
                        '(bookinfo)))
      (doom-node-replace-child book new-chapter
                              (doom-node-first-child book))
      (cl-assert (equal (mapcar 'doom-node-name (doom-node-child-nodes book))
                        '(chapter))))

    (let* ((doc (make-doom-document))
           (par (doom-document-create-element doc 'p))
           (part1 (doom-document-create-text-node doc "This is "))
           (part2 (doom-document-create-element doc 'b))
           (part3 (doom-document-create-text-node doc ".")))
      (doom-element-append-child 
       part2 (doom-document-create-text-node doc "bold"))
      (doom-element-append-child par part1)
      (doom-element-append-child par part2)
      (doom-element-append-child par part3)
      (setf (doom-document-owner-document doc) doc
            (doom-document-element doc) par)
      (cl-assert (eq (doom-document-element doc) par))
      (cl-assert (string= (doom-node-text-content par)
                          "This is bold."))
      (doom-node-set-text-content par "This is plain.")
      (cl-assert (string= (doom-node-text-content par)
                          "This is plain."))
      (cl-assert (equal (mapcar 'doom-node-name (doom-node-child-nodes par))
                        '(\#text)))
      (doom-node-set-text-content par "New text.")
      (cl-assert (string= (doom-node-text-content par)
                          "New text."))
      (doom-node-set-text-content par "Different text.")
      (cl-assert (string= (doom-element-text-content par)
                          "Different text."))
      (let ((at (doom-document-create-attribute doc 'foo)))
        (setf (doom-attr-value at) "domino"
              (doom-element-attributes par) (list at))
        (cl-assert (string= "domino"
                            (doom-node-value
                             (doom-node-list-item
                              (doom-element-attributes par)
                              0))))
        (cl-assert (string= "domino"
                            (doom-node-text-content
                             (doom-node-list-item
                              (doom-element-attributes par)
                              0))))))

    (let* ((doc (doom-make-document-from-xml data))
           (title (car (doom-document-get-elements-by-tag-name doc "title"))))
      (cl-assert (equal (doom-element-text-content title)
                        "My own book!"))))
  (message "Passed tests!"))
