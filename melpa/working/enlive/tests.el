
(require 'enlive)

(ert-deftest enlive-test-get-elements-by-class-name ()
  (let ((element (enlive-parse "<div id=\"id1\" class=\"cls cls2\"></div>")))
    (should (equal (enlive-get-elements-by-class-name element "cls2")
                   '((div ((id . "id1") (class . "cls cls2"))))))))

(ert-deftest enlive-test-get-element-by-id ()
  (let ((element (enlive-parse "<div id=\"id1\" class=\"cls cls2\"><span id=\"id2\"></span></div>")))
    (should (equal (enlive-get-element-by-id element "id2")
                   '(span ((id . "id2")))))))

(ert-deftest enlive-test-get-elements-by-tag-name ()
  (let ((element (enlive-parse "<div><span>span</span></div>")))
    (should (equal (enlive-get-elements-by-tag-name element 'span)
                   '((span nil "span"))))))

(ert-deftest enlive-test-find-elements ()
  (let ((element (enlive-parse "<div id=\"id1\" class=\"cls cls2\"><span id=\"id2\"></span></div>")))
    (should (equal (enlive-find-elements element '((id . "id2")))
                   '((span ((id . "id2"))))))))

(ert-deftest enlive-test-parse-selector ()
  (should (equal (enlive-parse-selector [.cls .cls2])
                 '((enlive-find-elements node '((class . "cls")))
                   (enlive-direct-children node)
                   (enlive-find-elements node '((class . "cls2"))))))
  (should (equal (enlive-parse-selector [tag > .cls2])
                 '((enlive-find-elements node '((tag . "tag")))
                   (enlive-direct-children node)
                   (enlive-match-element node '((class . "cls2"))))))
  (should (equal (enlive-parse-selector [div *])
                 '((enlive-find-elements node '((tag . "div")))
                   (enlive-direct-children node)
                   (enlive-all node)))))

(ert-deftest enlive-test-query-class ()
  (let ((element (enlive-parse "<div id=\"id1\" class=\"cls cls2\"><span class=\"cls3 cls4\"></span></div>")))
    (should (equal (enlive-query element [.cls3])
                   '(span ((class . "cls3 cls4")))))))

(ert-deftest enlive-test-query-id ()
  (let ((element (enlive-parse "<div id=\"id1\" class=\"cls cls2\"><span class=\"cls2\" id=\"id2\"></span></div>")))
    (should (equal (enlive-query element [:id1 .cls2])
                   '(span ((class . "cls2") (id . "id2")))))))

(ert-deftest enlive-test-query-nested-classes ()
  (let ((element (enlive-parse "<p class=\"cls cls2\">
                                    <i class=\"cls3\"></i>
                                    <i class=\"cls3\">
                                        <i class=\"cls4\">
                                            <i class=\"cls5\"></i>
                                        </i>
                                    </i>
                                    <i class=\"cls3\">
                                        <i class=\"cls4\"></i>
                                    </i>
                                </p>")))
    (should (equal (enlive-query-all element [.cls2 .cls3 .cls4 .cls5])
                   '((i ((class . "cls5"))))))
    (should (equal (enlive-query-all element [.cls2 .cls3 .cls5])
                   '((i ((class . "cls5"))))))
    (should (equal (enlive-query-all element [.cls3 .cls5])
                   '((i ((class . "cls5"))))))))

(ert-deftest enlive-test-query-tag ()
  (let ((element (enlive-parse "<p><a><i></i></a></p>")))
    (should (equal (enlive-query element [a i])
                   '(i nil)))
    (should (equal (enlive-query element [i])
                   '(i nil)))))

(ert-deftest enlive-test-query-asterisk ()
  (let ((element (enlive-parse "<span><i class=\"cls\"></i><i></i></span>")))
    (should (equal (enlive-query-all element [span *])
                   '((i ((class . "cls")))
                     (i nil))))))

(ert-deftest enlive-test-tokenize ()
  (should (equal (enlive-tokenize "tag:id.cls.cls2")
                 '((tag . "tag") (id . "id") (class . "cls") (class . "cls2")))))

(ert-deftest enlive-test-with ()
  (enlive-with (enlive-parse "<div class=\"cls\"><span><i></i></span><i></i></div>")
               (should (equal (enlive-query-all [.cls i])
                              '((i nil) (i nil))))))

(ert-deftest enlive-test-let ()
  (should
   (equal
    (enlive-let (enlive-parse "<div><span><i class=\"cls1\"></i></span><span class=\"cls2\"></span></div>")
                ((el1 [.cls1])
                 (el2 [.cls2]))
                (list (caar el1) (caar el2)))
    '(i span))))
