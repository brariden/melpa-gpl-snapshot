(require 'blog-admin)
(require 'f)

(describe "hexo"
          (let ((work-path nil))
            (before-each
             (setq work-path (make-temp-file "test-" t))
             (f-mkdir work-path)
             (f-mkdir work-path "source")
             (f-mkdir work-path "source" "_drafts")
             (f-mkdir work-path "source" "_posts")
             (setq blog-admin-backend-path work-path)
             (setq blog-admin-backend-type 'hexo)
             )

            (after-each
             (f-delete work-path t)
             (setq work-path nil)
             (kill-buffer blog-admin-mode-buffer)
             (setq blog-admin-mode-buffer nil)
             )

            (it
             "new post with `blog-admin-backend-new-post-in-drafts' `t' and `blog-admin-backend-new-post-with-same-name-dir' `t', a `test.org' and `test' should exist in `<work-path>/source/_drafts/>"
             (setq blog-admin-backend-new-post-in-drafts t)
             (setq blog-admin-backend-new-post-with-same-name-dir t)
             (blog-admin-start)
             (blog-admin-backend-hexo-new-post "test.org")
             (expect (f-exists?
                      (f-join
                       work-path blog-admin-backend-hexo-drafts-dir "test.org"))
                     :to-equal t)
             (expect (f-exists?
                      (f-join
                       work-path blog-admin-backend-hexo-drafts-dir "test"))
                     :to-equal t))

            (it
             "new post with `blog-admin-backend-new-post-in-drafts' `t' and `blog-admin-backend-new-post-with-same-name-dir' `nil', a `test.org' should exist in `<work-path>/_drafts/> but no `test'"
             (setq blog-admin-backend-new-post-in-drafts t)
             (setq blog-admin-backend-new-post-with-same-name-dir nil)
             (blog-admin-start)
             (blog-admin-backend-hexo-new-post "test.org")
             (expect (f-exists?
                      (f-join
                       work-path blog-admin-backend-hexo-drafts-dir "test.org"))
                     :to-equal t)
             (expect (f-exists?
                      (f-join
                       work-path blog-admin-backend-hexo-drafts-dir "test"))
                     :to-equal nil)))


          (it
           "new post with one `blog-admin-backend-after-new-post-hook'"
           (setq blog-admin-backend-new-post-in-drafts t)
           (setq blog-admin-backend-new-post-with-same-name-dir nil)
           (let ((result nil))
             (add-hook 'blog-admin-backend-after-new-post-hook
                       (lambda (file-path)
                         (setq result file-path)
                         ))
             (blog-admin-start)
             (blog-admin-backend-hexo-new-post "test.org")
             (expect (f-join
                      work-path blog-admin-backend-hexo-drafts-dir "test.org")
                     :to-equal result)
             )
           )

          (it
           "new post with two `blog-admin-backend-after-new-post-hook'"
           (setq blog-admin-backend-new-post-in-drafts t)
           (setq blog-admin-backend-new-post-with-same-name-dir nil)
           (let ((result nil)
                 (result2 nil))
             (add-hook 'blog-admin-backend-after-new-post-hook
                       (lambda (file-path) (setq result file-path)))
             (add-hook 'blog-admin-backend-after-new-post-hook
                       (lambda (file-path) (setq result2 file-path)))
             (blog-admin-start)
             (blog-admin-backend-hexo-new-post "test.org")
             (expect (f-join
                      work-path blog-admin-backend-hexo-drafts-dir "test.org")
                     :to-equal result)
             (expect (f-join
                      work-path blog-admin-backend-hexo-drafts-dir "test.org")
                     :to-equal result2)
             )
           )



          )
