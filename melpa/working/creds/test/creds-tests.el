(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(require 'creds)

;; store a dummy file
(with-temp-file "/tmp/temporary-authinfo"
  (insert "machine machine1 port 993 login some-login password some-password\n")
  (insert "machine machine2 port 888 login some-other-login password \"one spaced password\""))

(expectations
 (desc "creds/--protect-blank-spaced-words")
 (expect (format "\"some%sstring%swith%sblanks\"" *creds/protection-string-against-blank-char*
                 *creds/protection-string-against-blank-char*
                 *creds/protection-string-against-blank-char*
                 *creds/protection-string-against-blank-char*)
   (creds/--protect-blank-spaced-words "\"some string with blanks\"")))

(expectations
 (desc "creds/--unprotect-blank-spaced-words")
 (expect "some string with blanks"
         (creds/--unprotect-blank-spaced-words (format "\"some%sstring%swith%sblanks\"" *creds/protection-string-against-blank-char*
                                                       *creds/protection-string-against-blank-char*
                                                       *creds/protection-string-against-blank-char*
                                                       *creds/protection-string-against-blank-char*))))

(expectations
 (desc "creds/--read-and-protect-content-file")
 (expect "machine machine1 port 993 login some-login password some-password
machine machine2 port 888 login some-other-login password \"one@#$~!!~$#@spaced@#$~!!~$#@password\""
         (creds/--read-and-protect-content-file "/tmp/temporary-authinfo")))

(expectations
 (desc "creds/read-lines")
 (expect
  '(("machine" "machine1" "port" "993" "login" "some-login" "password" "some-password")
    ("machine" "machine2" "port" "888" "login" "some-other-login" "password" "one spaced password"))
  (creds/read-lines "/tmp/temporary-authinfo")))

;; another edge case

(with-temp-file "/tmp/temporary-authinfo2"
  (insert "machine email-description mail some-email surname some-surname x-url some-url mail-host some-mail-host signature-file \"/path/to/some signature file\" smtp-server some-smtp-server draft-folder \"/[Gmail].Drafts\" sent-folder \"/[Gmail].Sent Mail\" trash-folder \"/[Gmail].Trash\" archive-folder \"/[Gmail].All Mail\" attachment-folder \"~/Downloads\""))

(expectations
 (desc "creds/read-lines - edge case")
 (expect
  '(("machine" "email-description"
     "mail" "some-email"
     "surname" "some-surname"
     "x-url" "some-url"
     "mail-host" "some-mail-host"
     "signature-file" "/path/to/some signature file"
     "smtp-server" "some-smtp-server"
     "draft-folder" "/[Gmail].Drafts"
     "sent-folder" "/[Gmail].Sent Mail"
     "trash-folder" "/[Gmail].Trash"
     "archive-folder" "/[Gmail].All Mail"
     "attachment-folder" "~/Downloads"))
  (creds/read-lines "/tmp/temporary-authinfo2")))

(setq dat '(("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")
            ("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993")
            ("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")
            ("machine" "jabber" "login" "some-login" "password" "some-pwd")
            ("machine" "description" "name" "\"my" "name" "is\"" "blog" "some-blog" "mail" "some-mail")))

(expectations
 (desc "creds/get")
  (expect '("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")      (creds/get dat "machine0"))
  (expect '("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993") (creds/get dat "machine1"))
  (expect nil                                                                             (creds/get dat "login"))
  (expect nil                                                                             (creds/get dat "something-that-does-not-exist"))
  (expect nil                                                                             (creds/get dat nil))
  (expect nil                                                                             (creds/get nil nil)))

(expectations
 (desc "creds/get-with")
 (expect '("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")
         (creds/get-with dat '(("machine" . "machine0") ("login" . "nouser"))))
 (expect '("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")
         (creds/get-with dat '(("machine" . "machine2") ("login" . "some-login")))))

(setq machine '("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass"))

(expectations
 (desc "creds/get-entry")
 (expect "machine0" (creds/get-entry machine "machine"))
 (expect "http"     (creds/get-entry machine "port"))
 (expect "nouser"   (creds/get-entry machine "login"))
 (expect "nopass"   (creds/get-entry machine "password"))
 (expect nil        (creds/get-entry machine "something-that-does-not-exist"))
 (expect nil        (creds/get-entry nil nil)))

(provide 'creds-tests)
