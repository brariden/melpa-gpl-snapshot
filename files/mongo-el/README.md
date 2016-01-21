This is EmacsLisp for talking to mongo-db.

A quick example using Marmalade's mongo-db:

```
(let* ((result
        (mongo-with-open-database
            (db :host 'local)
          (mongo-do-request
           (make-mongo-message-query
            :flags 0
            :number-to-skip 0
            :number-to-return 0
            :full-collection-name "marmalade.packages"
            :query '(("name" . "fakir")))
           :database db)))
       (docres (mongo-message-reply-documents result)))
  (assoc-string
   "headers"
   (assoc-string "_latestVersion" (car docres))))
```

This performs a query for a single result, the ```car docres``` is
used just because the result is always a list.

You can look up objects by their object id by using the bson serializer:

```
(make-mongo-message-query
 :flags 0
 :number-to-skip 0
 :number-to-return 0
 :full-collection-name "marmalade.packages"
 :query (list (cons "_id" (bson-oid-of-hex-string "4f65e980cd6108da68000252"))))
```

Dealing with mongo
==================

Dealing with mongo seems to result in lots of little JSON documents
represented as alists.

[This module might help with that](https://github.com/nicferrier/emacs-dotaccess)


Building with Elpakit
=====================

An [elpakit](https://github.com/nicferrier/elpakit) recipe is included
so you can easily build the mongo-el package.
