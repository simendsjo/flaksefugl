(cl:defpackage :flaksefugl
  (:use :cl :alexandria :serapeum)
  (:local-nicknames (#:gk #:trivial-gamekit)
                    (#:a #:alexandria)
                    (#:t #:trivia))
  (:export #:flaksefugl
           #:start
           #:stop))
