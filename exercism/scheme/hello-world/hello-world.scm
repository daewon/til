(define-module (hello-world) #:export (hello))

(define hello
  (lambda args
    (let ((rest (if (> (length args) 0) (car args) "World")))
      (string-append "Hello, " rest "!"))))
