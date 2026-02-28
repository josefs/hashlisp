;;; ==========================================================================
;;; quine.lsp — A quine (self-reproducing program) in Hashlisp
;;; ==========================================================================

((lambda (x) (list x (list 'quote x)))
 '(lambda (x) (list x (list 'quote x))))
