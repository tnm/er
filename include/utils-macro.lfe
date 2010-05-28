
; Yes, the many similar redis-cmd-*'s below could be created by a macro 
; yielding macros, but they aren't.
; Need to figure out how to get recursive defmacro to spit out a usable
; macro at compile time.

;(defmacro redis-cmd (small-type return-decoder)
;  `(defsyntax ,(mk-a 'redis-cmd- small-type)
;    ([command-name] (redis-cmd-mk command-name () ,return-decoder))
;    ([command-name command-args] (redis-cmd-mk command-name command-args ,return-decoder))))
;
;(redis-cmd -n redis-return-nil)
;(redis-cmd -s redis-return-status)
;(redis-cmd -i redis-return-integer)
;(redis-cmd -l redis-return-single-line)
;(redis-cmd -b redis-return-bulk)
;(redis-cmd -m redis-return-multibulk)
;(redis-cmd -o redis-return-special)
;(redis-cmd -i-tf redis-return-integer-true-false)

(defsyntax redis-cmd-n
  ([command-name] (redis-cmd-mk command-name () redis-return-nil))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-nil)))
(defsyntax redis-cmd-s
  ([command-name] (redis-cmd-mk command-name () redis-return-status))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-status)))
(defsyntax redis-cmd-i
  ([command-name] (redis-cmd-mk command-name () redis-return-integer))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-integer)))
(defsyntax redis-cmd-l
  ([command-name] (redis-cmd-mk command-name () redis-return-single-line))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-single-line)))
(defsyntax redis-cmd-b
  ([command-name] (redis-cmd-mk command-name () redis-return-bulk))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-bulk)))
(defsyntax redis-cmd-m
  ([command-name] (redis-cmd-mk command-name () redis-return-multibulk))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-multibulk)))
(defsyntax redis-cmd-o
  ([command-name] (redis-cmd-mk command-name () redis-return-special))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-special)))
(defsyntax redis-cmd-i-tf
  ([command-name] (redis-cmd-mk command-name () redis-return-integer-true-false))
  ([command-name command-args] (redis-cmd-mk command-name command-args redis-return-integer-true-false)))

(defmacro return-type (name redis-cmds)
  `(defun ,(mk-a-return-type name) () ',redis-cmds))
