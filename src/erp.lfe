(defmodule (erp client)
  (export all))

(eval-when-compile 
  (include-file "include/utils.lfe"))

(include-file "include/utils-macro.lfe")

(defmacro redis-cmd-mk (command-name command-args wrapper-fun-name)
    (let* ((cmd (b command-name)))
     `(defun ,command-name (,@command-args)
        (,wrapper-fun-name (: erldis_client send client
          (: erldis multibulk_cmd (list ,cmd ,@command-args)) 2000)))))

(include-file "include/redis-return-types.lfe")
(include-file "include/redis-cmds.lfe")
