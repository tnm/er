(defmodule er_server
  (export all))

(eval-when-compile 
  (include-file "include/utils.lfe"))

(include-file "include/utils-macro.lfe")

(defmacro redis-cmd-mk (command-name command-args wrapper-fun-name)
    (let* ((cmd (b command-name)))
     `(defun ,command-name (gen-server-name ,@command-args)
        (,wrapper-fun-name
          (: gen_server call gen-server-name
            (tuple 'cmd
              (: erldis multibulk_cmd (list ,cmd ,@command-args))))))))

(include-file "include/redis-return-types.lfe")
(include-file "include/redis-cmds.lfe")

(defrecord state
  (cxn 'nil))

(defun start_link
  ([gen-server-name ip port]
    (when (is_atom gen-server-name) (is_list ip) (is_integer port))
    (: gen_server start_link
      (tuple 'local gen-server-name) 'er_server (tuple ip port) '())))

(defun init
  ([(tuple ip port)]
    (case (: erldis connect ip port)
      ((tuple 'ok connection) (tuple 'ok (make-state cxn connection))))))

(defun handle_call
  ([(tuple 'cmd cmd-list) from state]
    (let* ((cxn (state-cxn state)))
      (spawn (lambda ()
        (: gen_server reply from
          (: erldis_client send cxn cmd-list 2000)))))
    (tuple 'noreply state)))
 
 (defun handle_cast (_request state)
   (tuple 'noreply state))
 
 (defun terminate (_reason _state)
   'ok)
 
 (defun handle_info (_request state)
   (tuple 'noreply state))

 (defun code_change (_old-version state _extra)
   (tuple 'ok state))

