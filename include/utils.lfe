; turn anything reasonable into an atom
(defun a
  ((c) (when (is_list c)) (list_to_atom c))
  ((c) (when (is_atom c)) c)
  ((c) (when (is_binary c)) (a (binary_to_list c))))

(defun mk-a (c d)
  (a (: lists flatten (cons (l c) (l d)))))

; turn anything reasonable into a list
(defun l
  ((c) (when (is_list c)) c)
  ((c) (when (is_atom c)) (atom_to_list c))
  ((c) (when (is_binary c)) (binary_to_list c)))

; turn anything reasonable into a binary
(defun b
  ((c) (when (is_list c)) (list_to_binary c))
  ((c) (when (is_atom c)) (b (atom_to_list c)))
  ((c) (when (is_binary c)) c))

(defun mk-a-return-type (type)
  (mk-a 'return-type:: type))

; This is supposed to take the return types in redis-return-types.lfe
; and automatically extract which commands have which return type.
; Can't figure out how to force LFE to let me funcall or execute
; a function based on an atom name.
;(defun build-cmd-type-dict ()
;  ; fold over reutrn-type::return-types to get all types
;  (: lists foldl
;    (lambda (type acc)
;      (: dict merge (lambda (k v1 v2) v1) acc 
;        ; for each type, fold over each command to store its type
;        (: lists foldl
;          (lambda (cmd-name cmd-acc)
;            (: dict store cmd-name type cmd-acc)) ; fun
;          (: dict new) ; acc0
;          ((mk-a-return-type type))) ; list <-- this line is the problem. 
;          ; ^^^ needs to call the dynamic function name at compile time.  doesn't work yet
;    (: dict new) ; acc0
;    (return-type::return-types)))) ; list

;(defun to-tuple-list (cmd-type cmds)
;  (lc ((<- cmd cmds)) (tuple cmd cmd-type)))

;(defun dict-from-type (cmd-type cmds)
;  (: dict from_list (to-tuple-list cmd-type cmds)))

; This is a less-loopy example of above, but we still end up with an
; atom name that has to be executed against.  No go.
;(defun build-cmd-type-dict ()
;  (let* ((dict1 (dict-from-type 'nil (return-type::nil)))
;         (dict2 (dict-from-type 'status (return-type::status)))
;         (dict3 (dict-from-type 'integer (return-type::integer)))
;         (dict4 (dict-from-type 'single-line (return-type::single-line)))
;         (dict5 (dict-from-type 'bulk (return-type::bulk)))
;         (dict6 (dict-from-type 'multibulk (return-type::multibulk)))
;         (dict7 (dict-from-type 'special (return-type::special)))
;         (alldicts (list dict1 dict2 dict3 dict4 dict5 dict6 dict7))
;         (nofun (lambda (key val1 val2) val1)))
;    (: lists foldl (lambda (e acc) (: dict merge nofun e acc)) (: dict new) alldicts)))

;(defun find-return-wrapper (cmd)
;   (let* ((cmd-to-type (build-cmd-type-dict)))
;     'return-nil))
