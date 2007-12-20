(load "luna")

(defvar pases:central-registry
  (list (expand-file-name "~/.pases/"))
  "Central directory for pases systems.")

(defun byte-recompile-file (file)
  (if (file-newer-than-file-p file (concat file "c"))
      (byte-compile-file file)
    t))

(luna-define-generic pases:load-op (component))
(luna-define-generic pases:compile-op (component))

;; pases:component
(luna-define-class pases:component nil (name version dependencies pathname))

(luna-define-internal-accessors 'pases:component)

(luna-define-method pases:load-op ((c pases:component))
  (message "Loading component %s." (pases:component-name-internal c))
  (pases:compile-op c))

;; pases:source-file
(luna-define-class pases:source-file (pases:component)
		   (load compile))

(luna-define-internal-accessors 'pases:source-file)

(luna-define-method initialize-instance :before ((file pases:source-file) &rest args)
  (pases:source-file-set-compile-internal file t))

(luna-define-method pases:load-op :after ((f pases:source-file))
  (message "Loading source file %s." (pases:component-name-internal c))
  (if (pases:source-file-load-internal f)
      (load (pases:component-pathname-internal f))))

(luna-define-method pases:compile-op ((f pases:source-file))
  (if (pases:source-file-compile-internal f)
      (progn
	(message "Compiling source file %s." (pases:component-name-internal f))
	(if (not (byte-recompile-file
		  (concat (pases:component-pathname-internal f) ".el")))
	    (error "Error compiling %s " (pases:component-name-internal f))))))
    
;; pases:source-dir
(luna-define-class pases:source-dir (pases:component))

(luna-define-internal-accessors 'pases:source-dir)

(luna-define-method pases:compile-op :after ((dir pases:source-dir))
  (add-to-list 'load-path
	       (pases:component-pathname-internal dir)))

;(luna-define-method pases:compile-op ((dir pases:source-dir))
;  (byte-recompile-directory
;   (pases:component-pathname-internal dir) nil t))

;; pases:module
(luna-define-class pases:module (pases:component) 
		   (components
		    default-component-class
		    if-component-dep-fails
		    serial))

(luna-define-internal-accessors 'pases:module)

(luna-define-method pases:load-op :after ((m pases:module))
  (mapc (lambda (c)
	  (pases:load-op c))
	(pases:module-components-internal m)))

(luna-define-method pases:compile-op ((m pases:module))
  (mapc (lambda (c)
	  (pases:compile-op c))
	(pases:module-components-internal m)))

;; pases:system
(luna-define-class pases:system (pases:module)
		   (depends))

(luna-define-internal-accessors 'pases:system)

(luna-define-method pases:load-op :before ((s pases:system))
  (mapc (lambda (s)
	  (pases:oos 'pases:load-op s))
	(pases:system-depends-internal s)))

(defun pases:define-systems ()
  (mapc 'pases:define-system-dir
	pases:central-registry))

(defun pases:define-system-dir (dir)
  (let ((files (directory-files dir t "\\.pases$")))
    (mapc 'pases:define-system files)))

(defun pases:define-system (file)
  (let ((pases:system-file (file-truename file)))
    (load-file file)))

(defun pases:oos (op sys)
  (funcall op (get sys 'pases:system)))

(defvar pases:systems '())

(defmacro pases:defsystem (name &rest args)
  `(let ((dir (file-name-directory pases:system-file)))
     (add-to-list 'pases:systems (quote ,name))
     (put (quote ,name) 'pases:system 
	  (luna-make-entity 'pases:system
			    :name (symbol-name (quote ,name))
			    :pathname dir
			    ,@args))))

(defmacro pases:deffile (name &rest args)
  `(luna-make-entity 'pases:source-file
		     :name ,name
		     :pathname (expand-file-name ,name dir)
		     ,@args))

(pases:define-systems)

(mapc (lambda (s)
	(pases:oos 'pases:load-op s))
      pases:systems)
