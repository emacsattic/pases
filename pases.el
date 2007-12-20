(load "luna")

(defvar pases:central-registry
  (list (expand-file-name "~/.pases/"))
  "Central directory for pases systems.")

(defvar pases:debug t)

(defun byte-recompile-file (file)
  (if (file-newer-than-file-p file (concat file "c"))
      (byte-compile-file file)
    t))

(defsubst pases:debug-message (&rest args)
  (if pases:debug
      (apply 'message args)))

(luna-define-generic pases:load-op (component &optional basedir))
(luna-define-generic pases:compile-op (component &optional basedir targetdir))

;; pases:component
(luna-define-class pases:component nil (name version dependencies pathname))

(luna-define-internal-accessors 'pases:component)

(luna-define-method pases:load-op ((c pases:component) &optional basedir)
  (pases:debug-message "[pases] Loading component %s." (pases:component-name-internal c))
  (pases:compile-op c basedir))

;; pases:source-file
(luna-define-class pases:source-file (pases:component)
		   (load compile))

(luna-define-internal-accessors 'pases:source-file)

(luna-define-method initialize-instance :before ((file pases:source-file) &rest args)
  (pases:source-file-set-compile-internal file t))

(luna-define-method pases:load-op :after ((file pases:source-file) &optional basedir)
  (pases:debug-message "[pases] Loading %s from %s." (pases:component-name-internal c) basedir)
  (if (pases:source-file-load-internal file)
      (if (pases:component-pathname-internal file)
          (load (pases:component-pathname-internal file))
        (load (expand-file-name (pases:component-name-internal file)
                                basedir)))))

(luna-define-method pases:compile-op ((f pases:source-file) &optional basedir targetdir)
  (if (pases:source-file-compile-internal f)
      (progn
        (let ((path (expand-file-name
                     (concat (pases:component-name-internal f) ".el")
                     basedir)))
          (pases:debug-message "[pases] Maybe compiling %s." path)
	(if (not (byte-recompile-file path))
	    (error "Error compiling %s " (pases:component-name-internal f)))))))
    
;; pases:source-dir
(luna-define-class pases:source-dir (pases:component))

(luna-define-internal-accessors 'pases:source-dir)

(luna-define-method pases:compile-op :after ((dir pases:source-dir) &optional basedir targetdir)
  (add-to-list 'load-path
	       (pases:component-pathname-internal dir)))

;; pases:module
(luna-define-class pases:module (pases:component) 
		   (components
		    default-component-class
		    if-component-dep-fails
		    serial))

(luna-define-internal-accessors 'pases:module)

(luna-define-method pases:load-op :after ((m pases:module) &optional basedir)
  (mapc (lambda (c)
	  (pases:load-op c (pases:component-pathname-internal m)))
	(pases:module-components-internal m)))

(luna-define-method pases:compile-op ((m pases:module) &optional basedir targetdir)
  (mapc (lambda (c)
	  (pases:compile-op c (pases:component-pathname-internal m)))
	(pases:module-components-internal m)))

;; pases:system
(luna-define-class pases:system (pases:module)
		   (depends version))

(luna-define-internal-accessors 'pases:system)

(luna-define-method pases:load-op :before ((s pases:system) &optional basedir)
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
  (let ((system-real (get sys 'pases:system)))
    (if (not system-real)
        (error "[pases] System %s is not loaded." sys) 
      (funcall op (get sys 'pases:system)))))

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
		     ;; :pathname (expand-file-name ,name dir)
		     ,@args))

(pases:define-systems)

(mapc (lambda (s)
	(pases:oos 'pases:load-op s))
      pases:systems)
