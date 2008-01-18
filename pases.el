(add-to-list 'load-path (file-name-directory load-file-name))
(load "luna")
(load "pases-package")

(defvar pases:central-registry
  (list (expand-file-name "~/.pases.d/"))
  "Central directory for pases systems.")

(defcustom pases:package-dir
  (expand-file-name "~/.pases.d/")
  "Directory to install user packages in."
  :type 'directory)

(defvar pases:debug t)

(defun byte-recompile-file (file)
  (if (file-newer-than-file-p file (concat file "c"))
      (byte-compile-file file)
    t))

(defsubst pases:debug-message (&rest args)
  (if pases:debug
      (apply 'message args)))

;; pases:op
(luna-define-class pases:op nil (func name))
(luna-define-internal-accessors 'pases:op)
(luna-define-generic pases:operate (op component &optional parent)) 
(luna-define-method pases:operate ((op pases:op) component &optional parent)
  (let ((dep-op (cdr (assoc (pases:op-name-internal op) (pases:component-dep-op-internal component)))))
    (if dep-op
        (funcall 'pases:operate (symbol-value dep-op) component parent))
    (funcall (pases:op-func-internal op) component parent)))

(setq pases:load-op (luna-make-entity 'pases:op :func 'pases:load :name 'pases:load-op))
(setq pases:compile-op (luna-make-entity 'pases:op :func 'pases:compile :name 'pases:compile-op))
  
;; pases:component
(luna-define-class pases:component nil 
		   (name version dependencies pathname dep-op))

(luna-define-internal-accessors 'pases:component)

(luna-define-generic pases:load (component &optional parent))
(luna-define-generic pases:compile (component &optional parent))
(luna-define-generic pases:operate (component operation &optional args))

;; pases:source-file
(luna-define-class pases:source-file (pases:component)
		   (load compile))

(luna-define-internal-accessors 'pases:source-file)

;;(luna-define-method initialize-instance :before ((file pases:source-file) &rest args)
;;  (pases:component-set-dep-op-internal file
;;                                       '((pases:load-op . pases:compile-op)))
;;  (pases:source-file-set-compile-internal file t))

;; pases:elisp-source
(luna-define-class pases:elisp-source (pases:source-file))

(luna-define-method pases:load ((file pases:elisp-source) &optional parent)
  (let ((basedir (pases:component-pathname-internal parent)))
    (pases:debug-message "[pases] loading %s from %s." (pases:component-name-internal c) basedir)
    (if (pases:source-file-load-internal file)
        (if (pases:component-pathname-internal file)
            (load (pases:component-pathname-internal file))
          (load (expand-file-name (pases:component-name-internal file)
                                  basedir))))))

(luna-define-method pases:compile ((f pases:elisp-source) &optional parent)
  (let ((basedir (pases:component-pathname-internal parent))
        (targetdir))
    (if (pases:source-file-compile-internal f)
        (progn
          (let ((path (expand-file-name
                       (concat (pases:component-name-internal f) ".el")
                       basedir)))
            (pases:debug-message "[pases] maybe compiling %s." path)
            (if (byte-recompile-file path)
                (if targetdir
                    (let ((target-path
                           (expand-file-name 
                            (concat (pases:component-name-internal f) ".elc")
                            targetdir)))
                      (rename-file (concat path "c") target-path)))
	    (error "Error compiling %s " (pases:component-name-internal f))))))))

(defmacro pases:deffile (name &rest args)
  `(luna-make-entity 'pases:elisp-source
		     :name ,name
                     ,@(if (not (plist-get args :compile))
                          '(:compile t))
                     ,@(if (not (plist-get args :dep-op))
                          '(:dep-op (quote ((pases:load-op . pases:compile-op)))))
                     ,@args))
 
;; pases:source-dir
(luna-define-class pases:source-dir (pases:component))

(luna-define-internal-accessors 'pases:source-dir)

(luna-define-method pases:load ((dir pases:source-dir) &optional parent)
  (let* ((basedir (pases:component-pathname-internal parent))
         (fullpath (expand-file-name (pases:component-name-internal dir) basedir)))
    (pases:debug-message "[pases] loading dir: %s." fullpath)
    (add-to-list 'load-path fullpath)))

(defmacro pases:defdir (name &rest args)
  `(luna-make-entity 'pases:source-dir
		     :name ,name
		     ,@args))

;; pases:module
(luna-define-class pases:module (pases:component) 
		   (components
		    default-component-class
		    if-component-dep-fails
		    serial))

(luna-define-internal-accessors 'pases:module)

(luna-define-method pases:load ((m pases:module) &optional parent)
  (pases:debug-message "[pases] loading module components from %s."
		       (pases:component-pathname-internal m))
  (mapc (lambda (c)
	  (pases:operate pases:load-op c m))
          (pases:module-components-internal m)))

;; pases:system
(luna-define-class pases:system (pases:module)
		   (depends version))

(luna-define-internal-accessors 'pases:system)

(luna-define-method pases:load :before ((s pases:system) &optional basedir)
  (pases:debug-message "[pases] loading system %s." (pases:component-name-internal s) basedir)
  (mapc (lambda (s)
	  (pases:oos 'pases:load s))
	(pases:system-depends-internal s)))

(defmacro pases:defsystem (name &rest args)
  `(progn
     (add-to-list 'pases:systems (quote ,name))
     (put (quote ,name) 'pases:system
	  (luna-make-entity 'pases:system
			  :name (symbol-name (quote ,name))
			  :pathname (file-name-directory (file-truename load-file-name))
			  ,@args))))

(defun pases:load-sysdefs ()
  (mapc 'pases:load-sysdef-dir
	pases:central-registry))

(defun pases:load-sysdef-dir (dir)
  (let ((files (directory-files dir t "\\.pasdef$")))
    (mapc 'pases:load-sysdef files))
  (let ((dirs (directory-files dir t "^[^\\.]")))
    (mapc (lambda (d)
	        (if (file-directory-p d)
		    (pases:load-sysdef-dir d)))
	  dirs)))

(defun pases:load-sysdef (file)
  (load-file file))
   
(defun pases:oos (op sys)
  (let ((system-real (get sys 'pases:system)))
    (if (not system-real)
        (error "[pases] System %s is not loaded." sys) 
      (funcall 'pases:operate op system-real))))

(defvar pases:systems '())
 
(defun pases:load-all ()
  (mapc (lambda (s)
	  (pases:oos pases:load-op s))
	pases:systems))

(put 'emacs 'pases:system
     (luna-make-entity 
      'pases:system
      :name "emacs"
      :version emacs-version))

(if (not (file-exists-p pases:package-dir))
    (make-directory pases:package-dir))

(pases:load-sysdefs)
(pases:load-all)
