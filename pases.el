;;; pases.el --- packaging & system definition for Emacs
;;
;; Copyright (C) 2008-2010 Erik Hetzner
;;
;; Author: Erik Hetzner <egh@e6h.org>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Variables
(defvar pases:emacs-variant
  (if (featurep 'xemacs)
      :xemacs
    :gnuemacs)
  "Emacs variant (ie, :gnuemacs or :xemacs).")

(defvar pases:window-system
  (cond ((eq window-system 't)
         :x))
  "Window system (ie, :x).")

(defvar pases:emacs-version
  (concat 
   (number-to-string emacs-major-version) "."
   (number-to-string emacs-minor-version))
  "Version string of the current emacs version.")

(defvar pases:system-name
  (cond ((eq pases:emacs-variant :gnuemacs)
         (system-name))
        ((eq pases:emacs-variant :xemacs)
         (getenv "HOST")))
  "The name of the host that Emacs is running on.")

(defvar pases:elc-dir
  (expand-file-name
   (format "~/.pases.%s.%d.d"
           (substring 
            (symbol-name pases:emacs-variant) 1)
           emacs-major-version))
  "Directory to store elc files in.")

(defvar pases:debug t)

(defvar pases:systems '())

;; Utility functions
(defun pases:byte-recompile-file (el-file)
  (let ((elc-file
	 (expand-file-name (concat (file-name-nondirectory el-file) "c")
			   pases:elc-dir)))
    (if (not (file-directory-p pases:elc-dir))
        (make-directory pases:elc-dir))
    (if (file-newer-than-file-p el-file elc-file)
	(let ((results (byte-compile-file el-file)))
	  (or (eq results 'no-byte-compile)
	      results))
      t)))

(defsubst pases:debug-message (str &rest args)
  (if pases:debug
      (apply 'message (concat "[pases] " str) args)))

(defsubst pases:mk-symbol (s)
  (if (stringp s) (intern s) s))

(defsubst pases:mk-list (l)
  (if (listp l) l (list l)))

;; borrowed from apel
(defun pases:module-installed-p (module)
  (let ((module-symbol (pases:mk-symbol module))
        (paths load-path))
    (or (featurep module-symbol)
        (let ((file (symbol-name module-symbol)))
          (catch 'tag
            (while paths
              (let ((stem (expand-file-name file (car paths)))
                    (sufs '(".elc" ".el")))
                (while sufs
                  (let ((file (concat stem (car sufs))))
                    (if (file-exists-p file)
                        (throw 'tag file)))
                  (setq sufs (cdr sufs))))
              (setq paths (cdr paths))))))))

(defun pases:modules-installed-p (modules)
  (let ((modules-list (pases:mk-list modules)))
  (catch 'done
    (mapc (lambda (m)
            (if (not (pases:module-installed-p m))
                (throw 'done nil)))
          modules-list)
    (throw 'done t))))

(defun pases:get-system (name)
  "Get the object describing system with name (a symbol)."
  (get name 'pases:system))

(defun pases:put-system (name system)
  "Set the object describing system with name (a symbol)."
  (put name 'pases:system system))

(defun pases:put-elisp (name elisp)
  "Set the object describing elisp file name (a string)."
  (let ((s (if (symbolp name)
	       s
	     (intern (file-name-nondirectory name)))))
    (put s 'pases:elisp-source elisp)))

(defun pases:get-elisp (name)
  "Get the object describing elisp file name (a string)."
  (let ((s (if (symbolp name)
	       s
	     (intern (file-name-nondirectory name)))))
    (get s 'pases:elisp-source)))

(defun pases:read-sysdef (file)
  (load-file file))

(defun pases:undef-system (name)
  (pases:put-system name nil)
  (setq pases:systems (delq name pases:systems)))
   
(defun pases:system-defined? (name)
  (not (not (pases:get-system name))))
       
(defun pases:oos (op sys)
  "Call op on system (a symbol)."
  (let ((system-real (pases:get-system sys)))
    (if (not system-real)
        (error "[pases] System %s is not defined." sys) 
      (funcall 'pases:operate op system-real))))

(defun pases:load-all ()
  "Call pases:load-op on all systems."
  (mapc (lambda (s)
	  (pases:oos pases:load-op s))
	pases:systems))

;; pases:op
;; An "operation", e.g. compile, load.
(pases:luna-define-class pases:op nil (func name needed-func))
(pases:luna-define-internal-accessors 'pases:op)
(pases:luna-define-generic pases:operate (op component &optional parent)) 
(pases:luna-define-method pases:operate ((op pases:op) component &optional parent)
  (let* ((dep-op (cdr (assoc (pases:op-name-internal op) (pases:component-dep-op-internal component))))
	 (short-op-name (substring (symbol-name (pases:op-func-internal op)) 6))
         (needed-func (pases:op-needed-func-internal op))
         (pases:current-component component)
	 (after-hook-name (intern (concat "pases:component-after-" short-op-name "-hook-internal")))
	 (before-hook-name (intern (concat "pases:component-before-" short-op-name "-hook-internal")))
	 (before-hook (eval (macroexpand `(,before-hook-name component))))
	 (after-hook (eval (macroexpand `(,after-hook-name component)))))
    (if dep-op
        (funcall 'pases:operate (symbol-value dep-op) component parent))
    ;; Check if this operation is needed.
    (if (or (not needed-func)
            (funcall needed-func component parent))
        (progn
          (if before-hook (funcall before-hook))
          (funcall (pases:op-func-internal op) component parent)
          (if after-hook (funcall after-hook)))
      (pases:debug-message "%s on %s not needed." (pases:op-name-internal op)
                           (pases:component-name-internal component)))))

;; Define operations.
(setq pases:load-op (pases:luna-make-entity 'pases:op
                                      :func 'pases:load
                                      :needed-func 'pases:load-needed
                                      :name 'pases:load-op))

(setq pases:unload-op (pases:luna-make-entity 'pases:op
                                        :func 'pases:unload 
                                        :needed-func 'pases:unload-needed
                                        :name 'pases:unload-op))

(setq pases:compile-op (pases:luna-make-entity 'pases:op
                                               :func 'pases:compile
                                               :needed-func 'pases:compile-needed
                                               :name 'pases:compile-op))

(setq pases:enable-op (pases:luna-make-entity 'pases:op
					      :func 'pases:enable
					      :needed-func 'pases:enable-needed
                                               :name 'pases:enable-op))

(setq pases:disable-op (pases:luna-make-entity 'pases:op
                                               :func 'pases:disable
                                               :needed-func 'pases:disable-needed
                                               :name 'pases:disable-op))
  
;; pases:component
;; Base class for all components.
(pases:luna-define-class pases:component nil 
		   (name pathname dep-op loaded
			 before-enable-hook after-enable-hook
                         before-compile-hook after-compile-hook
                         before-load-hook after-load-hook
                         before-unload-hook after-unload-hook
                         before-disable-hook after-disable-hook))
(pases:luna-define-internal-accessors 'pases:component)

;; Base functions for enable.
(pases:luna-define-generic pases:enable (component &optional parent))
(pases:luna-define-generic pases:enable-needed (component &optional parent))

(pases:luna-define-method pases:enable-needed ((c pases:component) &rest args)
  t)

;; Base functions for compile.
(pases:luna-define-generic pases:compile (component &optional parent))
(pases:luna-define-generic pases:compile-needed (component &optional parent))

(pases:luna-define-method pases:compile-needed ((c pases:component) &rest args)
  t)

(pases:luna-define-generic pases:load (component &optional parent))
(pases:luna-define-generic pases:load-needed (component &optional parent))

;; We only need to load if it is not loaded already.
(pases:luna-define-method pases:load-needed ((c pases:component) &rest args)
  (not (pases:component-loaded-internal c)))

(pases:luna-define-method pases:load ((c pases:component) &rest args)
  (pases:component-set-loaded-internal c t))

;; Base functions for unload.
(pases:luna-define-generic pases:unload (component &optional parent))
(pases:luna-define-generic pases:unload-needed (component &optional parent))

;; We only need to unload if it is loaded.
(pases:luna-define-method pases:unload-needed ((c pases:component) &rest args)
  (pases:component-loaded-internal c))

(pases:luna-define-method pases:unload ((c pases:component) &rest args)
  (pases:component-set-loaded-internal c nil))

;; Base functions for disable.
(pases:luna-define-generic pases:disable (component &optional parent))
(pases:luna-define-generic pases:disable-needed (component &optional parent))

(pases:luna-define-method pases:disable-needed ((c pases:component) &rest args)
  t)

;; pases:source-file
;; Base for anything that needs to be "compiled".
(pases:luna-define-class pases:source-file (pases:component)
                         (load compile optional))
(pases:luna-define-internal-accessors 'pases:source-file)

;; pases:elisp-source
;; Meat of pases.
(pases:luna-define-class pases:elisp-source
                   (pases:source-file)
		   (compile-after
                    generate-autoloads-to
                    generated
                    only-if))
(pases:luna-define-internal-accessors 'pases:elisp-source)

;; Enabling entails copying into pases:elc-dir.
(pases:luna-define-method pases:enable-needed ((f pases:elisp-source) &optional parent)
  (let ((only-if (pases:elisp-source-only-if-internal f)))
    (and (or (not only-if)
             (funcall only-if))
         (not (pases:elisp-source-generated-internal f))
         (not (file-exists-p (expand-file-name
                              (file-name-nondirectory (concat (pases:component-name-internal f) ".el"))
                              pases:elc-dir))))))

(pases:luna-define-method pases:enable ((f pases:elisp-source) &optional parent)
  (let ((file-name (concat (pases:component-name-internal f) ".el"))
	(basedir (pases:component-pathname-internal parent)))
    (copy-file (expand-file-name file-name basedir)
	       (expand-file-name (file-name-nondirectory file-name) pases:elc-dir))))

(pases:luna-define-method pases:compile-needed ((f pases:elisp-source) &optional parent)
  (let ((compile-after (pases:elisp-source-compile-after-internal f))
        (only-if (pases:elisp-source-only-if-internal f)))
    (and (or (not compile-after)
             (pases:modules-installed-p compile-after))
         (or (not only-if)
             (funcall only-if))
         (file-newer-than-file-p
          (expand-file-name
           (concat (pases:component-name-internal f) ".el")
           (pases:component-pathname-internal parent))
          (expand-file-name (concat (file-name-nondirectory 
                                     (pases:component-name-internal f)) ".elc")
                            pases:elc-dir)))))

(pases:luna-define-method pases:compile ((f pases:elisp-source) &optional parent)
  (let* ((autoloads-to-raw (pases:elisp-source-generate-autoloads-to-internal f))
	 (autoloads-to (and autoloads-to-raw
			    (if (string= (substring autoloads-to-raw -3) ".el")
				autoloads-to-raw
			      (concat autoloads-to-raw ".el")))))
    (let ((path (expand-file-name
                 (file-name-nondirectory (concat (pases:component-name-internal f) ".el"))
                 pases:elc-dir)))
      (pases:debug-message "maybe compiling %s." path)
      (if (pases:source-file-compile-internal f)
          (if (pases:byte-recompile-file path)
              (if autoloads-to
                  (let ((generated-autoload-file
                         (expand-file-name autoloads-to pases:elc-dir)))
                    (update-file-autoloads path t)
                    (kill-buffer (find-buffer-visiting generated-autoload-file))))
            (if (not (pases:source-file-optional-internal f))
                (error "Error compiling %s" (pases:component-name-internal f))
              (message "Error compiling %s" (pases:component-name-internal f))))))))

;; Generally elisp files are *not* loaded by default. Those with the
;; load slot eq t are.
(pases:luna-define-method pases:load-needed ((file pases:elisp-source) &optional parent)
  (and (pases:source-file-load-internal file)
       (not (pases:component-loaded-internal file))))

(pases:luna-define-method pases:load :before ((file pases:elisp-source) &optional parent)
  (load (file-name-nondirectory
         (pases:component-name-internal file))))

(pases:luna-define-method pases:unload-needed ((file pases:elisp-source) &optional parent)
  (or (pases:component-loaded-internal file)
      (featurep (pases:mk-symbol (pases:component-name-internal file)))))

(pases:luna-define-method pases:unload :before ((file pases:elisp-source) &optional parent)
  (let ((module (pases:mk-symbol (pases:component-name-internal file))))
    (condition-case err
        (unload-feature module)
      (error
       (message "[pases] Caught error unloading %s: %s." module (cdr err))))))

(pases:luna-define-method pases:disable-needed ((f pases:elisp-source) &optional parent)
  (let* ((name (file-name-nondirectory (pases:component-name-internal f)))
         (el-file (expand-file-name (concat name ".el") pases:elc-dir))
         (elc-file (expand-file-name (concat name ".elc") pases:elc-dir)))
    (or (file-exists-p el-file)
        (file-exists-p elc-file))))

(pases:luna-define-method pases:disable ((f pases:elisp-source) &optional parent)
  (let* ((name (file-name-nondirectory (pases:component-name-internal f)))
         (el-file (expand-file-name (concat name ".el") pases:elc-dir))
         (elc-file (expand-file-name (concat name ".elc") pases:elc-dir)))
    (if (file-exists-p el-file) (delete-file el-file))
    (if (file-exists-p elc-file) (delete-file elc-file))))

(defmacro pases:deffile (name &rest args)
  `(progn
     (pases:put-elisp ,name
                      (pases:luna-make-entity
                       'pases:elisp-source
                       :name ,name
                       ,@(plist-put (copy-list args) :only-if nil)
                       ,@(if (not (plist-member args :compile))
                             '(:compile t))
                       ,@(if (not (plist-member args :optional))
                             '(:optional nil))
                       ,@(if (not (plist-member args :dep-op))
                             '(:dep-op
			       (quote ((pases:load-op . pases:compile-op)
				       (pases:compile-op . pases:enable-op)
                                       (pases:disable-op . pases:unload-op)))))
                       ,@(if (plist-member args :only-if)
                             `(:only-if
                               (lambda ()
                                 ,(plist-get args :only-if))))))
     (pases:get-elisp ,name)))

;; pases:texi-file
;; (pases:luna-define-class pases:texi-source (pases:source-file))
;; (pases:luna-define-internal-accessors 'pases:texi-source)

;; (pases:luna-define-method pases:compile ((texi-file pases:texi-source) &optional parent)
;;   (let* ((basedir (pases:component-pathname-internal parent))
;;          (texi-file-name (expand-file-name
;;                           (pases:component-name-internal texi-file) basedir))
;;          (info-file-name (expand-file-name
;;                           (concat (file-name-nondirectory (file-name-sans-extension texi-file-name)) ".info")
;;                           basedir)))
;;     (if (not (file-exists-p info-file-name))
;;         (with-temp-buffer
;;           (pases:debug-message "[pases] building texinfo file %s." info-file-name)
;;           (insert-file-literally texi-file-name)
;;           (texinfo-format-buffer)
;;           (write-file info-file-name)))))

;; (defmacro pases:def-texi-file (name &rest args)
;;   `(pases:luna-make-entity 'pases:texi-source
;; 		     :name ,name
;; 		     ,@args))

;; pases:info-dir
(pases:luna-define-class pases:info-dir (pases:component))
(pases:luna-define-internal-accessors 'pases:info-dir)

(pases:luna-define-method pases:load :before ((info-dir pases:info-dir) &optional parent)
  (let* ((basedir (pases:component-pathname-internal parent))
         (dir-name (expand-file-name (pases:component-name-internal info-dir)
                                     basedir)))
    (eval-after-load "info"
      `(progn
         (info-initialize)
         (add-to-list 'Info-directory-list ,dir-name)))))

(defmacro pases:def-info-dir (name &rest args)
  `(pases:luna-make-entity 'pases:info-dir
		     :name ,name
		     ,@args))
 
;; pases:module
;; A modules is a container for other components.
(pases:luna-define-class pases:module (pases:component) 
		   (components))
(pases:luna-define-internal-accessors 'pases:module)

(pases:luna-define-generic pases:operate-on-children (module op &optional reverse))
(pases:luna-define-method pases:operate-on-children ((module pases:module) op &optional reverse)
  (pases:debug-message "%s module components from %s."
                       (pases:op-name-internal op)
		       (pases:component-pathname-internal module))
  (mapc (lambda (component)
	  (pases:operate op component module))
        (if reverse
            (reverse (pases:module-components-internal module))
          (pases:module-components-internal module))))

(pases:luna-define-method pases:enable :before ((module pases:module) &rest args)
  (pases:operate-on-children module pases:enable-op))

(pases:luna-define-method pases:load :before ((module pases:module) &rest args)
  (pases:operate-on-children module pases:load-op))

(pases:luna-define-method pases:compile :before ((module pases:module) &rest args)
  (pases:operate-on-children module pases:enable-op))

(pases:luna-define-method pases:unload :before ((module pases:module) &rest args)
  (pases:operate-on-children module pases:unload-op t))

(pases:luna-define-method pases:disable :before ((module pases:module) &rest args)
  (pases:operate-on-children module pases:disable-op t))

;; pases:system
;; A system is a top level module, with a symbol for a name.
(pases:luna-define-class pases:system (pases:module)
                         (after version))
(pases:luna-define-internal-accessors 'pases:system)

(pases:luna-define-generic pases:handle-after (system op))
(pases:luna-define-method pases:handle-after ((system pases:system) op)
  (mapc (lambda (after-system)
	  (pases:oos op after-system))
	(pases:mk-list (pases:system-after-internal system))))

(pases:luna-define-method pases:enable :before ((s pases:system) &optional basedir)
  (pases:debug-message "Enabling system %s." (pases:component-name-internal s) basedir)
  (pases:handle-after s pases:enable-op))

(pases:luna-define-method pases:compile :before ((s pases:system) &optional basedir)
  (pases:debug-message "Compiling system %s." (pases:component-name-internal s) basedir)
  (pases:handle-after s pases:compile-op))

(pases:luna-define-method pases:load :before ((s pases:system) &optional basedir)
  (pases:debug-message "Loading system %s." (pases:component-name-internal s) basedir)
  (if (not (member pases:elc-dir load-path))
      (add-to-list 'load-path pases:elc-dir))
  (pases:handle-after s pases:load-op))

(defmacro pases:defsystem (name &rest args)
  `(progn
     (pases:put-system (quote ,name)
                       (pases:luna-make-entity
                        'pases:system
                        :name (symbol-name (quote ,name))
                        :pathname (file-name-directory (file-truename load-file-name))
                        ,@args
                        ,@(if (not (plist-member args :dep-op))
                              '(:dep-op
                                (quote ((pases:load-op . pases:compile-op)
                                        (pases:compile-op . pases:enable-op)
                                        (pases:disable-op . pases:unload-op)))))))
     (add-to-list 'pases:systems (quote ,name))))
