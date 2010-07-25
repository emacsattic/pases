;;; pases.el --- packaging & system definition for Emacs
;;
;; Copyright (C) 2008 Erik Hetzner
;;
;; Author: Erik Hetzner <ehetzner@gmail.com>
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

(add-to-list 'load-path (file-name-directory load-file-name))
(load "luna")

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

(defvar pases:debug t)

(defun pases:byte-recompile-file (file compile-thunk)
  (if (file-newer-than-file-p file (concat file "c"))
      (progn
	(if compile-thunk (funcall compile-thunk))
	(byte-compile-file file))
    t))

(defsubst pases:debug-message (&rest args)
  (if pases:debug
      (apply 'message args)))

(defsubst pases:mk-symbol (s)
  (if (stringp s) (intern s) s))

(defsubst pases:mk-list (l)
  (if (listp l) l (list l)))

(defvar pases:file-blacklist '()
  "Blacklist of files to not load.")

;; (defadvice load (around pases:load-blacklist
;;                         (file))
;;   "Disable some files from loading."
;;   (if (not (member file pases:do-file-blacklist))
;;       ad-do-it))

;; (ad-activate 'load)

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

;; pases:op
(pases:luna-define-class pases:op nil (func name))
(pases:luna-define-internal-accessors 'pases:op)
(pases:luna-define-generic pases:operate (op component &optional parent)) 
(pases:luna-define-method pases:operate ((op pases:op) component &optional parent)
  (let ((dep-op (cdr (assoc (pases:op-name-internal op) (pases:component-dep-op-internal component)))))
    (if dep-op
        (funcall 'pases:operate (symbol-value dep-op) component parent))
    (funcall (pases:op-func-internal op) component parent)))

(setq pases:load-op (pases:luna-make-entity 'pases:op
                                      :func 'pases:load
                                      :name 'pases:load-op))
(setq pases:unload-op (pases:luna-make-entity 'pases:op
                                        :func 'pases:unload 
                                        :name 'pases:unload-op))
(setq pases:compile-op (pases:luna-make-entity 'pases:op :func 'pases:compile :name 'pases:compile-op))
  
;; pases:component
(pases:luna-define-class pases:component nil 
		   (name version dependencies pathname dep-op loaded))

(pases:luna-define-internal-accessors 'pases:component)

(pases:luna-define-generic pases:load (component &optional parent))
(pases:luna-define-generic pases:unload (component &optional parent))
(pases:luna-define-generic pases:compile (component &optional parent))
(pases:luna-define-generic pases:operate (component operation &optional args))

;; pases:source-file
(pases:luna-define-class pases:source-file (pases:component)
		   (load compile compile-thunk optional))

(pases:luna-define-internal-accessors 'pases:source-file)

(pases:luna-define-method pases:load ((c pases:component) &rest args)
  (pases:component-set-loaded-internal c t))

(pases:luna-define-method pases:unload ((c pases:component) &rest args)
  (pases:component-set-loaded-internal c nil))

;;(pases:luna-define-method initialize-instance :before ((file pases:source-file) &rest args)
;;  (pases:component-set-dep-op-internal file
;;                                       '((pases:load-op . pases:compile-op)))
;;  (pases:source-file-set-compile-internal file t))

;; pases:elisp-source
(pases:luna-define-class pases:elisp-source
                   (pases:source-file)
		   (compile-after
                    only-if))

(pases:luna-define-internal-accessors 'pases:elisp-source)

(pases:luna-define-method pases:load :before ((file pases:elisp-source) &optional parent)
  (let ((basedir (pases:component-pathname-internal parent)))
    (pases:debug-message "[pases] loading %s from %s." (pases:component-name-internal c) basedir)
    (if (pases:source-file-load-internal file)
        (if (pases:component-pathname-internal file)
            (load (pases:component-pathname-internal file))
          (load (expand-file-name (pases:component-name-internal file)
                                  basedir))))))

(pases:luna-define-method pases:unload :before ((file pases:elisp-source) &optional parent)
  (let ((module (pases:mk-symbol (pases:component-name-internal file))))
    (if (featurep module)
        (unload-feature module))))

(pases:luna-define-method pases:compile ((f pases:elisp-source) &optional parent)
  (let ((basedir (pases:component-pathname-internal parent))
        (compile-after (pases:elisp-source-compile-after-internal f))
        (only-if (pases:elisp-source-only-if-internal f))
        (targetdir))
    (if (and (pases:source-file-compile-internal f)
             (or (not compile-after)
                 (pases:modules-installed-p compile-after))
             (or (not only-if)
                 (funcall only-if)))
        (progn
          (let ((path (expand-file-name
                       (concat (pases:component-name-internal f) ".el")
                       basedir)))
            (pases:debug-message "[pases] maybe compiling %s." path)
            (if (pases:byte-recompile-file path (pases:source-file-compile-thunk-internal f))
                (if targetdir
                    (let ((target-path
                           (expand-file-name 
                            (concat (pases:component-name-internal f) ".elc")
                            targetdir)))
                      (rename-file (concat path "c") target-path)))
              (if (not (pases:source-file-optional-internal f))
                  (error "Error compiling %s " (pases:component-name-internal f))
                (message "Error compiling %s " (pases:component-name-internal f)))))))))

(defmacro pases:deffile (name &rest args)
  `(pases:luna-make-entity 'pases:elisp-source
                     :name ,name
                     ,@args
                     ,@(if (not (plist-member args :compile))
                           '(:compile t))
                     ,@(if (not (plist-member args :optional))
                           '(:optional nil))
                     ,@(if (not (plist-member args :dep-op))
                           '(:dep-op (quote ((pases:load-op . pases:compile-op)))))
                     ,@(if (plist-member args :only-if)
                           `(:only-if
                             (lambda ()
                               ,(plist-get args :only-if))))))

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
    (message "%s" dir-name)
    (eval-after-load "info"
      `(progn
         (info-initialize)
         (add-to-list 'Info-directory-list ,dir-name)))))

(defmacro pases:def-info-dir (name &rest args)
  `(pases:luna-make-entity 'pases:info-dir
		     :name ,name
		     ,@args))
 
;; pases:source-dir
(pases:luna-define-class pases:source-dir (pases:component))

(pases:luna-define-internal-accessors 'pases:source-dir)

(pases:luna-define-method pases:load :before ((dir pases:source-dir) &optional parent)
  (let* ((basedir (pases:component-pathname-internal parent))
         (fullpath (expand-file-name (pases:component-name-internal dir) basedir)))
    (pases:debug-message "[pases] loading dir: %s." fullpath)
    (add-to-list 'load-path fullpath)))

(pases:luna-define-method pases:unload ((dir pases:source-dir) &optional parent)
  (let* ((basedir (pases:component-pathname-internal parent))
         (fullpath (expand-file-name (pases:component-name-internal dir) basedir))
         (mem (member fullpath load-path)))
    (if mem
        (setq load-path (delq (car mem) load-path)))))

(defmacro pases:defdir (name &rest args)
  `(pases:luna-make-entity 'pases:source-dir
		     :name ,name
		     ,@args))

;; pases:module
(pases:luna-define-class pases:module (pases:component) 
		   (components
		    default-component-class
		    if-component-dep-fails
		    serial))

(pases:luna-define-internal-accessors 'pases:module)

(pases:luna-define-method pases:load :before ((m pases:module) &optional parent)
  (pases:debug-message "[pases] loading module components from %s."
		       (pases:component-pathname-internal m))
  (mapc (lambda (c)
	  (pases:operate pases:load-op c m))
          (pases:module-components-internal m)))

(pases:luna-define-method pases:unload :before ((m pases:module) &optional parent)
  (pases:debug-message "[pases] unloading module components from %s."
		       (pases:component-pathname-internal m))
  (mapc (lambda (c)
	  (pases:operate pases:unload-op c m))
        (reverse (pases:module-components-internal m))))

;; pases:system
(pases:luna-define-class pases:system (pases:module)
		   (load-after version))

(pases:luna-define-internal-accessors 'pases:system)

(pases:luna-define-method pases:load :before ((s pases:system) &optional basedir)
  (pases:debug-message "[pases] loading system %s." (pases:component-name-internal s) basedir)
  (mapc (lambda (s)
	  (pases:oos pases:load-op s))
	(pases:mk-list (pases:system-load-after-internal s))))

(defmacro pases:defsystem (name &rest args)
  `(progn
     (add-to-list 'pases:systems (quote ,name))
     (put (quote ,name) 'pases:system
	  (pases:luna-make-entity 'pases:system
			  :name (symbol-name (quote ,name))
			  :pathname (file-name-directory (file-truename load-file-name))
			  ,@args))))

(defun pases:read-sysdef (file)
  (load-file file))
   
(defun pases:oos (op sys)
  (let ((system-real (get sys 'pases:system)))
    (if (not system-real)
        (error "[pases] System %s is not defined." sys) 
      (funcall 'pases:operate op system-real))))

(defvar pases:systems '())

(put 'emacs 'pases:system
     (pases:luna-make-entity 
      'pases:system
      :name "emacs"
      :version emacs-version))

 (defun pases:load-all ()
  (mapc (lambda (s)
	  (pases:oos pases:load-op s))
	pases:systems))

(load "pases-package")

;; Load all systems.
(pases:load-all)
