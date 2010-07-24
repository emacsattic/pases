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
		   (load compile compile-thunk optional))

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
  `(luna-make-entity 'pases:elisp-source
		     :name ,name
                     ,@(if (not (plist-get args :compile))
                          '(:compile t))
                     ,@(if (not (plist-get args :optional))
                          '(:optional nil))
                     ,@(if (not (plist-get args :dep-op))
                          '(:dep-op (quote ((pases:load-op . pases:compile-op)))))
                     ,@args))

;; pases:texi-file
;; (luna-define-class pases:texi-source (pases:source-file))
;; (luna-define-internal-accessors 'pases:texi-source)

;; (luna-define-method pases:compile ((texi-file pases:texi-source) &optional parent)
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
;;   `(luna-make-entity 'pases:texi-source
;; 		     :name ,name
;; 		     ,@args))

;; pases:info-dir
(luna-define-class pases:info-dir (pases:component))
(luna-define-internal-accessors 'pases:info-dir)

(luna-define-method pases:load ((info-dir pases:info-dir) &optional parent)
  (let* ((basedir (pases:component-pathname-internal parent))
         (dir-name (expand-file-name (pases:component-name-internal info-dir)
                                     basedir)))
    (message "%s" dir-name)
    (eval-after-load "info"
      `(progn
         (info-initialize)
         (add-to-list 'Info-directory-list ,dir-name)))))

(defmacro pases:def-info-dir (name &rest args)
  `(luna-make-entity 'pases:info-dir
		     :name ,name
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
	  (pases:oos pases:load-op s))
	(pases:system-depends-internal s)))

(defmacro pases:defsystem (name &rest args)
  `(progn
     (add-to-list 'pases:systems (quote ,name))
     (put (quote ,name) 'pases:system
	  (luna-make-entity 'pases:system
			  :name (symbol-name (quote ,name))
			  :pathname (file-name-directory (file-truename load-file-name))
			  ,@args))))

(defun pases:read-sysdef (file)
  (load-file file))
   
(defun pases:oos (op sys)
  (let ((system-real (get sys 'pases:system)))
    (if (not system-real)
        (error "[pases] System %s is not loaded." sys) 
      (funcall 'pases:operate op system-real))))

(defvar pases:systems '())

(put 'emacs 'pases:system
     (luna-make-entity 
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
