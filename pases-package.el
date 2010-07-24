;;; pases-package.el --- packaging & system definition for Emacs
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

(require 'tar-mode)
(require 'jka-compr)

;; A "package" is a gzipped tar file that can be installed by the
;; user. It should be named package-version.pases. It should unpack in
;; the current directory (i.e., it is a tar-bomb). An installed
;; package is one installed in the user's pases:package-dirs.  A
;; disable packaged is an installed package that ends in _; an enabled
;; package is any undisabled, installed package. There should only be
;; one version of any package installed at once.

(defcustom pases:package-dirs
  (list (expand-file-name "~/.pases.d/"))
  "Directories to load pases packages from."
  :type '(list directory)
  :group 'pases)

(defcustom pases:package-install-dir
  (expand-file-name "~/.pases.d/")
  "Directory to install user packages in."
  :type 'directory
  :group 'pases)

(defcustom pases:tar-bin
  "tar"
  "Program to use for untarring packages (in Emacs 23)."
  :type 'string
  :group 'pases)

(unless (member "\\.pases\\'" (mapcar (lambda (vec) (elt vec 0))
                                     jka-compr-compression-info-list))
    (add-to-list 'jka-compr-compression-info-list
               ["\\.pases\\'" nil nil nil
                "uncompressing"  "gzip"  ("-c" "-q" "-d")
                nil t "^_\213"])
    (when (jka-compr-installed-p)
      (jka-compr-uninstall)
      (jka-compr-install)))

(defun pases:untar-22 (tar dir)
  (save-excursion
    (with-temp-buffer
      (insert-file-contents tar)
      (let ((default-directory dir))
        (tar-summarize-buffer)
        (tar-untar-buffer)))))
  
(defun pases:untar-23 (tar dir)
  (let ((args (list pases:tar-bin nil nil nil
		    "x" "-C" (expand-file-name dir)
		    "-zf" (expand-file-name tar))))
  (if (not (eq 0 (apply 'call-process args)))
      (error "Could not untar %s into %s." tar dir))))

(defun pases:untar (tar dir)
  (if (eq emacs-major-version 23)
      (pases:untar-23 tar dir)
    (pases:untar-22 tar dir)))

(defun pases:package-name (name-version)
  (if (string-match "^\\(.*\\)-[^-]+$" name-version)
      (match-string-no-properties 1 name-version)))

(defun pases:package-version (name-version)
  (if (string-match "\\([^-]+[^_]\\)_?$" name-version)
      (match-string-no-properties 1 name-version)))
    
(defun pases:check-old-versions (name version)
  (if (pases:find-package-path name version)
      (error "[pases] package already installed?"))
  (let ((old-version (pases:enabled-package-version name)))
    (if old-version
        (if (yes-or-no-p
             (format "Another version (%s) of %s installed; remove? "
                     old-version name))
            (pases:uninstall-package name)
          (progn
            (message "OK, disabling old version.")
            (pases:disable-package name old-version))))))

(defun pases:find-package-path (name version)
  "Find the dir path of a package with a given name & version."
  (let ((path))
    (mapc (lambda (dir)
	    (let ((fullpath (expand-file-name 
			     (concat name "-" version)
			     dir)))
	      (if (file-directory-p fullpath)
		  (setq path fullpath))))
	  pases:package-dirs)
    path))

(defun pases:disable-package (name version)
  (let ((path (pases:find-package-path name version)))
    (rename-file path (concat path "_"))))

(defun pases:install-package (&optional package)
  (interactive)
  (let* ((package-path-p (lambda (f) (or (file-directory-p f) 
                                         (string= (file-name-extension f)
                                                  "pases"))))
         (package-path (if package package
                         (read-file-name "Package to install: " nil nil
                                         t nil package-path-p)))
         (name-version (file-name-nondirectory
                                    (file-name-sans-extension package-path)))
         (name (pases:package-name name-version))
         (version (pases:package-version name-version))
	 (package-install-dir (expand-file-name
                               name-version
                               pases:package-install-dir)))
    (pases:check-old-versions name version)
    (make-directory package-install-dir)
    (pases:untar package-path package-install-dir)
    (pases:read-package package-install-dir)
    (pases:load-all)
    (message "[pases] Successfully installed %s (%s) to %s." name version
             pases:package-install-dir)))

(defun pases:enabled-packages-in-dir (package-dir)
  (let ((packages))
    (mapc (lambda (dir)
            (if (file-directory-p (expand-file-name dir package-dir))
                (setq packages (cons (cons (pases:package-name dir) (pases:package-version dir)) packages))))
	  (directory-files package-dir nil "^[^\\.].+[^_]$"))
    packages))
  
(defun pases:enabled-packages ()
  "Return an alist of installed, enabled packages and their version."
  (let ((enabled-packages '()))
    (mapcar (lambda (dir)
	      (setq enabled-packages
		    (append enabled-packages
			    (pases:enabled-packages-in-dir dir))))
	    pases:package-dirs)
    enabled-packages))

(defun pases:merge-into-alist (alist elm)
  "Merge elm into alist, appeneding the values of elm to any
existing values in alist with the same key, sorting them. elm
should be a list, not a cons cell."
  (let* ((key (car elm))
	 (val (cdr elm))
	 (old (assoc key alist)))
    (if (null old)
	(cons elm alist)
      ;; assq is OK because we are using the actual old key, which is eq to itself
      (let ((new-alist (assq-delete-all (car old) alist)))
	(cons (cons key
		    (sort (append val (cdr old)) 'string<))
	      new-alist)))))

(defun pases:merge-alists (a b)
  "Merge two alists, with pases:merge-into-alist."
  (let ((new-alist (copy-alist a)))
    (mapcar
     (lambda (elm)
       (setq new-alist 
	     (pases:merge-into-alist new-alist elm)))
     b)
    new-alist))

(defun pases:disabled-packages-in-dir (package-dir)
  "Return an alist of disabled packages in dir with a list of their versions; e.g.:
 ((\"name\" . (\"1.0\", \"2.0\")))"
  (let ((disabled-packages '()))
    (mapcar (lambda (dir)
	      (setq disabled-packages (pases:merge-into-alist
				       disabled-packages
				       (list (pases:package-name dir)
					     (pases:package-version dir)))))
	    (directory-files package-dir nil "^[^\\.].+[_]$"))
    disabled-packages))

(defun pases:disabled-packages ()
  "Return an alist of disabled package names with a list of their versions; e.g.:
 ((\"name\" . (\"1.0\", \"2.0\")))"
  (let ((disabled-packages '()))
    (mapcar
     (lambda (dir)
       (setq disabled-packages
	     (pases:merge-alists disabled-packages
				 (pases:disabled-packages-in-dir dir))))
     pases:package-dirs)
    disabled-packages))

(defun pases:enabled-package-version (package-name)
  "Given a package name, returns the currently enabled version of
that package."
  (cdr (assoc-string package-name (pases:enabled-packages))))

(defun pases:enabled-package-path (name)
  (pases:find-package-path name (pases:enabled-package-version name)))

(defun pases:uninstall-package (package-name)
  (interactive
   (list (completing-read "Package: " 
			  (mapcar (lambda (p) (car p)) (pases:enabled-packages)))))
  (let* ((package-path (pases:enabled-package-path package-name))
         (recurse-delete (lambda (d)
                           (loop for f in (directory-files d)
                                 do (let ((path (expand-file-name f d)))
                                      (if (and (not (string= f "."))
                                               (not (string= f "..")))
                                          (if (file-directory-p path)
                                              (funcall recurse-delete path)
                                            (delete-file path)))))
                           (delete-directory d))))
    (if (yes-or-no-p
       (format "Are you sure that you want to remove all files in %s? " package-path))
        (progn
          (funcall recurse-delete package-path)
          (message "The package %s has been removed. However, please note that the package will remain loaded into your running Emacs until restart."
                   package-name)))))

(defun pases:read-packages ()
  (mapc 'pases:read-packages-dir pases:package-dirs))

(defun pases:read-packages-dir (dir)
  "Process a directory of packages, reading each package in it."
  (mapc (lambda (package)
          (pases:read-package (expand-file-name 
                               (concat (car package) "-" (cdr package)) dir)))
        (pases:enabled-packages-in-dir dir)))

(defun pases:read-package (package-dir)
  "Read a package directory, loading its pasdef file."
  (let ((files (directory-files package-dir t "\\.pasdef$")))
    (if (not (null files))
	(pases:read-sysdef (car files)))))
  
(if (not (file-exists-p pases:package-install-dir))
    (make-directory pases:package-install-dir))

;; Read all packages.
(pases:read-packages)
