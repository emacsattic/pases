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

(unless (member "\\.pases\\'" (mapcar (lambda (vec) (elt vec 0))
                                     jka-compr-compression-info-list))
    (add-to-list 'jka-compr-compression-info-list
               ["\\.pases\\'" nil nil nil
                "uncompressing"  "gzip"  ("-c" "-q" "-d")
                nil t "^_\213"])
    (when (jka-compr-installed-p)
      (jka-compr-uninstall)
      (jka-compr-install)))

(defun pases:install-package (&optional package)
  (interactive)
  (let* ((package-path-p (lambda (f) (or (file-directory-p f) 
                                         (string= (file-name-extension f)
                                                  "pases"))))
         (package-path (if package
                           package
                         (read-file-name "Package to install: " nil nil
                                         t nil package-path-p)))
         (package-name (file-name-nondirectory
                        (file-name-sans-extension package-path)))
	 (package-install-dir (expand-file-name
                               package-name
                               pases:package-dir)))
    (if (file-exists-p package-install-dir)
	(error "[pases] package already installed?")
      (make-directory package-install-dir))
    (save-excursion
      (with-temp-buffer
	(insert-file-contents package-path)
        (let ((default-directory package-install-dir))
          (tar-summarize-buffer)
          (tar-untar-buffer))))
    (pases:load-sysdef-dir package-install-dir)
    (pases:load-all)
    (message "[pases] Successfully installed %s." package-install-dir)))

(defun pases:uninstall-package (package-name)
  (interactive
   (list (completing-read "Package: " (mapcar 'symbol-name pases:systems))))
  (message package-name)
  (let* ((package (intern package-name))
         (package-def (get package 'pases:system))
         (package-path (pases:component-pathname-internal package-def))
         (recurse-delete (lambda (d)
                           (loop for f in (directory-files d)
                                 do (let ((path (expand-file-name f d)))
                                      (if (and (not (string= f "."))
                                               (not (string= f "..")))
                                          (if (file-directory-p path)
                                              (funcall recurse-delete path)
                                            (delete-file path)))))
                           (delete-directory d))))
    (print (directory-files package-path t "^[^\\.+]$"))
    (if (yes-or-no-p
       (format "Are you sure that you want to remove all files in %s? " package-path))
        (progn
          (funcall recurse-delete package-path)
          (delq package pases:systems)
          (message "The package %s has been removed. However, please note that the package will remain loaded into your running Emacs until restart."
                   package-name)))))
