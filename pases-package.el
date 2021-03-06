;;; pases-package.el --- packaging & system definition for Emacs
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

;; A "package" is a zip file that can be installed by the user. It
;; should be named package-version.pases. It should unpack in the
;; current directory (i.e., it is a tar-bomb). An installed package is
;; one installed in the user's pases:package-dirs. A disable packaged
;; is an installed package that ends in _; an enabled package is any
;; undisabled, installed package. There should only be one version of
;; any package installed at once.

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

(defun pases:merge-into-alist (alist elm)
  "Merge elm into alist, appeneding the values of elm to any
existing values in alist with the same key, sorting them."
  (let* ((key (car elm))
	 (val (cdr elm))
	 (old (assoc key alist)))
    (if (null old)
	(cons elm alist)
      ;; assq is OK because we are using the actual old key, which is eq to itself
      (let ((new-alist (assq-delete-all (car old) alist))
            (new-val (sort (if (listp val)
                               (append val (cdr old))
                             (append (list val) (cdr old))) 'string<)))
	(cons (cons key new-val)
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

(defun pases:unzip (zip dir)
  (let ((args (list "unzip" nil nil nil
                    "-d" (expand-file-name dir)
                    (expand-file-name zip))))
    (if (not (eq 0 (apply 'call-process args)))
        (error "[pases] Could not unzip %s into %s." zip dir))))

(defun pases:package-name (name-version)
  (if (string-match "^\\(.+\\)-\\([^-]+?\\)_?+$" name-version)
      (match-string-no-properties 1 name-version)))

(defun pases:package-version (name-version)
  (if (string-match "^\\(.+\\)-\\([^-]+?\\)_?$" name-version)
      (match-string-no-properties 2 name-version)))
    
(defun pases:check-old-versions (name version)
  (if (pases:package-enabled? name version)
      (error "[pases] package already installed"))
  (if (pases:package-disabled? name version)
      (if (y-or-n-p
           (format "Package %s (%s) installed but disabled; enable? "
                   name (pases:system-version-internal (pases:get-system (pases:mk-symbol name)))))
          (pases:enable-package name version)))
  (let ((old-version (pases:enabled-version name)))
    (if old-version
        (if (y-or-n-p
             (format "Another version (%s) of %s installed; uninstall? "
                     old-version name))
            (pases:uninstall-package name old-version)
          (progn
            (message "OK, disabling old version.")
            (pases:disable-package name))))))

(defun pases:enabled-package-path (name)
  "Returns the path of the currently enabled version of package
with NAME, or nil if no current version is enabled."
  (let ((version (pases:enabled-version name))
        path)
    (mapc (lambda (dir)
	    (let ((fullpath (expand-file-name 
			     (concat name "-" version) dir)))
	      (if (file-directory-p fullpath)
                  (setq path fullpath))))
	  pases:package-dirs)
    path))

(defun pases:installed-package-path (name version)
  "Returns the path of a given version of package with NAME, or
nil if no current version is installed."
  (if (string= (pases:enabled-version name)
               version)
      (pases:enabled-package-path name)
    (pases:disabled-package-path name version)))

(defun pases:disabled-package-path (name version)
  "Returns the path of a disabled version of package with NAME
and VERSION, or nil if no such disabled version exists."
  (let (path)
    (mapc (lambda (dir)
	    (let ((fullpath (expand-file-name 
			     (concat name "-" version "_") dir)))
	      (if (file-directory-p fullpath)
                  (setq path fullpath))))
	  pases:package-dirs)
    path))

(defun pases:package-enabled? (name &optional version)
  "Return t if the package with NAME is installed & enabled; nil otherwise.
If optional argument VERSION is supplied, checks the version."
  (let ((info (assoc name (pases:enabled-packages))))
    (and info (or (not version)
                  (string= version (cdr info))))))

(defun pases:package-disabled? (name &optional version)
  "Return t if the package with NAME is installed & disabled; nil otherwise.
If optional argument VERSION is supplied, checks the version."
  (let ((info (assoc name (pases:disabled-packages))))
    (and info (or (not version)
                  (and (member version (cdr info))
                       t)))))

(defun pases:completing-read-enabled-package ()
  (completing-read "Package: " 
                   (mapcar (lambda (p) (car p)) (pases:enabled-packages))))

(defun pases:completing-read-package-version (packages)
  (let* ((name (completing-read 
                "Package: " 
                (mapcar (lambda (p) (car p)) packages)))
         (versions (cdr (assoc name packages)))
         (version (pases:maybe-completing-read-version versions)))
    (list name version)))

(defun pases:maybe-completing-read-version (versions)
  (if (not (listp versions))
      versions
    (if (eq 1 (length versions))
      (car versions)
      (completing-read "Version: " versions))))

(defun pases:completing-read-installed-package ()
  (pases:completing-read-package-version
   (pases:installed-packages)))

(defun pases:completing-read-disabled-package ()
  (pases:completing-read-package-version
   (pases:disabled-packages)))
  
(defun pases:disable-package (name)
  (interactive (list (pases:completing-read-enabled-package)))
  (let ((path (pases:enabled-package-path name))
        (version (pases:enabled-version name)))
    (pases:oos pases:disable-op (pases:mk-symbol name))
    (pases:undef-system (pases:mk-symbol name))
    (rename-file path (concat path "_"))
    (message "[pases] Disabled %s (%s). Restart emacs in case of problems."
             name version)))

(defun pases:enable-package (name version)
  (interactive (pases:completing-read-disabled-package))
  (if (not (pases:package-disabled? name version))
      (error "[pases] Package %s (%s) is not disabled." name version))
  (if (pases:package-enabled? name version)
      (error "[pases] Package %s (%s) is already enabled." name version))
  (let ((old-version (pases:enabled-version name))
        (path (pases:disabled-package-path name version)))
    (if old-version
        (if (y-or-n-p
             (format "%s (%s) enabled; disable? " name old-version))
            (pases:disable-package name)
          (error "[pases] Cannot enable %s (%s) without disabling other version (%s)"
                 name version old-version)))
    (rename-file path (substring path 0 -1))
    (pases:read-package-dir (pases:enabled-package-path name))
    (pases:oos pases:load-op (pases:mk-symbol name))
    (message "[pases] Enabled %s (%s). Restart emacs in case of problems."
             name version)))

(defun pases:install-package (&optional package)
  "Install a pases package."
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
    (if (not (file-directory-p pases:package-install-dir))
        (mkdir pases:package-install-dir t))
    (pases:check-old-versions name version)
    (make-directory package-install-dir)
    (pases:unzip package-path package-install-dir)
    (condition-case err
	(progn
	  (pases:check-package-dir-depends package-install-dir)
	  (pases:read-package-dir package-install-dir)
	  (pases:oos pases:load-op (pases:mk-symbol name))
	  (message "[pases] Successfully installed %s (%s) to %s." name version
		   pases:package-install-dir))
      (depend-error
       (pases:uninstall-package name version t)
       (message "[pases] Could not install %s because dependencies not met: %s."
		name (cdr err)))
      (error
       (pases:uninstall-package name version t)
       (message "[pases] Error installing %s: %s"
		name (cdr err))))))

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

(defun pases:installed-packages ()
  "Return an alist of all installed (disabled and enabled)
packages with a list of their versions; e.g.:
  ((\"name\" . (\"1.0\", \"2.0\")))"
  (pases:merge-alists (pases:disabled-packages)
                      (pases:enabled-packages)))

(defun pases:enabled-version (package-name)
  "Given a package name, returns the currently enabled version of
that package."
  (cdr (assoc-string package-name (pases:enabled-packages))))

(defun pases:uninstall-package (name version &optional force)
  "Uninstall a package."
  (interactive
   (pases:completing-read-installed-package))
  (let* ((package-path (pases:installed-package-path name version))
         (was-enabled (string= version
                           (pases:enabled-version name)))
         (recurse-delete (lambda (d)
                           (loop for f in (directory-files d)
                                 do (let ((path (expand-file-name f d)))
                                      (if (and (not (string= f "."))
                                               (not (string= f "..")))
                                          (if (file-directory-p path)
                                              (funcall recurse-delete path)
                                            (delete-file path)))))
                           (delete-directory d))))
    (if (or force
	    (y-or-n-p
	     (format "Are you sure that you want to remove all files in %s? " package-path)))
        (progn
          (if (pases:system-defined?  (pases:mk-symbol name))
               (progn
                 (pases:oos pases:disable-op (pases:mk-symbol name))
                 (pases:undef-system (pases:mk-symbol name))))
          (funcall recurse-delete package-path)
          (message "[pases] Uninstalled %s (%s). Restart emacs in case of problems."
                   name version)
          (let ((other-versions (assoc name (pases:disabled-packages))))
            (if (and was-enabled other-versions)
                (if (y-or-n-p (format "Other version of %s exists; enable? " name))
                    (pases:enable-package
                     name
                     (pases:maybe-completing-read-version (cdr other-versions))))))))))

(defun pases:read-packages ()
  (mapc 'pases:read-packages-dir pases:package-dirs))

(defun pases:read-packages-dir (dir)
  "Process a directory of packages, reading each package in it."
  (mapc (lambda (package)
          (pases:read-package-dir (expand-file-name 
                                   (concat (car package) "-" (cdr package)) dir)))
        (pases:enabled-packages-in-dir dir)))

(defun pases:read-package-dir (package-dir)
  "Read a package directory, loading its pasdef file."
  (let ((files (directory-files package-dir t "\\.pasdef$")))
    (if (not (null files))
	(pases:read-sysdef (car files)))))

(defun pases:check-system-depends (system-name)
  (let* ((system (pases:get-system system-name))
        (dep-check (pases:system-check-dependencies-internal system)))
    (mapc (lambda (depend)
            (if (not (pases:system-defined? depend))
                (signal 'depend-error
                        (list (format "%s not a defined system." depend)))))
          (pases:system-after-internal system))
    (if dep-check
        (funcall dep-check))))

(defun pases:check-package-dir-depends (package-dir)
  (let ((orig-pases-sytems pases:systems)
	(pases:systems (copy-list pases:systems)))
    (unwind-protect
	(progn
	  (pases:read-package-dir package-dir)
	  (mapc (lambda (system-name)
		  (if (not (memq system-name orig-pases-sytems))
		      (pases:check-system-depends system-name)))
		pases:systems))
      (setq pases:systems orig-pases-sytems))))

(if (not (file-exists-p pases:package-install-dir))
    (make-directory pases:package-install-dir))

;; Read all packages.
(pases:read-packages)
