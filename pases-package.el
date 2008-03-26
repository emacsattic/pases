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
	 (default-directory (expand-file-name
			     package-name
			     pases:package-dir)))
    (if (file-exists-p default-directory)
	(error "[pases] package already installed?")
      (make-directory default-directory))
    (save-excursion
      (with-temp-buffer
	(insert-file-contents package-path)
	(tar-summarize-buffer)
	(tar-untar-buffer)))
    (pases:load-sysdef-dir default-directory)
    (pases:load-all)
    (message "[pases] Successfully installed %s." package-name)))
