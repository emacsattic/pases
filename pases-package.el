(require 'tar-mode)
(require 'jka-compr)

(unless (member "\\.pases\\'" (mapcar (lambda (vec) (elt vec 0))
                                     jka-compr-compression-info-list))
    (add-to-list 'jka-compr-compression-info-list
               ["\\.pases\\'" nil nil nil
                "uncompressing"  "gzip"  ("-c" "-q" "-d")
                nil t "^_\213"])
  (jka-compr-update))

(defun pases:install-package (file)
  (interactive "fPackage to install: ")
  (let* ((package-name (file-name-nondirectory
                        (file-name-sans-extension file)))
	 (default-directory (expand-file-name 
			     package-name
			     pases:package-dir)))
    (if (file-exists-p default-directory)
	(error "[pases] package already installed?")
      (make-directory default-directory))
    (save-excursion
      (with-temp-buffer
	(insert-file-contents file)
	(tar-summarize-buffer)
	(tar-untar-buffer)))
    (pases:load-sysdef-dir default-directory)
    (pases:load-all)))
