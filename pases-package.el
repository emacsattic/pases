(require 'tar-mode)

(defun pases:install-package (file)
  (interactive "f")
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

