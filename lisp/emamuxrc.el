(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages '((emamux . t))))

(let ((default-directory  (concat "/ssh:beowulf@ted:")))
  (process-file "ls"))

