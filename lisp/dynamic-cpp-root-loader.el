(require 'projectile)
(require 'dash)
(require 'ede)

;; TODO: create weigh function which considers length of path and
;;       amount of names like "inc", "include", "sdk", "lib" etc
(defun my-best-match (dir file-name)
  (let* ((default-directory dir)
         (all-files  (projectile-current-project-files))
         (good-files (--filter (string-match file-name it) all-files)))
    (when (not (null good-files))
      (expand-file-name (car good-files) dir))))

(defun my-is-cpp-file (file)
  (let ((ext (file-name-extension file)))
    (--any (equal ext it) '("h" "hpp" "c" "cpp"))))

(defun my-guess-cpp-proj (dir)
  (let* ((default-directory dir)
         (all-files (projectile-current-project-files))
         (all-files-cnt (length all-files))
         (cpp-files-cnt (-count 'my-is-cpp-file all-files))
         (cpp-percents (/ (* 100 cpp-files-cnt) all-files-cnt)))
    (or (> cpp-files-cnt 100)
        (> cpp-percents  20))))

(defun my-locate (filename root-dir)
  (my-best-match root-dir filename))

(defun my-file-for-dir (&optional dir)
  (let* ((default-directory dir)
         (proj (projectile-project-p)))
    (when  proj
      (let* ((proj-file-name   (expand-file-name ".cpp-project" proj))
             (proj-file-exists (file-exists-p proj-file-name)))
        (when (or proj-file-exists
                  (my-guess-cpp-proj dir))
          (when (not proj-file-exists)
            (with-temp-buffer (write-file proj-file-name)))
          proj-file-name)))))

(defun my-project-root (dir)
  (let ((projfile (my-file-for-dir (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

(defun my-load (dir)
  (ede-cpp-root-project "some-cool-project"
                        :file (my-file-for-dir dir)
                        :locate-fcn 'my-locate))

(ede-add-project-autoload
 (ede-project-autoload "dynamic-cpp-root"
                       :name "dynamic cpp root"
                       :file 'ede/cpp-root
                       :proj-file 'my-file-for-dir
                       :proj-root 'my-project-root
                       :load-type 'my-load
                       :proj-root-dirmatch "*" ;; have no idea what is it
                       :class-sym 'ede-cpp-root-project
                       :new-p nil
                       :safe-p t))

(provide 'dynamic-cpp-projects.el)