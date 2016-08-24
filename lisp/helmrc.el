(defun my-recentf-show-details (file)
  (append
   (list
    (file-name-nondirectory file))
   (list
    (format "(in `%s')" (file-name-directory file)))
   ))

(defun my-recentf-one-by-one-filter (candidate) 
         (my-recentf-show-details(candidate) . candidate)) 

(helm-make-source "Recentf" 'helm-recentf-source 
        :fuzzy-match helm-recentf-fuzzy-match 
        :filter-one-by-one #'my-recentf-one-by-one-filter)

(helm :sources 'helm-source-recentf
        :ff-transformer-show-only-basename nil
        :buffer "*helm recentf*")
