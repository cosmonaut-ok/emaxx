(defconst const/self-dir (file-name-directory load-file-name))
(load (concat const/self-dir "" "configure-repos.el"))
(load (concat const/self-dir "rc/" "rc.el"))
