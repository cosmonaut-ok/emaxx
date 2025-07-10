;; This file is for loading system packages,
;; defined in `deps.el` declarative file

(let ((emacs-user-deps (expand-file-name (locate-user-emacs-file "deps.el"))))
  (if (file-readable-p emacs-user-deps)
      (load emacs-user-deps)
    (error "sys-deps-loader: Can not load deps file %s" emacs-user-deps)))

(when (not (equal system-type 'gnu/linux))
  (error "sys-deps-loader: OS type %s is not supported" system-type))

;; check if sudo command is present in the system
(defconst const/sudo-cmd "sudo")

(when (equal (shell-command-to-string (concat "which " const/sudo-cmd)) "")
  (error "sys-deps-loader: Can not find %s executable" const/sudo-cmd))

(require 'cl-lib)

(defvar var/sys-check-cmd nil
  "Command used to check if a package is installed.")

(defvar var/sys-check-cmd-option nil
  "Options for the package check command.")

(defvar var/sys-install-cmd nil
  "Command used to install packages.")

(defvar var/sys-install-cmd-option nil
  "Options for the package install command.")

(defvar var/linux-name nil
  "Options for the linux distribution name.")

(defconst get-linux-distro-pkg-map
  '(("debian"
     :check-cmd "dpkg"
     :check-opt "-s"
     :install-cmd "apt-get"
     :install-opt "-y --no-install-recommends install")

    ("ubuntu"
     :check-cmd "dpkg"
     :check-opt "-s"
     :install-cmd "apt-get"
     :install-opt "-y --no-install-recommends install")

    ("linuxmint"
     :check-cmd "dpkg"
     :check-opt "-s"
     :install-cmd "apt-get"
     :install-opt "-y --no-install-recommends install")

    ("arch"
     :check-cmd "pacman"
     :check-opt "-Qi"
     :install-cmd "pacman"
     :install-opt "--noconfirm -S")

    ("manjaro"
     :check-cmd "pacman"
     :check-opt "-Qi"
     :install-cmd "pacman"
     :install-opt "--noconfirm -S")

    ("fedora"
     :check-cmd "rpm"
     :check-opt "-q"
     :install-cmd "dnf"
     :install-opt "-y install")

    ("rhel"
     :check-cmd "rpm"
     :check-opt "-q"
     :install-cmd "dnf"
     :install-opt "-y install")

    ("centos"
     :check-cmd "rpm"
     :check-opt "-q"
     :install-cmd "dnf"
     :install-opt "-y install")

    ("opensuse"
     :check-cmd "rpm"
     :check-opt "-q"
     :install-cmd "zypper"
     :install-opt "--non-interactive install")

    ("alpine"
     :check-cmd "apk"
     :check-opt "info -e"
     :install-cmd "apk"
     :install-opt "add --no-cache"))
  "Mapping from Linux distro ID to package management commands.")

(defun get-linux-distro ()
  "Return an alist describing the current Linux distribution.

Primary source: /etc/os-release
Fallback: lsb_release command

Returns nil if not on GNU/Linux or no information could be obtained."
  (when (eq system-type 'gnu/linux)
    (or (get-linux-distro--os-release)
        (get-linux-distro--lsb-release))))

(defun get-linux-distro--os-release ()
  "Parse /etc/os-release and return an alist, or nil."
  (when (file-readable-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (let (result)
        (goto-char (point-min))
        (while (re-search-forward
                "^\\([^=\n]+\\)=\\(\"\\([^\"]*\\)\"\\|\\(.+\\)\\)$"
                nil t)
          (let ((key (match-string 1))
                (value (or (match-string 3)
                           (match-string 4))))
            (push (cons key value) result)))
        (and result result)))))

(defun get-linux-distro--lsb-release ()
  "Use lsb_release to obtain distro info. Returns an alist or nil."
  (when (executable-find "lsb_release")
    (with-temp-buffer
      (when (zerop (call-process "lsb_release" nil t nil "-a"))
        (goto-char (point-min))
        (let (result)
          (while (re-search-forward
                  "^\\([^:\n]+\\):[ \t]+\\(.+\\)$"
                  nil t)
            (let ((key (match-string 1))
                  (value (string-trim (match-string 2))))
              ;; Normalize keys to os-release style
              (push
               (cons
                (pcase key
                  ("Distributor ID" "ID")
                  ("Description"    "PRETTY_NAME")
                  ("Release"        "VERSION_ID")
                  ("Codename"       "VERSION_CODENAME")
                  (_ key))
                value)
               result)))
          result)))))


(defun get-linux-distro-id ()
  "Return distro ID or nil."
  (cdr (assoc "ID" (get-linux-distro))))

(defun get-linux-distro-id-like ()
  "Return list of ID_LIKE entries."
  (let ((val (cdr (assoc "ID_LIKE" (get-linux-distro)))))
    (when val
      (split-string val "[ \t]+" t))))


(defun get-linux-distro-find-pkg-config ()
  "Find package manager config by ID or ID_LIKE."
  (let* ((info (get-linux-distro))
         (id (cdr (assoc "ID" info)))
         (id-like (get-linux-distro-id-like)))
    (or
     ;; 1. Exact ID match
     (assoc id get-linux-distro-pkg-map)

     ;; 2. Fallback via ID_LIKE
     (cl-find-if
      (lambda (entry)
        (member (car entry) id-like))
      get-linux-distro-pkg-map))))

(defun setup-sys-pkg-vars ()
  "Set package management variables based on Linux distro ID / ID_LIKE."
  (let ((cfg (get-linux-distro-find-pkg-config)))
    (if (not cfg)
        (message "No package manager config for this distro")
      (setq
       var/linux-name             (car cfg)
       var/sys-check-cmd          (plist-get (cdr cfg) :check-cmd)
       var/sys-check-cmd-option   (plist-get (cdr cfg) :check-opt)
       var/sys-install-cmd        (plist-get (cdr cfg) :install-cmd)
       var/sys-install-cmd-option (plist-get (cdr cfg) :install-opt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setup-sys-pkg-vars)

(when (equal (shell-command-to-string (concat "which " var/sys-check-cmd)) "")
  (error "sys-deps-loader: Can not find %s executable" var/sys-check-cmd))

(when (equal (shell-command-to-string (concat "which " var/sys-install-cmd)) "")
  (error "sys-deps-loader: Can not find %s executable" var/sys-install-cmd))

(let ((sudo-pass ""))
  (dolist (sd const/sys-deps)
    (when (equal (car sd) var/linux-name)
      (dolist (pkg (cadr sd))
        (if (eq 0 (shell-command (concat var/sys-check-cmd " " var/sys-check-cmd-option " " pkg)))
            (message "Package %s is already installed" pkg)
          (progn
            (message "Installing package %s" pkg)

            (when (equal sudo-pass "")
              (setq sudo-pass (read-passwd "Enter sudo password: ")))

            (if (eq 0 (shell-command (concat "echo '" sudo-pass "' | sudo -S " var/sys-install-cmd " " var/sys-install-cmd-option " " pkg)))
                (message "Installing package %s [DONE]" pkg)
              (error "Installing package %s [FAILED]" pkg)
              )
            )
          )
        )
      )
    )
  )
