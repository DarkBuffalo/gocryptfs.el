;;; gocryptfs.el --- goCryptFs Tool -*- lexical-binding: t; -*-
;;
;; Author: DarkBuffalo <darkbuffalo@gnu.re>
;; Created: Tue Jun 27 15:29:11 2023 (+0200)
;; Version: 0.2
;; Package-Requires: ((emacs "24.4") (epg) (cl-lib))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; gocryptfs.el provides simple commands to manage multiple gocryptfs
;; encrypted directories ("vaults") directly from Emacs.
;;
;; Features:
;; - mount / unmount gocryptfs vaults
;; - lazy unmount when a vault is in use (Nautilus, Emacs, shell, etc.)
;; - initialize new gocryptfs vaults
;; - secure password handling via stdin
;; - no shell password exposure
;; - multi-vault configuration
;;; Code:

(require 'epg)
(require 'cl-lib)

(defgroup gocryptfs nil
  "Manage gocryptfs encrypted directories."
  :group 'security)

;;;; Customization

(defcustom gocryptfs-vaults
  '((:name "personal"
     :cipher-dir "~/.tomb"
     :mount-dir "~/Perso"))
  "List of gocryptfs vaults.

Each vault is defined by:
- :name       Unique identifier
- :cipher-dir Encrypted directory (contains gocryptfs.conf)
- :mount-dir  Mount point directory."
  :type '(repeat
          (plist :tag "Vault"
                 :options
                 ((:name string)
                  (:cipher-dir directory)
                  (:mount-dir directory))))
  :group 'gocryptfs)

(defcustom gocryptfs-process-name "emacs-gocryptfs"
  "Name of the gocryptfs process."
  :type 'string
  :group 'gocryptfs)

(defcustom gocryptfs-mount-command "gocryptfs"
  "Command used to mount gocryptfs vaults."
  :type 'string
  :group 'gocryptfs)

(defcustom gocryptfs-umount-command
  (or (executable-find "fusermount3")
      (executable-find "fusermount"))
  "Command used to unmount gocryptfs vaults."
  :type 'string
  :group 'gocryptfs)


(defcustom gocryptfs-buffer-name " *emacs-gocryptfs*"
  "Name of the buffer used for gocryptfs output."
  :type 'string
  :group 'gocryptfs)


;;;; Utilities

(defun gocryptfs-available-p ()
  "Return non-nil if required gocryptfs tools are available."
  (and (executable-find gocryptfs-mount-command)
       gocryptfs-umount-command))


(defun gocryptfs--select-vault ()
  "Prompt the user to select a gocryptfs vault."
  (let ((names (mapcar (lambda (v) (plist-get v :name))
                       gocryptfs-vaults)))
    (unless names
      (user-error "No gocryptfs vault configured"))
    (let* ((choice (completing-read "Vault: " names nil t))
           (vault  (cl-find choice gocryptfs-vaults
                            :key (lambda (v) (plist-get v :name))
                            :test #'string=)))
      vault)))

(defun gocryptfs-vault-valid-p (vault)
  "Return non-nil if VAULT directories exist."
  (and (file-directory-p
        (expand-file-name (plist-get vault :cipher-dir)))
       (file-directory-p
        (expand-file-name (plist-get vault :mount-dir)))))

(defun gocryptfs-vault-initialized-p (vault)
  "Return non-nil if VAULT contains a gocryptfs configuration."
  (file-exists-p
   (expand-file-name
    "gocryptfs.conf"
    (expand-file-name (plist-get vault :cipher-dir)))))

(defun gocryptfs-vault-mounted-p (vault)
  "Return non-nil if VAULT mount directory is a gocryptfs mount point."
  (let ((mount (file-truename
                (expand-file-name (plist-get vault :mount-dir)))))
    (condition-case nil
        (string-match-p
         "gocryptfs"
         (string-trim
          (car (process-lines
                "findmnt"
                "-n"
                "-o" "FSTYPE"
                "--mountpoint" mount))))
      (error nil))))


;;;; Initialization

;;;###autoload
(defun gocryptfs-init-vault ()
  "Initialize a gocryptfs vault."
  (interactive)
  (let* ((vault (gocryptfs--select-vault))
         (name  (plist-get vault :name))
         (cipher (expand-file-name (plist-get vault :cipher-dir)))
         (buffer (get-buffer-create gocryptfs-buffer-name)))

    (when (gocryptfs-vault-initialized-p vault)
      (user-error "Vault %s is already initialized" name))

    (unless (yes-or-no-p
             (format "Initialize gocryptfs vault %s? " name))
      (user-error "Initialization aborted"))

    (let ((pw1 (read-passwd "New password: "))
          (pw2 (read-passwd "Confirm password: ")))
      (unless (string= pw1 pw2)
        (user-error "Passwords do not match"))

      (with-current-buffer buffer
        (erase-buffer))
      (display-buffer buffer)

      (let ((proc (make-process
                   :name "emacs-gocryptfs-init"
                   :buffer buffer
                   :stderr buffer
                   :command (list gocryptfs-mount-command "-init" cipher)
                   :noquery t)))
        (process-send-string proc (concat pw1 "\n" pw1 "\n"))
        (process-send-eof proc)
        (message "Initializing vault %s..." name)))))


;;;; Mount / Unmount


;;;###autoload
(defun gocryptfs-mount ()
  "Mount a gocryptfs vault."
  (interactive)

  (unless (gocryptfs-available-p)
    (user-error "gocryptfs tools are not available"))

  (let* ((vault (gocryptfs--select-vault))
         (name  (plist-get vault :name))
         (cipher (expand-file-name (plist-get vault :cipher-dir)))
         (mount  (expand-file-name (plist-get vault :mount-dir)))
         (buffer (get-buffer-create gocryptfs-buffer-name)))

    (unless (gocryptfs-vault-valid-p vault)
      (user-error "Vault %s is misconfigured" name))

    (unless (gocryptfs-vault-initialized-p vault)
      (user-error "Vault %s is not initialized" name))

    (when (gocryptfs-vault-mounted-p vault)
      (user-error "Vault %s is already mounted" name))

    (let ((password (read-passwd (format "Password for %s: " name))))
      (with-current-buffer buffer
        (erase-buffer))

      (make-process
       :name "emacs-gocryptfs"
       :buffer buffer
       :stderr buffer
       :command (list gocryptfs-mount-command cipher mount)
       :noquery t)

      (process-send-string "emacs-gocryptfs"
                           (concat password "\n"))
      (process-send-eof "emacs-gocryptfs")

      (message "Mounting vault %s..." name))))


;;;###autoload
(defun gocryptfs-umount ()
  "Unmount a gocryptfs vault."
  (interactive)

  (unless (gocryptfs-available-p)
    (user-error "gocryptfs tools are not available"))

  (let* ((vault (gocryptfs--select-vault))
         (name  (plist-get vault :name))
         (mount (expand-file-name (plist-get vault :mount-dir))))

    (unless (gocryptfs-vault-mounted-p vault)
      (user-error "Vault %s is not mounted" name))

    ;; normal unmount
    (cond
     ((zerop
       (process-file gocryptfs-umount-command
                     nil nil nil
                     "-u" mount))
      (message "Unmounted vault %s" name))

     ;; lazy unmount if busy
     ((yes-or-no-p
       (format "Vault %s is in use. Force lazy unmount? " name))
      (if (zerop
           (process-file gocryptfs-umount-command
                         nil nil nil
                         "-u" "-z" mount))
          (message "Lazy-unmounted vault %s" name)
        (user-error "Failed to unmount vault %s" name)))

     (t
      (message "Unmount aborted")))))


;;;###autoload
(defun gocryptfs-toggle ()
  "Toggle mount state of a gocryptfs vault."
  (interactive)
  (let ((vault (gocryptfs--select-vault)))
    (if (gocryptfs-vault-mounted-p vault)
        (gocryptfs-umount)
      (gocryptfs-mount))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gocryptfs.el ends here
