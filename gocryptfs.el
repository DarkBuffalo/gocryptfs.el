;;; gocryptfs.el --- goCryptFs Tool
;;
;; Filename: gocryptfs.el
;; Description:
;; Author: Matthias David <darkbuffalo@gnu.re>
;; Created: Tue Jun 27 15:29:11 2023 (+0200)
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (epg))

;;; Commentary:
;; This allows me to mount a private directory encrypted using gocryptfs.
;;
;;
;;; Code:

(require 'epg)

(defgroup gocrytfs nil
  "Mount a private directory encrypted using gocryptfs."
  :group 'crypto)

(defcustom gocryptfs-private-dir-name "Perso"
  "Gocryptfs private directory name."
  :type 'string
  :group 'gocryptfs)

(defcustom gocryptfs-root-dir "~/.perso.tomb/"
  "Gocryptfs root configuration directory."
  :type 'directory
  :group 'gocryptfs)

(defun gocryptfs-config-file ()
  (concat gocryptfs-root-dir "gocryptfs.conf"))

(defvar gocryptfs-buffer-name " *emacs-gocryptfs*")
(defvar gocryptfs-process-name "emacs-gocryptfs")
(defvar gocryptfs--mount-private-cmd "gocryptfs")
(defvar gocryptfs--umount-private-cmd "fusermount")


(defun gocryptfs-available-p ()
  (and (file-directory-p (expand-file-name gocryptfs-private-dir-name "~"))
       (cl-every #'file-exists-p (list gocryptfs--mount-private-cmd
                                       gocryptfs--umount-private-cmd
                                       (gocryptfs-config-file)))))

(defun gocryptfs-private-mounted-p ()
  (let ((mount (shell-command-to-string "mount")))
    (and (string-match-p (concat ".*" (expand-file-name gocryptfs-private-dir-name "~") ".*gocryptfs.*") mount)
         t)))


;;;###autoload
(defun gocryptfs-toggle-mount-private ()
  "Mount/Unmount gocryptfs' private directory."
  (interactive)
  (if (gocryptfs-private-mounted-p)
      (gocryptfs-umount-private)
    (gocryptfs-mount-private)))


;;;###autoload
(defun gocryptfs-mount-private ()
  "Mount gocryptfs' private directory."
  (interactive)

  (let ((try-again t))
    (while (and ;; In the first iteration, we try to silently mount the gocryptfs private directory,
            ;; this would succeed if the key is available in the keyring.
            (prog1 (not (zerop (shell-command gocryptfs--mount-private-cmd gocryptfs-buffer-name)))
              (message "Successfully mounted private directory."))
            (prog1 try-again (setq try-again nil)))
      (if (zerop (shell-command (concat "echo " (read-passwd "Password :") " | " gocryptfs--mount-private-cmd " " gocryptfs-root-dir " " gocryptfs-private-dir-name)))
          (message "Successfully mounted private directory.")
        (user-error "A problem occured while mounting the private directory, see %s"
                    gocryptfs-buffer-name)))))

;;;###autoload
(defun gocryptfs-umount-private ()
  "Unmount gocryptfs' private directory."
  (interactive)
  (if (zerop (shell-command (concat gocryptfs--umount-private-cmd " -u ~/" gocryptfs-private-dir-name)))
      (message "Unmounted private directory successfully.")
    (user-error "Cannot unmount the private directory, seems to be already unmounted.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gocryptfs.el ends here
