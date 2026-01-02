;;; gocryptfs.el --- goCryptFs Tool -*- lexical-binding: t; -*-
;;
;; Filename: gocryptfs.el
;; Description:
;; Author: Matthias David <darkbuffalo@gnu.re>
;; Created: Tue Jun 27 15:29:11 2023 (+0200)
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (epg) (cl-lib))

;;; Commentary:
;; This allows me to mount a private directory encrypted using gocryptfs.
;;
;;
;;; Code:

(require 'epg)
(require 'cl-lib)

(defgroup gocryptfs nil
  "Mount a private directory encrypted using gocryptfs."
  :group 'crypto)

(defcustom gocryptfs-private-dir-name "~/Perso"
  "Gocryptfs private directory name."
  :type 'string
  :group 'gocryptfs)

(defcustom gocryptfs-root-dir "~/.tomb/"
  "Gocryptfs root configuration directory."
  :type 'directory
  :group 'gocryptfs)

(defun gocryptfs-config-file ()
  (expand-file-name "gocryptfs.conf" gocryptfs-root-dir))

(defvar gocryptfs-buffer-name " *emacs-gocryptfs*")

(defvar gocryptfs-process-name "emacs-gocryptfs")

(defcustom gocryptfs--mount-private-cmd "gocryptfs"
  "Command used to mount gocryptfs")

(defcustom gocryptfs--umount-private-cmd "fusermount3"
  "Command used to unmount gocryptfs")

(defun gocryptfs-available-p ()
"Return non-nil if gocryptfs is correctly installed and configured."
  (and (file-directory-p (expand-file-name gocryptfs-private-dir-name "~"))
       (executable-find gocryptfs--mount-private-cmd)
       (executable-find gocryptfs--umount-private-cmd)
       (file-exists-p (gocryptfs-config-file))))

(defun gocryptfs-private-mounted-p ()
  "Return non-nil if the gocryptfs private directory is mounted."
  (let ((target (expand-file-name gocryptfs-private-dir-name "~")))
    (condition-case nil
        (and (process-lines "findmnt"
                             "-n"
                             "-o" "FSTYPE"
                             "--target" target)
             (member "fuse.gocryptfs"
                     (process-lines "findmnt"
                                    "-n"
                                    "-o" "FSTYPE"
                                    "--target" target)))
      (error nil))))


;;;###autoload
(defun gocryptfs-toggle-mount-private ()
  "Mount/Unmount gocryptfs' private directory."
  (interactive)
  (if (gocryptfs-private-mounted-p)
      (gocryptfs-umount-private)
    (gocryptfs-mount-private)))


;;;###autoload
(defun gocryptfs-mount-private ()
  "Mount gocryptfs' private directory without blocking Emacs."
  (interactive)

  (let* ((buffer   (get-buffer-create gocryptfs-buffer-name))
         (root     (expand-file-name gocryptfs-root-dir))
         (target   (expand-file-name gocryptfs-private-dir-name "~"))
         (password (read-passwd "Password: "))
         (command  (list gocryptfs--mount-private-cmd
                         root
                         target))
         proc)

    (with-current-buffer buffer
      (erase-buffer))

    (setq proc
          (make-process
           :name gocryptfs-process-name
           :buffer buffer
           :stderr buffer
           :command command
           :noquery t))

    ;; envoyer le mot de passe immédiatement
    (process-send-string proc (concat password "\n"))
    (process-send-eof proc)

    (message "Mounting private directory...")))


;;;###autoload
(defun gocryptfs-umount-private ()
  "Unmount gocryptfs' private directory."
  (interactive)
  (if (zerop (shell-command (concat gocryptfs--umount-private-cmd " -u ~/" gocryptfs-private-dir-name)))
      (message "Unmounted private directory successfully.")
    (user-error "Cannot unmount the private directory, seems to be already unmounted.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gocryptfs.el ends here
