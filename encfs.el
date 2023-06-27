;;; encfs.el --- Encfs
;;
;; Filename: encfs.el
;; Description:
;; Author: Matthias David
;; Maintainer: Matthias David
;; Created: Tue Jun 27 15:29:11 2023 (+0200)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; This allows me to mouont a private directory encrypted using encfs.
;;
;;
;;; Code:

(require 'epg)

(defcustom encfs-private-dir-name "Perso"
  "encfs private directory name"
  :type 'string)

(defcustom encfs-root-dir "~/.Encrypted/"
  "encfs root configuration directory"
  :type 'directory)

;;;###encfs-autoload
(defconst encfs-config-file ".encfs6.xml"
  "Encfs configuration file name.")

(defcustom encfs-passphrase-file (concat encfs-root-dir "my-pass.gpg")
  "GPG encrypted file containing encfs password.")

(defvar encfs-buffer-name " *emacs-encfs*")
(defvar encfs-process-name "emacs-encfs")
(defvar encfs--mount-private-cmd "/sbin/encfs")
(defvar encfs--umount-private-cmd "/sbin/fusermount")

(defun encfs--wrapped-passphrase-file ()
  (concat encfs-root-dir "wrapped-passphrase"))

(defun encfs--mount-passphrase-sig-file ()
  (concat encfs-root-dir encfs-config-file))


(defun encfs-available-p ()
  (and (file-directory-p (expand-file-name encfs-private-dir-name "~"))
       (cl-every #'file-exists-p (list encfs--mount-private-cmd
                                       encfs--umount-private-cmd
                                       (encfs-config-file)))))

(defun encfs-private-mounted-p ()
  (let ((mount (shell-command-to-string "mount")))
    (and (string-match-p (concat ".*" (expand-file-name encfs-private-dir-name "~") ".*encfs.*") mount)
         t)))

(defun encfs--encrypt-filenames-p ()
  (/= 1 (with-temp-buffer
          (insert-file-contents (encfs--mount-passphrase-sig-file))
          (count-lines (point-min) (point-max)))))

(defun encfs--passphrase ()
  (string-trim-right
   (epg-decrypt-file
    (epg-make-context)
    (expand-file-name encfs-passphrase-file)
    nil)
   "[\n\r]+"))

;;;###autoload
(defun encfs-toggle-mount-private ()
  "Mount/Unmount encfs' private directory."
  (interactive)
  (if (encfs-private-mounted-p)
      (encfs-umount-private)
    (encfs-mount-private)))

(defun encfs--unwrap-passphrase-command ()
  (format
   (if (encfs--encrypt-filenames-p)
       "encfs-insert-wrapped-passphrase-into-keyring %s '%s'"
     "encfs-unwrap-passphrase %s '%s' | encfs-add-passphrase -")
   (encfs--wrapped-passphrase-file) (encfs--passphrase)))

;;;###autoload
(defun encfs-mount-private ()
  "Mount encfs' private directory."
  (interactive)
  (if (not (and (file-exists-p (concat encfs-root-dir encfs-config-file))))
      (user-error "Encrypted private directory \"%s\" is not setup properly."
                  encfs-private-dir-name)
    (let ((try-again t))
      (message "Encrypted filenames mode [%s]" (if (encfs--encrypt-filenames-p) "ON" "OFF"))
      (while (and ;; In the first iteration, we try to silently mount the encfs private directory,
              ;; this would succeed if the key is available in the keyring.
              (prog1 (not (zerop (shell-command encfs--mount-private-cmd encfs-buffer-name)))
                (message "Successfully mounted private directory."))
              (prog1 try-again (setq try-again nil)))
        (if (zerop (shell-command (encfs--unwrap-passphrase-command) encfs-buffer-name))
            (message "Successfully mounted private directory.")
          (user-error "A problem occured while mounting the private directory, see %s"
                      encfs-buffer-name))))))

;;;###autoload
(defun encfs-umount-private ()
  "Unmount encfs' private directory."
  (interactive)
  (if (zerop (shell-command encfs--umount-private-cmd encfs-buffer-name))
      (message "Unmounted private directory successfully.")
    (user-error "Cannot unmount the private directory, seems to be already unmounted.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; encfs.el ends here
