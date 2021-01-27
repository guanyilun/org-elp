;;; org-elp.el --- Preview latex equations in org mode while editing

;; Author: Yilun Guan
;; URL: https://github.com/guanyilun/org-elp

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Preview latex equations in org mode in a seperate buffer while
;; editing.  Possible configurations:
;;
;; To adjust split size:
;; (setq org-elp-split-size 30)
;;
;; To adjust buffer name
;; (setq org-elp-buffer-name "*Equation Live*")
;;
;; To adjust idle time to run latex preview
;; (setq org-elp-idle-time 0.5)

;;; Code:

;; (require 'posframe)  ;; not working

(defgroup org-elp nil
  "org-elp customizable variables."
  :group 'org)

(defcustom org-elp-buffer-name "*Equation Live Preview*"
  "Buffer name used to show the previewed equation."
  :type  'string
  :group 'org-elp)

(defcustom org-elp-split-size 20
  "Split window size for displaying equation, this number is the number of lines in the main text remaining after the split."
  :type  'integer
  :group 'org-elp)

(defcustom org-elp-idle-time 0.5
  "Idle time after editing to wait before creating the fragment."
  :type  'float
  :group 'org-elp)

(defvar org-elp--timer nil)
(defvar org-elp--org-buffer nil)
(defvar org-elp--preview-buffer nil)

(defun org-elp-preview ()
  "Preview the equation at point in buffer defined in `org-elp-buffer-name'."
  (let ((datum (org-element-context)))
    (and (memq (org-element-type datum) '(latex-environment latex-fragment))
         (let* ((beg (org-element-property :begin datum))
                (end (org-element-property :end datum))
                (text (buffer-substring-no-properties beg end)))
           (with-current-buffer org-elp--preview-buffer
             (let ((inhibit-read-only t))
               (erase-buffer) (insert (replace-regexp-in-string "\n$" "" text))
               (org-preview-latex-fragment)))))))

(defun org-elp-open-buffer ()
  "Open the preview buffer."
  (split-window-vertically org-elp-split-size)
  (other-window 1)
  (switch-to-buffer org-elp-buffer-name)
  (special-mode)
  (other-window -1))

;;;###autoload
(defun org-elp-activate ()
  "Activate previewing.  This launches the idle timer and preview equations after idle time defined in `org-elp-idle-time'."
  (interactive)
  (message "Activating org-elp")
  (setq org-elp--org-buffer (current-buffer)
        org-elp--preview-buffer (get-buffer-create org-elp-buffer-name))
  (org-elp-open-buffer)
  (setq org-elp--timer (run-with-idle-timer
                        org-elp-idle-time t 'org-elp-preview)))

;;;###autoload
(defun org-elp-deactivate ()
  "Deactivate previewing.  This remove the idle timer."
  (interactive)
  (delete-other-windows)
  (message "Deactivating org-elp")
  (cancel-function-timers 'org-elp-preview))

;;;###autoload
(define-minor-mode org-elp
  "org-elp mode: display latex fragment while typing"
  :lighter " org-elp"
  :group   'org-elp
  :require 'org-elp
  (if org-elp
      (org-elp-activate)
    (org-elp-deactivate)))

(provide 'org-elp)
;;; org-elp ends here
