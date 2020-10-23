;; org-equation-live-preview.el 	-*- lexical-binding: t -*-

;; Author: Yilun Guan
;; URL: https://github.com/guanyilun/org-equation-live-preview

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
;; editing. Possible configurations:
;;
;; To adjust split size:
;; (setq org-equation-live-preview-split-size 20)
;;
;; To adjust buffer name
;; (setq org-equation-live-preview-buffer-name "*Equation Live*")
;;
;; To adjust idle time to run latex preview
;; (setq org-equation-live-preview-idle-time 0.5)

;;; Code:

(defvar org-equation-live-preview-buffer-name "*Equation Live*")
(defvar org-equation-live-preview-split-size 20)
(defvar org-equation-live-preview-idle-time 0.5)
(defvar org-equation-live-preview--timer)
(defvar org-equation-live-preview--org-buffer)
(defvar org-equation-live-preview--preview-buffer)

(defun org-equation-live-preview--preview ()
  (interactive)
  (let ((datum (org-element-context)))
    (and (memq (org-element-type datum) '(latex-environment latex-fragment))
         (let* ((beg (org-element-property :begin datum))
                (end (org-element-property :end datum))
                (text (buffer-substring-no-properties beg end)))
           (with-current-buffer org-equation-live-preview--preview-buffer
             (let ((inhibit-read-only t))
               (erase-buffer) (insert (replace-regexp-in-string "\n$" "" text))
               (org-preview-latex-fragment)))))))

(defun org-equation-live-preview--open-buffer ()
  (interactive)
  (setq org-equation-live-preview--org-buffer (current-buffer))
  (let ((buffer-name org-equation-live-preview-buffer-name))
    (setq org-equation-live-preview--preview-buffer
          (get-buffer-create buffer-name))
    (split-window-vertically org-equation-live-preview-split-size)
    (other-window 1)
    (switch-to-buffer buffer-name)
    (special-mode)
    (other-window -1)))

(defun org-equation-live-preview--activate ()
  (interactive)
  (org-equation-live-preview--open-buffer)
  (message "Activating org-equation-live-preview")
  (setq org-equation-live-preview--timer
        (run-with-idle-timer org-equation-live-preview-idle-time t 'org-equation-live-preview--preview)))

(defun org-equation-live-preview--deactivate ()
  (interactive)
  (delete-other-windows)
  (message "Deactivating org-equation-live-preview")
  (cancel-function-timers 'org-equation-live-preview--preview))

(define-minor-mode live-equation
  "Toggle live-equation on or off."
  :lighter nil
  (if live-equation
      (org-equation-live-preview--activate)
    (org-equation-live-preview--deactivate)))
(global-set-key (kbd "<f9>") 'live-equation)

(provide 'org-equation-live-preview)
;; org-equation-live-preview ends here
