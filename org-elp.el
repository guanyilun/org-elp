;;; org-elp.el --- Preview latex equations in org mode while editing -*- lexical-binding: t; -*-

;; Author: Yilun Guan
;; URL: https://github.com/guanyilun/org-elp
;; Keywords: lisp, tex, org
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (posframe "1.4.0"))

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
;; Preview latex equations in org mode in a separate buffer or popup
;; while editing.  Possible configurations:
;;
;; To set the display mode ('buffer or 'popup):
;; (setq org-elp-display-mode 'popup)
;;
;; To set the fraction of the window taken up by the previewing buffer
;; (setq org-elp-split-fraction 0.2)
;;
;; To adjust buffer name
;; (setq org-elp-buffer-name "*Equation Live*")
;;
;; To adjust idle time to run latex preview
;; (setq org-elp-idle-time 0.5)
;;
;; Launch or deactivate the previewing with
;; M-x org-elp-mode
;;

;;; Code:

(require 'org-element)
(require 'posframe)
(require 'cl-lib)

(defgroup org-elp nil
  "org-elp customizable variables."
  :group 'org)

(defcustom org-elp-display-mode 'buffer
  "Display mode for preview: 'buffer for split window, 'popup for floating frame."
  :type '(choice (const :tag "Buffer mode" buffer)
                 (const :tag "Popup mode" popup))
  :group 'org-elp)

(defcustom org-elp-buffer-name "*Equation Live Preview*"
  "Buffer name used to show the previewed equation."
  :type  'string
  :group 'org-elp)

(defcustom org-elp-split-fraction 0.2
  "Fraction of the window taken up by the previewing buffer."
  :type  'float
  :group 'org-elp)

(defcustom org-elp-idle-time 0.5
  "Idle time after editing to wait before creating the fragment."
  :type  'float
  :group 'org-elp)

(defcustom org-elp-popup-poshandler 'posframe-poshandler-point-bottom-left-corner
  "Poshandler function for popup positioning."
  :type 'function
  :group 'org-elp)

(defcustom org-elp-popup-timeout nil
  "Seconds before popup auto-hides. nil means no timeout."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'org-elp)

(defvar org-elp--timer nil
  "A variable that keeps track of the idle timer.")

(defvar org-elp--preview-buffer nil
  "Buffer used for generating and previewing equations.")

(defvar org-elp--popup-buffer " *org-elp-popup*"
  "Buffer name for popup display.")

(defvar org-elp--source-buffer nil
  "The Org buffer where org-elp was activated.")

(defvar org-elp--popup-timer nil
  "Timer for auto-hiding popup.")

(defvar org-elp--poll-timer nil
  "Timer for polling overlay completion.")

(defvar org-elp--poll-count 0
  "Counter for polling attempts.")

(defvar org-elp--last-text nil
  "Last previewed text to avoid redundant re-renders.")

(defvar org-elp--element-end nil
  "End position of the LaTeX element currently being previewed.")

(defcustom org-elp-poll-interval 0.1
  "Interval in seconds between polling attempts for overlay."
  :type 'float
  :group 'org-elp)

(defcustom org-elp-poll-max-attempts 30
  "Maximum number of polling attempts before giving up."
  :type 'integer
  :group 'org-elp)

;;;###autoload
(defun org-elp--preview ()
  "Preview the equation at point."
  (when (and (buffer-live-p org-elp--source-buffer)
             (eq (current-buffer) org-elp--source-buffer)
             (eq major-mode 'org-mode))
    (let ((datum (org-element-context)))
      (if (memq (org-element-type datum) '(latex-environment latex-fragment))
          (let* ((beg (org-element-property :begin datum))
                 (end (org-element-property :end datum))
                 (text (buffer-substring-no-properties beg end)))
            ;; Store the end of the element, trimmed of trailing whitespace
            (setq org-elp--element-end
                  (save-excursion
                    (goto-char end)
                    (skip-chars-backward " \t\n")
                    (point)))
            ;; Skip if text hasn't changed (avoid redundant re-renders)
            (unless (equal text org-elp--last-text)
              (setq org-elp--last-text text)
              (if (eq org-elp-display-mode 'popup)
                  (org-elp--generate-image-async text)
                (org-elp--show-in-buffer text))))
        ;; NOT on a latex fragment → hide popup
        (when (eq org-elp-display-mode 'popup)
          (org-elp--hide-popup)
          (setq org-elp--last-text nil
                org-elp--element-end nil))))))

(defun org-elp--generate-image-async (text)
  "Generate LaTeX preview image from TEXT asynchronously with polling."
  ;; Cancel any existing poll timer
  (when org-elp--poll-timer
    (cancel-timer org-elp--poll-timer))
  (setq org-elp--poll-count 0)
  ;; Prepare the preview buffer
  (with-current-buffer org-elp--preview-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      ;; Clear any existing overlays
      (dolist (ov (overlays-in (point-min) (point-max)))
        (delete-overlay ov))
      ;; Generate preview (no prefix arg - '(4) would CLEAR previews!)
      ;; Use org-latex-preview (Org 9.7+) or fallback to org-toggle-latex-fragment
      (if (fboundp 'org-latex-preview)
          (org-latex-preview)
        (org-toggle-latex-fragment))))
  ;; Start polling for overlay
  (setq org-elp--poll-timer
        (run-at-time org-elp-poll-interval org-elp-poll-interval
                     #'org-elp--poll-for-image)))

(defun org-elp--poll-for-image ()
  "Poll for the image overlay to be ready."
  (cl-incf org-elp--poll-count)
  (with-current-buffer org-elp--preview-buffer
    (let* ((ov (cl-find-if
                (lambda (o)
                  (let ((d (overlay-get o 'display)))
                    (and d (listp d) (eq (car d) 'image))))
                (overlays-in (point-min) (point-max))))
           (disp (and ov (overlay-get ov 'display))))
      (cond
       ;; Image ready → show it
       (disp
        (cancel-timer org-elp--poll-timer)
        (setq org-elp--poll-timer nil)
        (org-elp--show-popup disp))
       ;; Timeout → give up
       ((>= org-elp--poll-count org-elp-poll-max-attempts)
        (cancel-timer org-elp--poll-timer)
        (setq org-elp--poll-timer nil))))))

(defun org-elp--show-popup (image-spec)
  "Show IMAGE-SPEC in a posframe popup."
  ;; Cancel previous hide timer
  (when org-elp--popup-timer
    (cancel-timer org-elp--popup-timer))
  (let* ((img (append image-spec nil))
         (img-size (ignore-errors (image-size img t)))) ; pixel dimensions
    ;; Display in popup buffer
    (with-current-buffer (get-buffer-create org-elp--popup-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image img)))
    ;; Show posframe and resize to image dimensions
    (let ((frame (posframe-show org-elp--popup-buffer
                                ;; Use end of equation element instead of cursor position
                                :position (with-current-buffer org-elp--source-buffer
                                            (or org-elp--element-end (point)))
                                :poshandler org-elp-popup-poshandler
                                :background-color (face-attribute 'default :background nil t)
                                :foreground-color (face-attribute 'default :foreground nil t)
                                :border-width 1
                                :border-color (face-attribute 'shadow :foreground nil t)
                                :left-fringe 0
                                :right-fringe 0)))
      ;; Resize frame to actual image size (in pixels)
      (when (and frame img-size)
        (set-frame-size frame
                        (+ (car img-size) 4)   ; width + small padding
                        (+ (cdr img-size) 4)   ; height + small padding
                        t)))                   ; t = pixelwise
    ;; Set auto-hide timer if configured
    (when org-elp-popup-timeout
      (setq org-elp--popup-timer
            (run-at-time org-elp-popup-timeout nil #'org-elp--hide-popup)))))

(defun org-elp--hide-popup ()
  "Hide the popup frame."
  (posframe-hide org-elp--popup-buffer)
  (when org-elp--popup-timer
    (cancel-timer org-elp--popup-timer)
    (setq org-elp--popup-timer nil)))

(defun org-elp--show-in-buffer (text)
  "Show TEXT in the preview buffer."
  (with-current-buffer org-elp--preview-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (replace-regexp-in-string "\n$" "" text))
      ;; Use org-latex-preview (Org 9.7+) or fallback to org-toggle-latex-fragment
      (if (fboundp 'org-latex-preview)
          (org-latex-preview)
        (org-toggle-latex-fragment)))))

(defun org-elp--open-buffer ()
  "Open the preview buffer in a split window."
  (split-window-vertically (floor (* (- 1 org-elp-split-fraction)
                                     (window-height))))
  (other-window 1)
  (switch-to-buffer org-elp-buffer-name)
  (org-mode)
  (read-only-mode 1)
  (other-window -1))

(defun org-elp--close-buffer ()
  "Close the preview buffer and window."
  (when (buffer-live-p org-elp--preview-buffer)
    (with-current-buffer org-elp--preview-buffer
      (when (get-buffer-window)
        (delete-window (get-buffer-window)))
      (kill-buffer))))

;;;###autoload
(defun org-elp-activate ()
  "Activate previewing and idle timer."
  (interactive)
  (message "Activating org-elp (%s mode)" org-elp-display-mode)
  (setq org-elp--source-buffer (current-buffer))
  (setq org-elp--preview-buffer (get-buffer-create org-elp-buffer-name))
  ;; Initialize preview buffer with org-mode
  (with-current-buffer org-elp--preview-buffer
    (org-mode))
  ;; Open buffer window if in buffer mode
  (when (eq org-elp-display-mode 'buffer)
    (org-elp--open-buffer))
  ;; Start idle timer
  (setq org-elp--timer (run-with-idle-timer
                        org-elp-idle-time t #'org-elp--preview)))

;;;###autoload
(defun org-elp-deactivate ()
  "Deactivate previewing and remove the idle timer."
  (interactive)
  ;; Cancel poll timer if running
  (when org-elp--poll-timer
    (cancel-timer org-elp--poll-timer)
    (setq org-elp--poll-timer nil))
  ;; Hide popup if visible
  (when (eq org-elp-display-mode 'popup)
    (org-elp--hide-popup)
    ;; Clean up popup buffer with posframe-delete (removes frame + buffer)
    (when (get-buffer org-elp--popup-buffer)
      (posframe-delete org-elp--popup-buffer)))
  ;; Close buffer if in buffer mode
  (when (eq org-elp-display-mode 'buffer)
    (org-elp--close-buffer))
  ;; Kill preview buffer if it exists
  (when (buffer-live-p org-elp--preview-buffer)
    (kill-buffer org-elp--preview-buffer))
  ;; Cancel timer
  (cancel-function-timers #'org-elp--preview)
  (setq org-elp--timer nil
        org-elp--source-buffer nil
        org-elp--last-text nil
        org-elp--element-end nil
        org-elp--preview-buffer nil)
  (message "Deactivating org-elp"))

;;;###autoload
(define-minor-mode org-elp-mode
  "org-elp mode: display latex fragment while typing.

When enabled, LaTeX fragments at point are previewed in real-time.
The preview can be displayed in a split buffer or a floating popup,
controlled by `org-elp-display-mode'."
  :lighter " org-elp"
  :group   'org-elp
  (if org-elp-mode
      (org-elp-activate)
    (org-elp-deactivate)))


(provide 'org-elp)
;;; org-elp.el ends here
