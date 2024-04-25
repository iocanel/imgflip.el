;;; imgflip.el --- imgflip meme generator  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Ioannis Canellos
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;         http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 




;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;;; Code:
(require 'request)
(require 'json)

(defvar imgflip-username user-login-name "The imgflip username.")
(defvar imgflip-password nil "The imgflip password.")


(defcustom imgflip-font "impact" "The imgflip font."
  :type 'string
  :options '("impact" "arial")
  :group 'imgflip)

(defcustom imgflip-max-font-size "50px" "The maximum font size in pixels." :group 'imgflip)

(defcustom imgflip-download-dir "~/Downloads/imgflip/" "The imgflip download directory."
  :type 'string
  :group 'imgflip)

(defun imgflip-get-top-templates ()
  (let ((result ())
        (retries 3))  ;; Adding a retry count
    (while (and (not result) (> retries 0))
    (request "https://imgflip.com/popular_meme_ids"
      :type "GET"
      :sync t
      :parser 'buffer-string
      :success (cl-function (lambda (&key data &allow-other-keys) (setq result (imgflip--scrap-template-alist data))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Error retrieving templates: %S" error-thrown)
                            (setq retries (1- retries)))))
    (unless result
      (message "Retrying...")))
    result))

(defun imgflip-caption-image (template-or-string top-text &optional bottom-text)
  "Create a caption using the specified TEMPLATE-OR-STRING, TOP-TEXT optionally BOTTOM-TEXT to create a meme. Reutrn the image url."
  (let ((post-data `(("template_id" . ,(imgflip--to-template template-or-string))
                ("username" . ,imgflip-username)
                ("password" . ,imgflip-password)))
        (url nil))
    (when top-text
      (setq post-data (add-to-list 'post-data `("text0" . ,top-text))))

    (when bottom-text
      (setq post-data (add-to-list 'post-data `("text1" . ,bottom-text))))

    (request "https://api.imgflip.com/caption_image"
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :sync t
      :params post-data
      ;:data (json-encode post-data)
      :parser 'json-read
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (setq url (cdr (assoc 'url (assoc-default 'data data))))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys) (message "Got error: %S" error-thrown))))
      url))

(defun imgflip-download-caption-image (template-id top-text &optional bottom-text)
  "Create a caption and download the image using the specified TEMPLATE-ID, TOP-TEXT optionally BOTTOM-TEXT to create a meme. Reutrn the path to the image."
  (let ((filename (concat imgflip-download-dir (format "%s-%s-%s.jpg" template-id
                                                     (replace-regexp-in-string "[ ]" "-" top-text) 
                                                     (replace-regexp-in-string "[ ]" "-" bottom-text)))))
    (url-copy-file (imgflip-caption-image template-id top-text bottom-text) filename t)
    filename))

;;
;; Utils
;;
(defun imgflip--scrap-template-alist (html)
  "Converts a string HTML into a alist containing description and template id."
  (let ((result ()))
    (with-temp-buffer
      (goto-char 0)
      (insert html)
      (goto-char 0)
      (while (re-search-forward "<tr><td>\\([0-9]+\\)</td><td>\\([^<]+\\)" nil t)
        (let ((template-id (match-string 1))
              (description (match-string 2)))
          (setq result (add-to-list 'result `(,description . ,template-id))))))
    result))
                    
(defun imgflip--to-template (template-or-string)
  "Return the template-id that matches TEMPLATE-OR-STRING. If TEMPLATE-OR-STRING is a number use it as is. If it is a string, find a matching template-id."
  (cond ((numberp template-or-string) template-or-string)
        ((stringp template-or-string) (cdr (imgflip--find-matching-template (imgflip-get-top-templates) template-or-string)))
        (t nil)))
  
(defun imgflip--find-matching-template (template-alist target)
  "Find a matching template from TEMPLATE-ALIST that matches TARGET."
  (let ((common-words 0)
        (selected nil))

    (dolist (template template-alist)
            (let* ((id (cdr template))
                   (description (car template))
                   (current (imgflip--count-common-words (capitalize target) (capitalize description))))
              (when (> current common-words)
                (progn
                    (setq distance current)
                    (setq selected template)))))
    selected))

(defun imgflip--count-common-words (str1 str2)
  "Return the commons words the two string have."
  (let ((words1 (split-string str1 "[ ]+"))
        (words2 (split-string str2 "[ ]+")))
    (length (seq-intersection words1 words2))))


(provide 'imgflip.el)
;;; imgflip.el ends here
