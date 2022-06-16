;;; bits-widget.el --- Widget for setting bits -*- lexical-binding: t -*-

;; Author: Jesse Millwood
;; Maintainer: Jesse Millwood
;; Version: 0.1
;; Package-Requires: (widget)
;; Homepage: NA
;; Keywords: widget, bit manipulation


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; TODO:
;;  - [x] Allow Choosing bits to set and update hex field
;;  - [x] Allow Inputing Hex Number and update bits
;;    - [x] This might require changing how the bit widgets are generated
;;      - Need to think about that more
;;      - Maybe require "word size" parameter (8, 16, 32, 64)
;;        - Then iterate through and create/ update the bit widgets
;;        - This way, your hex value should fit in the word size you chose
;;          - If not, truncate and issue a warning
;;          - And the bit fields should be available to represent back and forth
;;  - [x] Export to Calc
;;    - [x] Optionally append unit
;;  - [ ] Import from Calc (not sure if can do this
;;  - [ ] Change initial position of point
;;  - [ ] Extend Keymap
;;   - [ ] "q" Burry buffer
;;   - [ ] "h" Jump to Hex input
;;   - [ ] "n" next field/bit
;;   - [ ] "p" previous field/bit
;;   - [ ] "b" Jump to Binary input
;;     - [ ] Handle predicate args to jump to bit
;;  - [ ] Change faces of headings
;;  - [x] Allow input of word size
;;    - [x] Update hex field ":size" accordingly
;;  - [x] Operate on byte sizes only

;;; Code:

(require 'widget)

(defvar bw/hex-value)
(defvar bw/endianness 'little)
(defvar bw/size 1)
(defvar bw/calc-kill-ring-ending " Byte")
(defvar bw/kill-ring-format "calc")

(defun bw/gen-bit-toggle-f (index)
  (widget-create 'toggle
                 :on "1"
                 :off "0"
                 :format "%[%v%]"
                 :notify #'(lambda (this-widget changed-widget &optional change-event)
                             (let* ((current-bits (string-to-number
                                                   (widget-value bw/hex-value) 16))
                                    (updated-bits (if (equal (widget-value this-widget) t)
                                                      (logior current-bits (lsh 1 index))
                                                    (logand current-bits (lognot (lsh 1 index))))))
                               (widget-value-set bw/hex-value (format "%x" updated-bits)))
                             (widget-setup))))

(defun bits-widget ()
  "Manipulate a bit field"
  (interactive)
  (switch-to-buffer "*Bit Widget*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (make-local-variable 'bw/hex-value)
  (make-local-variable 'bw/calc-kill-ring-ending)
  (message "Starging Bit Manipulation Widget")
  (message "  Size: %d" bw/size)
  (widget-insert "Bit Manipulation Widget\n\n")

;;  (widget-create 'menu-choice
;;                 :tag "Endianness"
;;                 :value "Little"
;;                 :notify (lambda (this-widget &rest ignore)
;;                           (message "Endianness Chosen: %s" (widget-value this-widget))
;;                           (if (string-equal (widget-value this-widget ) "Little")
;;                               (setq bw/endianness 'little)
;;                             (setq bw/endianness 'big))
;;                           )
;;                 '(item :tag "Little Endian" :value "Little")
;; TODO: Support big endian
;;                 '(item :tag "Big Endian" :value "Big")
;;                 )
  (widget-create 'menu-choice
                 :tag "Size (Bytes)"
                 :value (number-to-string bw/size)
                 :notify (lambda (this-widget &rest ignore)
                           (setq bw/size (string-to-number (widget-value this-widget)))
                           ;; TODO: maybe call bits-widget here to reset the widget?
                           (bits-widget)
;;                           (widget-setup)
                           )

                 '(item :tag "1 Byte (8 bits)" :value "1")
                 '(item :tag "2 Byte (16 bits)" :value "2")
                 '(item :tag "4 Byte (32 bits)" :value "4")
                 '(item :tag "8 Byte (64 bits)" :value "8")
                 )

  (setq bw/hex-value
        (widget-create 'editable-field
                       :format "Hex: 0x%v"
                       :value "0"
                       :size 16
                       ))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let ((num-bits (* bw/size 8))
                                 (cur-bit 0)
                                 (cur-hex-value (string-to-number (widget-value bw/hex-value) 16)))
                             (while (< cur-bit num-bits)
                               (widget-value-set (nth cur-bit bw/bits-widget)
                                                 (if ( <= 1 (logand (lsh 1 cur-bit) cur-hex-value))
                                                     t
                                                   nil))
                               (setq cur-bit (1+ cur-bit))
                               )
                             (widget-setup)
                             ))
                 "Calculate Bits")
  (widget-insert "\n")

  (widget-insert "Binary: \n")

  (widget-insert "|")
  (setq bw/bits-widget '())
  (let ((cur-bit (- (* bw/size 8) 1)))
    (while (>= cur-bit 0)
      (setq bw/bits-widget (cons (bw/gen-bit-toggle-f cur-bit) bw/bits-widget))
      (setq cur-bit (1- cur-bit))
      )
    )
  (widget-insert "|\n")
  (widget-insert "|")
  (let ((cur-bit (- (* bw/size 8) 1)))
    (while (>= cur-bit 0)
      (if (equal 0 (mod (+ 1 cur-bit) 4))
          (let ((bit-indexes (format "↑%-3d" cur-bit)))
            (widget-insert bit-indexes))
        )
      (setq cur-bit (1- cur-bit))
      )
    )
  (widget-insert "|")
  (widget-insert"\n\n")
  (widget-insert "Push to Kill Ring :\n")

  (setq format-widget
        (widget-create 'radio-button-choice
                       :value bw/kill-ring-format
                       :notify (lambda (this-widget &rest ignore)
                                 (setq bw/kill-ring-format (widget-value this-widget))
                                 )
                       '(item :tag "Calc format (16#)" :value "calc")
                       '(item :tag "Elisp format (#x)" :value "elisp")
                       '(item :tag "Regular format (0x)" :value "regular")))
  (widget-insert "Append unit:\n")
  (setq add-byte-exportp
        (widget-create 'radio-button-choice
                       :value bw/calc-kill-ring-ending
                       :notify (lambda (this-widget &rest ignore)
                                 (setq bw/calc-kill-ring-ending (widget-value this-widget))
                                 )
                       '(item :tag "Byte" :value " Byte")
                       '(item :tag "None" :value "")))

  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let* ((bw-value (string-to-number (widget-value bw/hex-value) 16))
                                 (prefix-string (cond ((string-equal bw/kill-ring-format "calc")
                                                       "16#")
                                                      ((string-equal bw/kill-ring-format "elisp")
                                                       "#x")
                                                      ((string-equal bw/kill-ring-format "regular")
                                                       "0x"))
                                                )
                                 (number-string (format "%s%x%s"
                                               prefix-string
                                               bw-value
                                               bw/calc-kill-ring-ending)))
                             (with-temp-buffer
                               (beginning-of-buffer)
                               (insert number-string)
                               (kill-region (point-max) (point-min)))
                             (message "Bit Widget pushing %s to kill-ring"
                                      number-string))
                           (widget-setup)
                           )
                 "Push Hex")

  (widget-insert "\n\n")
  (widget-insert "Form Control: \n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (bits-widget))
                 "Reset")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  )
(provide 'bits-widget)
;;; bits-widget.el ends here
