;;; image-line-spike.el --- T13 spike: external-reservation placement -*- lexical-binding: t; -*-

;; Question (image-line Phase 2, spec §9 risk 1): can `kitty-gfx--refresh'
;; paint an overlay whose DISPLAY is reserved by an external package
;; (image-line lays its own blank slice rows), rather than one kitty owns?
;;
;; Code answer (GO): `kitty-gfx--refresh-overlay' (kitty-graphics.el)
;; computes the paint position from `(overlay-start ov)' via
;; `kitty-gfx--overlay-screen-pos' (posn-at-point) and reads ONLY the props
;; `kitty-gfx-{id,pid,cols,rows,file}'.  It never reads or writes the
;; overlay's `display'.  So the minimal registration record is:
;;   - the overlay pushed onto buffer-local `kitty-gfx--overlays'
;;   - props kitty-gfx-id / kitty-gfx-cols / kitty-gfx-rows / kitty-gfx-file
;;     (pid is allocated per-window by `kitty-gfx--record-image-placement')
;;   - the overlay START at the image's top-left cell
;;   - the space (rows tall) reserved by the CALLER (image-line's rows).
;;
;; This selftest reserves rows with an external display and registers such
;; an overlay, then forces one refresh and reports whether kitty emitted a
;; graphics placement (APC "\e_G…") for it.  Run inside a REAL kitty/ghostty
;; terminal:
;;   emacs -nw -Q -L . -l kitty-graphics.el -l image-line-spike.el \
;;     --eval '(kitty-gfx-selftest-external-placement "/path/to/img.png")'
;; Expect: "PLACEMENT EMITTED" and the image painted over the reserved rows.

(require 'kitty-graphics)

(defun kitty-gfx-selftest-external-placement (file)
  "Reserve rows with an EXTERNAL overlay display, register it, refresh."
  (interactive "fImage file: ")
  (switch-to-buffer (get-buffer-create "*kitty-external-selftest*"))
  (erase-buffer)
  (unless (bound-and-true-p kitty-graphics-mode) (kitty-graphics-mode 1))
  (redisplay t)
  (unless kitty-gfx--active-backend
    (user-error "No graphics backend — run inside kitty/ghostty, -nw"))
  (let* ((abs (expand-file-name file))
         (px (kitty-gfx--image-pixel-size abs))
         (dims (if px (kitty-gfx--compute-cell-dims (car px) (cdr px)
                                                    kitty-gfx-max-width 40)
                 (cons 40 15)))
         (cols (car dims)) (rows (cdr dims))
         (id (kitty-gfx--alloc-id))
         (sent 0))
    ;; layout: heading, then an anchor char, then trailing text.  The anchor
    ;; carries BOTH the external reservation (display = ROWS blank lines,
    ;; image-line style) and the kitty registration props.
    (insert "External-placement selftest — image should paint over the blank rows below:\n")
    (let ((anchor (point)))
      (insert "X\n")
      (insert "----- text AFTER the reserved region (must not be overwritten) -----\n")
      (kitty-gfx--kitty-prepare abs id)          ; transmit bytes + mark
      (let ((ov (make-overlay anchor (1+ anchor))))
        ;; external reservation: ROWS blank screen lines, COLS wide
        (overlay-put ov 'display
                     (concat (mapconcat (lambda (_) (make-string cols ?\s))
                                        (number-sequence 1 rows) "\n")
                             "\n"))
        (overlay-put ov 'face 'default)
        ;; registration record (no display ownership by kitty)
        (overlay-put ov 'kitty-gfx t)
        (overlay-put ov 'kitty-gfx-external t)
        (overlay-put ov 'kitty-gfx-id id)
        (overlay-put ov 'kitty-gfx-cols cols)
        (overlay-put ov 'kitty-gfx-rows rows)
        (overlay-put ov 'kitty-gfx-file abs)
        (push ov kitty-gfx--overlays))
      ;; count graphics escapes emitted during the forced refresh
      (advice-add 'kitty-gfx--terminal-send :before
                  (lambda (s &rest _)
                    (when (and (stringp s) (string-match-p "\e_G" s))
                      (setq sent (1+ sent))))
                  '((name . il-spike-count)))
      (unwind-protect
          (progn (setq kitty-gfx--force-redisplay t) (kitty-gfx--refresh))
        (advice-remove 'kitty-gfx--terminal-send 'il-spike-count))
      (let* ((ov (car kitty-gfx--overlays))
             (placed (overlay-get ov 'kitty-gfx-last-row)))
        (goto-char (point-max))
        (insert (format "\n\n=== SELFTEST RESULT ===\nimage cols=%d rows=%d id=%d\n"
                        cols rows id))
        (insert (format "graphics escapes emitted this refresh: %d\n" sent))
        (insert (format "placement recorded (kitty-gfx-last-row): %S\n" placed))
        (insert (if (and placed (> sent 0))
                    "GO: kitty placed an EXTERNALLY-RESERVED overlay ✓\n"
                  "NO-GO: no placement emitted — inspect *Messages* (kitty-gfx--log)\n"))
        (message "selftest: escapes=%d placed=%S" sent placed)))))

(provide 'image-line-spike)
;;; image-line-spike.el ends here
