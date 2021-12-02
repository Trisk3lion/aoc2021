(defun dive ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "c:/WS/src/adventofcode/dag2/input.txt")
    (goto-char (point-min))
    (let ((fram 0)
          (djup 0))
      (while (re-search-forward "^\\(\\w+\\) \\([0-9]\\)" nil t)
        (let ((riktning (match-string 1))
              (siffra (string-to-number (match-string 2))))
          (cond
           ((equal riktning "down")
            (setq djup (+ siffra djup)))
           ((equal riktning "up")
            (setq djup (- djup siffra)))
           ((equal riktning "forward")
            (setq fram (+ siffra fram))))))
      (message "%s" fram)
      (message "%s" djup)
      (message "%d" (* fram djup)))))

(dive)



(defun dive2 ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "c:/WS/src/adventofcode/dag2/input.txt")
    (goto-char (point-min))
    (let ((fram 0)
          (djup 0)
          (aim 0))
      (while (re-search-forward "^\\(\\w+\\) \\([0-9]\\)" nil t)
        (let ((riktning (match-string 1))
              (siffra (string-to-number (match-string 2))))
          (cond
           ((equal riktning "down")
            (setq aim (+ siffra aim)))
           ((equal riktning "up")
            (setq aim (- aim siffra)))
           ((equal riktning "forward")
            (setq djup (+ (* aim siffra) djup))
            (setq fram (+ siffra fram))))))
      (message "%s" fram)
      (message "%s" djup)
      (message "%d" (* fram djup)))))

(dive2)
