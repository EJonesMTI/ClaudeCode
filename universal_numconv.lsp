;;;============================================================
;;; NUMCONV.LSP - Conveyor Numbering Utility v10
;;; Fixed cross-machine coordinate translation and Z handling
;;;============================================================

(defun pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n))
)

(defun build-id (L Z S use-sub)
  (if use-sub
    (strcat (itoa L) "." (pad2 Z) "." (pad2 S))
    (strcat (itoa L) "." (pad2 Z))
  )
)

(defun write-att (ent tagname val / att attdata done)
  (setq att (entnext ent) done nil)
  (while (and att (not done))
    (setq attdata (entget att))
    (cond
      ((member (cdr (assoc 0 attdata)) '("INSERT" "SEQEND"))
        (setq done T)
      )
      ((and
         (= (cdr (assoc 0 attdata)) "ATTRIB")
         (= (cdr (assoc 2 attdata)) tagname)
       )
        (setq attdata (subst (cons 1 val) (assoc 1 attdata) attdata))
        (entmod attdata)
        (entupd att)
        (setq done T)
      )
    )
    (if (not done) (setq att (entnext att)))
  )
)

(defun read-att (ent tagname / att attdata)
  (setq att (entnext ent))
  (while (and att
              (not (and
                     (= (cdr (assoc 0 (entget att))) "ATTRIB")
                     (= (cdr (assoc 2 (entget att))) tagname)
                   ))
              (not (member (cdr (assoc 0 (entget att))) '("INSERT" "SEQEND")))
         )
    (setq att (entnext att))
  )
  (if (and att (= (cdr (assoc 0 (entget att))) "ATTRIB"))
    (cdr (assoc 1 (entget att)))
    ""
  )
)

(defun get-posint (prompt / val)
  (setq val 0)
  (while (< val 1)
    (setq val (getint prompt))
    (if (or (null val) (< val 1))
      (progn (princ "\n  Please enter a positive integer.") (setq val 0))
    )
  )
  val
)

;;; Robust DCS->WCS conversion with UCS fallback
;;; grread should return DCS (trans code 2) but some
;;; versions/configs return UCS (code 1) — try both and
;;; use whichever gives a point inside the current view
(defun grpt->wcs (pt / wcs-attempt view-min view-max)
  ;; Try DCS (2) first
  (setq wcs-attempt (trans pt 2 0))
  ;; Sanity check: is the result inside the current view extents?
  ;; If not, fall back to UCS (1) translation
  (setq view-min (getvar "VIEWCTR"))
  (setq view-max (getvar "VIEWSIZE"))
  ;; Simple check — if X or Y is wildly outside view centre by
  ;; more than 10x the view height, assume wrong coord system
  (if (> (abs (- (car wcs-attempt) (car view-min)))
         (* view-max 10.0))
    (setq wcs-attempt (trans pt 1 0))
  )
  wcs-attempt
)

;;; Aperture size in WCS units — minimum of 1.0 to handle
;;; colleagues with very small PICKBOX settings
(defun pick-aperture ()
  (max
    1.0
    (* (getvar "PICKBOX")
       (/ (getvar "VIEWSIZE")
          (cadr (getvar "SCREENSIZE"))
       )
    )
  )
)

;;; Find top-level INSERT near a WCS point
;;; Uses the full Z range of the WCS point +/- aperture
;;; to handle blocks not at Z=0
(defun find-insert-at (wcs-pt / ap z ss ent owner)
  (setq ap  (pick-aperture)
        z   (caddr wcs-pt)   ; use actual Z from translated point
        ss  (ssget "_C"
              (list (- (car wcs-pt) ap)
                    (- (cadr wcs-pt) ap)
                    (- z ap))          ; Z min — was hardcoded 0.0
              (list (+ (car wcs-pt) ap)
                    (+ (cadr wcs-pt) ap)
                    (+ z ap))          ; Z max — was hardcoded 0.0
              '((0 . "INSERT"))
            )
        ent nil
  )
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (while
        (and ent
             (setq owner (cdr (assoc 330 (entget ent))))
             (= (cdr (assoc 0 (entget owner))) "INSERT")
        )
        (setq ent owner)
      )
    )
  )
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    ent
    nil
  )
)

;;; Unhighlight an entity and its sub-entities
(defun unhighlight (ent / sub)
  (if ent
    (progn
      (redraw ent 4)
      (setq sub (entnext ent))
      (while (and sub
                  (not (member (cdr (assoc 0 (entget sub)))
                               '("SEQEND" "INSERT")))
             )
        (redraw sub 4)
        (setq sub (entnext sub))
      )
    )
  )
)

;;; Highlight an entity and its sub-entities
(defun highlight (ent / sub)
  (if ent
    (progn
      (redraw ent 3)
      (setq sub (entnext ent))
      (while (and sub
                  (not (member (cdr (assoc 0 (entget sub)))
                               '("SEQEND" "INSERT")))
             )
        (redraw sub 3)
        (setq sub (entnext sub))
      )
    )
  )
)

;;; Print status line
(defun print-status (L Z S use-sub)
  (princ
    (strcat
      "\n  ID:[" (build-id L Z S use-sub) "]"
      "  L=" (if use-sub
                (strcat (itoa (1+ L)) ".01.01")
                (strcat (itoa (1+ L)) ".01"))
      "  Z=" (if use-sub
                (strcat (itoa L) "." (pad2 (1+ Z)) ".01")
                (strcat (itoa L) "." (pad2 (1+ Z))))
      "  "  (if use-sub
               (strcat "S=drop-sub[" (itoa L) "." (pad2 Z) "]")
               (strcat "S=add-sub["  (itoa L) "." (pad2 Z) ".01]"))
      "  U=Undo  Esc=Done  | Pick block:"
    )
  )
)

;;;============================================================
;;; MAIN COMMAND
;;;============================================================
(defun c:NUMCONV (/ cur-L cur-Z cur-S use-sub
                    done gr gr-type gr-val
                    wcs-pt ent existing val
                    history snap hover-ent sub)

  (princ "\n")
  (princ "+================================================+\n")
  (princ "|  Conveyor Numbering Utility  v10               |\n")
  (princ "|  Click = assign current ID to block            |\n")
  (princ "|  L = Next Line   Z = Next Zone                 |\n")
  (princ "|  S = Toggle Sub  U = Undo   Esc = Finish       |\n")
  (princ "+================================================+\n")

  (setq cur-L     (get-posint "\n  Starting Line : ")
        cur-Z     (get-posint "  Starting Zone : ")
        cur-S     1
        use-sub   nil
        done      nil
        history   '()
        hover-ent nil
  )

  (princ "\n  Ready. S to enable subsystem at any time.")
  (print-status cur-L cur-Z cur-S use-sub)

  (while (not done)

    (setq gr      (grread T 15 2)
          gr-type (car gr)
          gr-val  (cadr gr)
    )

    (cond

      ;;--- Mouse move: update hover highlight ---
      ((= gr-type 11)
        (setq wcs-pt (grpt->wcs gr-val))
        (setq ent (find-insert-at wcs-pt))
        (cond
          ((and ent (not (equal ent hover-ent)))
            (unhighlight hover-ent)
            (highlight ent)
            (setq hover-ent ent)
          )
          ((and (null ent) hover-ent)
            (unhighlight hover-ent)
            (setq hover-ent nil)
          )
        )
      )

      ;;--- Left click ---
      ((= gr-type 3)
        (setq wcs-pt (grpt->wcs gr-val))
        (setq ent (find-insert-at wcs-pt))
        (if ent
          (progn
            (setq val      (build-id cur-L cur-Z cur-S use-sub)
                  existing (read-att ent "EQ_NUM")
            )
            (if (and existing (/= existing "") (/= existing val))
              (princ (strcat "\n  ! Overwriting [" existing "]"))
            )
            (unhighlight hover-ent)
            (setq hover-ent nil)
            (write-att ent "EQ_NUM" val)
            (setq history
              (cons (list ent existing cur-L cur-Z cur-S use-sub) history)
            )
            (princ (strcat "\n  + [" val "] assigned"))
            (if use-sub
              (setq cur-S (1+ cur-S))
              (setq cur-Z (1+ cur-Z))
            )
            (print-status cur-L cur-Z cur-S use-sub)
          )
          (princ "\n  No block found — try clicking closer to the block.")
        )
      )

      ;;--- Keystroke ---
      ((= gr-type 2)
        (cond

          ((or (= gr-val 76) (= gr-val 108))  ; L / l
            (setq cur-L (1+ cur-L) cur-Z 1 cur-S 1)
            (princ (strcat "\n  >> Line → " (build-id cur-L cur-Z cur-S use-sub)))
            (print-status cur-L cur-Z cur-S use-sub)
          )

          ((or (= gr-val 90) (= gr-val 122))  ; Z / z
            (setq cur-Z (1+ cur-Z) cur-S 1)
            (princ (strcat "\n  >> Zone → " (build-id cur-L cur-Z cur-S use-sub)))
            (print-status cur-L cur-Z cur-S use-sub)
          )

          ((or (= gr-val 83) (= gr-val 115))  ; S / s
            (if use-sub
              (progn
                (setq use-sub nil)
                (princ (strcat "\n  >> Sub OFF — ["
                               (build-id cur-L cur-Z cur-S nil) "]"))
              )
              (progn
                (unhighlight hover-ent)
                (setq hover-ent nil)
                (setq cur-S   (get-posint "\n  Starting subsystem number <1>: ")
                      use-sub T
                )
                (princ (strcat "\n  >> Sub ON  — ["
                               (build-id cur-L cur-Z cur-S T) "]"))
              )
            )
            (print-status cur-L cur-Z cur-S use-sub)
          )

          ((or (= gr-val 85) (= gr-val 117))  ; U / u
            (if history
              (progn
                (setq snap    (car history)
                      history (cdr history)
                )
                (write-att (nth 0 snap) "EQ_NUM" (nth 1 snap))
                (setq cur-L   (nth 2 snap)
                      cur-Z   (nth 3 snap)
                      cur-S   (nth 4 snap)
                      use-sub (nth 5 snap)
                )
                (princ (strcat "\n  Undone — restored [" (nth 1 snap) "]"))
                (print-status cur-L cur-Z cur-S use-sub)
              )
              (princ "\n  Nothing to undo.")
            )
          )

          ((= gr-val 27)  ; Esc
            (unhighlight hover-ent)
            (setq hover-ent nil done T)
          )

        )
      )

    )
  )

  (princ (strcat "\n\n  Done. " (itoa (length history)) " block(s) numbered.\n"))
  (princ "+================================================+\n")
  (princ)
)
