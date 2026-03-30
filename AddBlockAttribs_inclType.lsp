;; ============================================================
;; ADD-BLOCK-ATTRIBUTES.lsp
;; Adds EQ_NUM, NAME, and TYPE attribute definitions to all
;; block definitions in the current drawing.
;; Safe to re-run — skips attributes that already exist.
;; ============================================================

(defun c:AddBlockAttribs ( / doc blkTable blkName blkDef added)

  (vl-load-com)

  ;; ---- Helper: determine block type from name ----
  (defun get-block-type (blkName / n)
    (setq n (strcase blkName))
    (cond
      ((or (wcmatch n "LAC-PO-DR-*")
           (wcmatch n "LAC-POB-DR-*"))        "Drive - Poly-O")
      ((wcmatch n "*PR-OS-DRIVE*")            "Drive - Pallet Roller")
      ((or (wcmatch n "LAC-PO-P*")
           (wcmatch n "LAC-POB-P*")
           (wcmatch n "LAC-POM-*"))           "Conveyor - Poly-O")
      ((wcmatch n "LAC-PR-P*")               "Conveyor - Pallet Roller")
      ((wcmatch n "LAC-PV*")                 "Conveyor - Poly-V")
      ((or (wcmatch n "LAC-24V-TRANSFER*")
           (wcmatch n "TONG CONVEYOR*"))      "Conveyor")
      ((or (wcmatch n "IRB*")
           (wcmatch n "KR_*"))               "Robot")
      ((or (wcmatch n "*GRIPPER*")
           (wcmatch n "*TOOL*"))             "End Effector")
      ((or (wcmatch n "*RETRO-REFLECTIVE*")
           (wcmatch n "*DIFFUSED PROXY*")
           (wcmatch n "*DIFFUSED*PROXY*")
           (wcmatch n "*LIGHT_CURTAIN*"))    "Sensor")
      ((or (wcmatch n "*PUSHER*")
           (wcmatch n "*BLADE STOP*")
           (wcmatch n "*DIVERT*")
           (wcmatch n "*SEPARATION STATION*")
           (wcmatch n "*SCISSOR LIFT*"))     "Actuator")
      ((or (wcmatch n "INFILLPLATE*")
           (wcmatch n "PALLET*")
           (wcmatch n "COMPACTOR*"))         "Ancillary")
      (T                                     "Other")
    )
  )

  ;; ---- Helper: check if block definition has a given attribute tag ----
  (defun has-attrib (blkDef tagName / found)
    (setq found nil)
    (vlax-for obj blkDef
      (if (and
            (= (vla-get-ObjectName obj) "AcDbAttributeDefinition")
            (= (strcase (vla-get-TagString obj)) (strcase tagName))
          )
        (setq found T)
      )
    )
    found
  )

  ;; ---- Helper: add an attribute definition to a block ----
  (defun add-attdef (blkDef tag default prompt height / att ins)
    (setq ins (vlax-3d-point 0.0 0.0 0.0))
    (setq att
      (vla-addAttribute
        blkDef
        height  ; Text height
        0       ; Mode: visible
        prompt  ; Prompt string
        ins     ; Insertion point
        tag     ; Tag
        default ; Default value
      )
    )
    (vla-put-Layer att "MT_EQList")
    (vlax-release-object ins)
    att
  )

  ;; ---- Helper: ensure layer exists and is green ----
  (defun ensure-layer (layerName / layers layer trueColor)
    (setq layers (vla-get-Layers doc))
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers layerName)))
      (setq layer (vla-add layers layerName))
      (setq layer (vla-item layers layerName))
    )
    (setq trueColor (vla-get-TrueColor layer))
    (vla-SetRGB trueColor 0 255 0)
    (vla-put-TrueColor layer trueColor)
  )

  ;; ---- Main logic ----
  (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blkTable (vla-get-Blocks doc))
  (setq added    0)

  (ensure-layer "MT_EQList")

  (vlax-for blkDef blkTable
    (setq blkName (vla-get-Name blkDef))

    (if (not (or
               (= (substr blkName 1 1) "*")
               (= (substr blkName 1 3) "A$C")
               (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-get-IsXRef (list blkDef)))
               (equal (vl-catch-all-apply 'vla-get-IsXRef (list blkDef)) :vlax-true)
             ))
      (progn
        (princ (strcat "\nProcessing: " blkName))

        ;; EQ_NUM first (appears at top of attribute editor)
        (vl-catch-all-apply
          '(lambda ()
             (if (not (has-attrib blkDef "EQ_NUM"))
               (progn
                 (add-attdef blkDef "EQ_NUM" (strcat blkName "-001") (strcat "EQ number for " blkName) 50)
                 (princ " -> Added EQ_NUM")
                 (setq added (1+ added))
               )
               (princ " -> EQ_NUM exists, skipped")
             )
           )
        )

        ;; NAME second
        (vl-catch-all-apply
          '(lambda ()
             (if (not (has-attrib blkDef "NAME"))
               (progn
                 (add-attdef blkDef "NAME" blkName (strcat "Name for " blkName) 2.5)
                 (princ " -> Added NAME")
                 (setq added (1+ added))
               )
               (princ " -> NAME exists, skipped")
             )
           )
        )

        ;; TYPE third
        (vl-catch-all-apply
          '(lambda ()
             (if (not (has-attrib blkDef "TYPE"))
               (progn
                 (add-attdef blkDef "TYPE" (get-block-type blkName) (strcat "Type for " blkName) 2.5)
                 (princ " -> Added TYPE")
                 (setq added (1+ added))
               )
               (princ " -> TYPE exists, skipped")
             )
           )
        )
      )
    )
  )

  ;; ---- Pass 1: sync all instances to pick up any new definitions ----
  (princ "\nSyncing block instances...")
  (vlax-for ref (vla-get-ModelSpace doc)
    (vl-catch-all-apply
      '(lambda ()
         (if (= (vla-get-ObjectName ref) "AcDbBlockReference")
           (vla-synchronizeAttributes ref)
         )
       )
    )
  )

  ;; ---- Pass 2: populate TYPE on ALL instances, whether new or existing ----
  (princ "\nPopulating TYPE values...")
  (vlax-for ref (vla-get-ModelSpace doc)
    (vl-catch-all-apply
      '(lambda ()
         (if (= (vla-get-ObjectName ref) "AcDbBlockReference")
           (vlax-for att (vlax-invoke ref 'GetAttributes)
             (if (= (strcase (vla-get-TagString att)) "TYPE")
               (vla-put-TextString att (get-block-type (vla-get-Name ref)))
             )
           )
         )
       )
    )
  )

  ;; ---- Pass 3: ATTSYNC each block by name to force instance updates ----
  (princ "\nRunning ATTSYNC per block...")
  (vlax-for blkDef blkTable
    (vl-catch-all-apply
      '(lambda ()
         (setq blkName (vla-get-Name blkDef))
         (if (not (or
                    (= (substr blkName 1 1) "*")
                    (= (substr blkName 1 3) "A$C")
                    (vl-catch-all-error-p
                      (vl-catch-all-apply 'vla-get-IsXRef (list blkDef)))
                    (equal (vl-catch-all-apply 'vla-get-IsXRef (list blkDef)) :vlax-true)
                  ))
           (progn
             (princ (strcat "\n  ATTSYNC: " blkName))
             (command "_.ATTSYNC" "_Name" blkName "")
           )
         )
       )
    )
  )
  (princ)
)
