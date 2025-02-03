;Draw the battery Soc
;Parameters
; soc -> state of charge, float
; min -> min batt level for alarm, when it's below that value the drawing turns red
; porc_volt -> 1 porcentage 0 volts
; max -> how many volts are the max, if percentage is displayed this parameter is 100
; min -> px py position, pixel

(def vin_min 10.0)
(def vin_max 13.0)

@const-start
(defun m-trunc (v min max)
    (cond
        ((< v  min) min)
        ((> v max) max)
        (t v)
))

(defun utils_map(x in_min in_max out_min out_max)
(/ (* (- x in_min) (- out_max out_min)) (+ (- in_max in_min) out_min))
)
@const-end
(def bar_val 0)
(def bar_val_aux 0)
(def prescaler 0)
@const-start
(defun bat_soc (soc min max porc_volt rem_sk px py){
    (def soc_aux 0)
    (def display_soc soc)  ; Store original value for display
    (setq soc (m-trunc soc min max))  ; Truncate for bar filling animation
    (setq prescaler (+ prescaler 1))

    (if(= porc_volt 0)
        (setq bar_val (utils_map (* soc 10) (* min 10) (* max 10) 1 39))
        (setq bar_val (utils_map soc min max 1 39))
    )

    (def bar_col 0)
    (setq bar_col (utils_map soc min max 1 15))
    (setq bar_col (m-trunc bar_col 1 16)) 
    (def bat_box (img-buffer 'indexed16 40 16))
    (img-rectangle bat_box 0 0 39  14 15)

    (if(and (= rem_sk 1) (= (isCharging) 1)){; charging indicator
        (setq bar_val_aux (+ bar_val_aux 1))
        (if (> bar_val_aux 39)
            (setq bar_val_aux bar_val)  ; Reset to actual SOC level instead of 0
        )
        (if (> bar_val bar_val_aux)     ; Use the larger of the two values
            (setq bar_val bar_val)
            (setq bar_val bar_val_aux)
        )
        (setq bar_col 12)    ; put it full green
    })

    (img-rectangle bat_box 2 2 bar_val 11 bar_col '(filled))
    (img-line bat_box 39 14 39 14 15)     ; missing point in the rectangle
    (img-line bat_box 38 1 38 12 0)       ; prevent map error
    (if (= porc_volt 1)
         (txt-block-c bat_box 14 20 8 font_9x14 (str-from-n soc "%d%%"))
         (progn
             (if(and (= rem_sk 1)){
                (txt-block-c bat_box 14 20 8 font_9x14 (str-from-n (to-i (* display_soc 100)) "%03dV"))
                (txt-block-c bat_box 14 12 8 font_9x14  ".")
             }
             {
                (txt-block-c bat_box 14 20 8 font_9x14 (str-from-n (to-i (* display_soc 10)) "%03dV"))
                (txt-block-c bat_box 14 20 8 font_9x14  ".")
             })
         )
    )
    (disp-render bat_box px py '(0 0xFF0000 0xFF6000 0xFFFF00 0xE0FF00 0xD9FF00 0xBAFF00 0x9BFF00 0x70FF00 0x7CFF00 0x5DFF00 0x3EFF00  0x1FFF00 0x00FF00 0x0000FF 0xFFFFFF))

    
    (if (= rem_sk 1)
        (progn
            (def bat_box (img-buffer 'indexed2 2 7))
            (img-rectangle bat_box  0 0 2 7 1 '(filled))
            (disp-render bat_box (+ px 40) (+ py 4) '(0 0xFFFFFF))
        )
        (progn
            (def bat_box (img-buffer 'indexed4 8 15))
            (img-rectangle bat_box  0 4 8 7 1 '(filled))
            (img-rectangle bat_box  3 0 2 15 1 '(filled))
            (img-rectangle bat_box  1 0 6 2 2 '(filled))
            (img-rectangle bat_box  1 13 6 2 2 '(filled))
            (disp-render bat_box (+ px 40) (+ py 0) '(0 0xFFFFFF 0xFFB500))
            (disp-render bat_box (+ px -8) (+ py 0) '(0 0xFFFFFF 0xFFB500))
        )
    )
        
})
@const-end
