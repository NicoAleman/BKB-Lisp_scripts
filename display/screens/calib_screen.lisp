(defun utils_map(x in_min in_max out_min out_max)
    (/ (* (- x in_min) (- out_max out_min)) (+ (- in_max in_min) out_min))
)

(defun draw_calib (bar_val mid_val px py){

    (def cal_box (img-buffer 'indexed4 104 16))

    (if (> bar_val mid_val)
        (img-rectangle cal_box 51 2 (utils_map bar_val mid_val 4095 0 51) 12 2 '(filled))
        (img-rectangle cal_box (utils_map bar_val 0 mid_val 0 51) 2 (- 51 (utils_map bar_val 0 mid_val 0 51)) 12 2 '(filled))
    )

    (img-rectangle cal_box 0 0 102 15 3)
    (img-line cal_box 102 15 102 15 3)
    (img-line cal_box 51 1 51 14 3)
    (disp-render cal_box px py '(0 0xFF0000 0xFF6000 0xFFFFFF))

})

(defun is_valid_calibration(min mid max)
    (and (> (- max mid) 500) (> (- mid min) 500))
)

(def thum_stick_adc 2048)
(def thum_min 4096)
(def thum_mid 0)
(def thum_max 0)
(def exit 1)
(def aux_adc 0)
(def firts_iteration_cal 0)
(def save_text_visible nil)

(defun calib_screen (){
    (if (= firts_iteration_cal 0){
        (def text_box (img-buffer 'indexed2 127 14))
        
        (def text_box (img-buffer 'indexed2 36 14))
        (txt-block-l text_box 1 0 0  font_9x14 "EXIT")
        (disp-render text_box (+ x_offset 1) (+ y_offset 49) '(0 0xFFFFFF))
        (img-clear text_box)

        (setq thum_mid (to-i (get-adc-raw)))
        (txt-block-c text_box 1 18 0 font_9x14 (str-from-n thum_mid "%04d"))
        (disp-render text_box (+ x_offset 46) (+ y_offset 1) '(0 0xFFFFFF))
        (img-clear text_box)

        (setq firts_iteration_cal 1)
        (setq save_text_visible nil)
    })

    ; Check if visibility state should change
    (if (and (not save_text_visible) (is_valid_calibration thum_min thum_mid thum_max))
        (progn
            (setq save_text_visible t)
            (def text_box (img-buffer 'indexed2 127 14))
            
            ; Render "Hold SAVE 2sec" if now valid
            (if save_text_visible
                (progn
                    (txt-block-l text_box 1 0 0  font_9x14 "Hold SAVE 2sec")
                    (disp-render text_box (+ x_offset 1) (+ y_offset 35) '(0 0xFFFFFF))
                )
            )

            (def text_box (img-buffer 'indexed2 36 14))
            
            ; Render "SAVE" if now valid
            (if save_text_visible
                (progn
                    (txt-block-l text_box 1 0 0  font_9x14 "SAVE")
                    (disp-render text_box (+ x_offset 90) (+ y_offset 49) '(0 0xFFFFFF))
                )
            )
            (img-clear text_box)
        )
    )

     (setq aux_adc (get-adc-raw))

     (if (< aux_adc thum_min)
         (setq thum_min (to-i aux_adc))
     )

     (if (> aux_adc thum_max)
          (setq thum_max (to-i aux_adc))
     )

     (txt-block-c text_box 1 18 0 font_9x14 (str-from-n thum_min "%04d"))
     (disp-render text_box (+ x_offset 2) (+ y_offset 1) '(0 0xFFFFFF))
     (img-clear text_box)

     (txt-block-c text_box 1 18 0 font_9x14 (str-from-n thum_max "%04d"))
     (disp-render text_box (+ x_offset 90) (+ y_offset 1) '(0 0xFFFFFF))
     (img-clear text_box)

     (draw_calib (get-adc-raw) thum_mid (+ x_offset 13) (+ y_offset 16))

     (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_cal 0)
        (setq thum_min 4096)
        (setq thum_mid 0)
        (setq thum_max 0)
     })

     (if (= cfg_pressed_long 1){
        (setq cfg_pressed_long 0)
        (setq cfg_pressed_short 0)
        (if (is_valid_calibration thum_min thum_mid thum_max)
            (progn
                (eeprom-store-i min_cal_add thum_min)
                (eeprom-store-i mid_cal_add thum_mid)
                (eeprom-store-i max_cal_add thum_max)
            )
        )
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_cal 0)
        (setq thum_min 4096)
        (setq thum_mid 0)
        (setq thum_max 0)

     })

})


