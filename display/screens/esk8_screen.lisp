;TODO move def to a file
@const-symbol-strings
(def UNIT "mm")
(def firts_iteration_esk 0)
(def firts_iteration_motor_config 0)
(def esk8_screen_num 0)
(def poles_config 20)
(def wheel_diam_config 0.075)
(def pulley_config   3.0)
(def batt_type_config 3)

@const-start
(defun esk8_screen(){
    (if (= firts_iteration_esk 0){
        (disp-clear)
        (def text_box (img-buffer 'indexed2 128 14))
        (def text_box_2 (img-buffer 'indexed2 40 14))
        (txt-block-l text_box 1 0 0  font_9x14 "EXIT      NEXT")
        (disp-render text_box (+ x_offset 0) (+ y_offset 53) '(0 0xFFFFFF))
        (img-clear text_box)

        (def numb_box (img-buffer 'indexed2 120 30))
        (setq firts_iteration_esk 1)

        (setq poles_config         (to-i (eeprom-read-i poles_add)))
        (setq wheel_diam_config    (to-float (eeprom-read-f wheel_diam_add)))
        (setq pulley_config        (to-float (eeprom-read-f pulley_add)))
        (setq batt_type_config     (to-i (eeprom-read-i batt_type_add)))

    })

    (cond
        ((eq esk8_screen_num 0) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Motor poles")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-c numb_box 1 60 0  font_20x30 (str-from-n poles_config "%d"))
            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

            (img-clear text_box_2)
            (disp-render text_box_2 (+ x_offset 37) (+ y_offset 44) '(0 0xFFFFFF))

            (if (> (get-adc-raw) 3000){
                (sleep 0.1)
                (setq poles_config (+ poles_config 1))
                (img-clear numb_box)
             })
            (if (< (get-adc-raw) 1000){
                (sleep 0.1)
                (setq poles_config (- poles_config 1))
                (img-clear numb_box)
            })
        ))
        ((eq esk8_screen_num 1) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Wheel diameter")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-c numb_box 1 60 0  font_20x30 (str-from-n (* wheel_diam_config 1000) "%.2f"));in mm
            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-c text_box_2 1 20 0  font_9x14 UNIT)
            (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF))
            (img-clear text_box_2)

             (if (> (get-adc-raw) 3000){
                (sleep 0.1)
                (setq wheel_diam_config (+ wheel_diam_config 0.001))
                (img-clear numb_box)
             })
            (if (< (get-adc-raw) 1000){
                (sleep 0.1)
                (setq wheel_diam_config (- wheel_diam_config 0.001))
                (img-clear numb_box)
            })
        ))
        ((eq esk8_screen_num 2) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Pulley reduction")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-c numb_box 1 60 0  font_20x30 (str-from-n pulley_config "%.1f:1"));
            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

            (img-clear text_box_2)
            (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF))

            (if (> (get-adc-raw) 3000){
                (sleep 0.1)
                (setq pulley_config (+ pulley_config 0.1))
                (img-clear numb_box)
             })
            (if (< (get-adc-raw) 1000){
                (sleep 0.1)
                (setq pulley_config (- pulley_config 0.1))
                (img-clear numb_box)
            })
        ))
        ((eq esk8_screen_num 3) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Battery")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-c numb_box 1 60 0  font_20x30 (str-from-n batt_type_config "%ds"));
            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

            (img-clear text_box_2)
            (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF))

            (if (> (get-adc-raw) 3000){
                (sleep 0.1)
                (setq batt_type_config (+ batt_type_config 1))
                (img-clear numb_box)
             })
            (if (< (get-adc-raw) 1000){
                (sleep 0.1)
                (setq batt_type_config (- batt_type_config 1))
                (img-clear numb_box)
            })
        ))
        ((eq esk8_screen_num 4) (progn
        (if (= firts_iteration_motor_config 0) {
            (disp-clear)
            (def text_box (img-buffer 'indexed2 127 14))
            (txt-block-l text_box 1 0 0  font_9x14 "Store config")
            (disp-render text_box (+ x_offset 1) (+ y_offset 0) '(0 0xFFFFFF))
            (img-clear text_box)
            (disp-render text_box (+ x_offset 1) (+ y_offset 35) '(0 0xFFFFFF))
            (img-clear text_box)
            (def text_box (img-buffer 'indexed2 36 14))
            (txt-block-l text_box 1 0 0  font_9x14 "EXIT")
            (disp-render text_box (+ x_offset 1) (+ y_offset 49) '(0 0xFFFFFF))
            (img-clear text_box)
            (txt-block-l text_box 1 0 0  font_9x14 "SAVE")
            (disp-render text_box (+ x_offset 90) (+ y_offset 49) '(0 0xFFFFFF))
            (img-clear text_box)
            })
            (txt-block-c numb_box 1 60 0  font_20x30 "SAVE?");
            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

            (img-clear text_box_2)
            (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF))
            (setq firts_iteration_motor_config 1)
        ))
    )
(if (= firts_iteration_motor_config 0)
    (if (= cfg_pressed_short 1){
        (setq cfg_pressed_short 0)
        (setq esk8_screen_num (+ esk8_screen_num 1))
        (if (> esk8_screen_num 4)
            (setq esk8_screen_num 0)
            (setq firts_iteration_motor_config 0)
        )
    })
    {
    (if (= cfg_pressed_long 1){
       (setq cfg_pressed_long 0)
       (setq cfg_pressed_short 0)
       (eeprom-store-i poles_add poles_config)
       (eeprom-store-f wheel_diam_add (to-float wheel_diam_config))
       (eeprom-store-f pulley_add (to-float pulley_config))
       (eeprom-store-i batt_type_add batt_type_config)
       (disp-clear)
       (setq firts_iteration 0)
       (setq menu_sub_index 0)
       (setq enter_menu 0)
       (setq firts_iteration_motor_config 0)
       (setq firts_iteration_esk 0)
       (setq esk8_screen_num 0)
    })
    })
    (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_esk 0)
        (setq firts_iteration_motor_config 0)
        (setq esk8_screen_num 0)
     })
})
@const-end