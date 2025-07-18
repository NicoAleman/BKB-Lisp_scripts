(def firts_iteration_remote 0)
(def remote_screen_num 0)
(def thum_stick_prescaler 0)
(def distance_total 0.0)

@const-start
(defun remote_screen(){
    (if (= firts_iteration_remote 0){
        (disp-clear)
        (def text_box (img-buffer 'indexed2 128 14))
        (txt-block-l text_box 1 0 0  font_9x14 "EXIT      NEXT")
        (disp-render text_box (+ x_offset 0) (+ y_offset 53) '(0 0xFFFFFF))
        (img-clear text_box)
        (def text_box_2 (img-buffer 'indexed2 40 14))
        (def numb_box (img-buffer 'indexed2 120 30))
        (setq firts_iteration_remote 1)
    })

    (cond
        ((eq remote_screen_num 0) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "MAC address")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)
            (def aux_mac 0)

            (setq aux_mac(ix (get-mac-addr) 0))
            (txt-block-l text_box 1 10 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
            (setq aux_mac(ix (get-mac-addr) 1))
            (txt-block-c text_box 1 64 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
            (setq aux_mac(ix (get-mac-addr) 2))
            (txt-block-l text_box 1 93 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
            (disp-render text_box (+ x_offset 0) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear text_box)

            (setq aux_mac(ix (get-mac-addr) 3))
            (txt-block-l text_box 1 10 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
            (setq aux_mac(ix (get-mac-addr) 4))
            (txt-block-c text_box 1 64 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
            (setq aux_mac(ix (get-mac-addr) 5))
            (txt-block-l text_box 1 93 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
            (disp-render text_box (+ x_offset 0) (+ y_offset 32) '(0 0xFFFFFF))
            (img-clear text_box)

        ))
        ((eq remote_screen_num 1) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Pair MAC")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-l text_box 1 10 0  font_9x14  (str-from-n (eeprom-read-i pair0_add) "%03d"))
            (txt-block-c text_box 1 64 0  font_9x14  (str-from-n (eeprom-read-i pair1_add) "%03d"))
            (txt-block-l text_box 1 93 0  font_9x14  (str-from-n (eeprom-read-i pair2_add) "%03d"))
            (disp-render text_box (+ x_offset 0) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-l text_box 1 10 0  font_9x14  (str-from-n (eeprom-read-i pair3_add) "%03d"))
            (txt-block-c text_box 1 64 0  font_9x14  (str-from-n (eeprom-read-i pair4_add) "%03d"))
            (txt-block-l text_box 1 93 0  font_9x14  (str-from-n (eeprom-read-i pair5_add) "%03d"))
            (disp-render text_box (+ x_offset 0) (+ y_offset 32) '(0 0xFFFFFF))
            (img-clear text_box)

        ))
        ((eq remote_screen_num 2) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Units")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (if (= units 1)
                (txt-block-c numb_box 1 60 0  font_20x30 "METRIC");
                (txt-block-c numb_box 1 60 0  font_20x30 "IMPERIAL");
            )

            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

            (img-clear text_box_2)
            (disp-render text_box_2 (+ x_offset 37) (+ y_offset 44) '(0 0xFFFFFF))

        ))
        ((eq remote_screen_num 3) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "total distance")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)
            (setq distance_total (to-float (eeprom-read-f total_trip_add)))
            (setq distance_total (+ distance_total (if (= distance 0) 0 (/ distance 1000))))
            (if (= units 1)
            {
                (txt-block-c numb_box 1 60 0  font_20x30 (str-from-n distance_total "%0.1f"));
                (txt-block-c text_box_2 1 20 0  font_9x14 "Km")
                (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF))
                (img-clear text_box_2)
            }
            {
                (setq distance_total (* distance_total 0.6213))
                (txt-block-c numb_box 1 60 0  font_20x30 (str-from-n distance_total "%0.1f"));
                (txt-block-c text_box_2 1 20 0  font_9x14 "Mil")
                (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF))
                (img-clear text_box_2)
            })

            (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF))
            (img-clear numb_box)

        ))
    )

    ; adjust values with thumb stick
    (setq thum_stick_prescaler (+ thum_stick_prescaler 1))
    (if (and (> thum_stick_prescaler 10)){
        ; thumb stick up
        (if (> (get-adc 0) 2){
            ; if on units screen, change units
            (if (eq remote_screen_num 2){
                (if (= units 0) (setq units 1) (setq units 0))
                (eeprom-store-i units_add units)
            })
        })

        ; thumb stick down
        (if (< (get-adc 0) 0.8){
            ; if on units screen, change units
            (if (eq remote_screen_num 2){
                (if (= units 0) (setq units 1) (setq units 0))
                (eeprom-store-i units_add units)
            })
        })

        (setq thum_stick_prescaler 0)
    })

    ; cycle through screens
    (if (= cfg_pressed_short 1){
        (setq cfg_pressed_short 0)
        (setq remote_screen_num (+ remote_screen_num 1))
        (disp-render numb_box (+ x_offset 4) (+ y_offset 17) '(0 0xFFFFFF)); needed to clear the pixels
        (disp-render text_box_2 (+ x_offset 47) (+ y_offset 44) '(0 0xFFFFFF)); needed to clear the pixels
        (if (> remote_screen_num 3)
            (setq remote_screen_num 0)
        )
    })

    ; exit
    (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_remote 0)
        (setq remote_screen_num 0)
     })
})
@const-start