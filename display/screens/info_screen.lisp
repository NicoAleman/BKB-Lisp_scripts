;TODO read firmware and hardware version fron skate and remote
(def firts_iteration_info 0)
(def iteration_data 0)
(def info_screen_num 0)
(def rem_fw 6.05)
(def rem_hw 1.0)
(def rec_fw 6.05)
(def rec_hw 1.0)
(def rec_lisp 1.0)
(def sk_fw 6.05)
(def sk_hw 1.0)
(def sk_lisp 1.0)
(def date_aux 0.0)
(def ppm_menu_stat 0.0)
(def ppm_status 0.0)

@const-start
(defun info_screen(){
    (if (= firts_iteration_info 0){
        (disp-clear)
        (def text_box (img-buffer 'indexed2 60 14))

        (txt-block-l text_box 1 0 0  font_9x14 "EXIT")
        (disp-render text_box (+ x_offset 0) (+ y_offset 53) '(0 0xFFFFFF))
        (img-clear text_box)

        (txt-block-c text_box 1 40 0  font_9x14 "NEXT")
        (disp-render text_box (+ x_offset 70) (+ y_offset 53) '(0 0xFFFFFF))
        (img-clear text_box)

        (txt-block-l text_box 1 0 0  font_9x14 "Fw:")
        (disp-render text_box (+ x_offset 50) (+ y_offset 12) '(0 0xFFFFFF))
        (img-clear text_box)

        (txt-block-l text_box 1 0 0  font_9x14 "Hw:")
        (disp-render text_box (+ x_offset 50) (+ y_offset 25) '(0 0xFFFFFF))
        (img-clear text_box)

        (txt-block-l text_box 1 0 0  font_9x14 "Lisp:")
        (disp-render text_box (+ x_offset 32) (+ y_offset 38) '(0 0xFFFFFF))
        (img-clear text_box)

        (def numb_box (img-buffer 'indexed2 36 14))
        (setq firts_iteration_info 1)

    })

    (cond
        ((eq info_screen_num 0) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Remote")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n rem_fw "%.2f")) ; firmware version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 12) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n rem_hw "%.2f")) ; hardware version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 25) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n Lisp_V "%.2f")) ; lisp version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 38) '(0 0xFFFFFF))
            (img-clear numb_box)

        ))
        ((eq info_screen_num 1) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Receiver")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n rec_fw "%.2f")) ; firmware version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 12) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n rem_hw "%.2f")) ; hardware version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 25) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n rec_lisp "%.2f")) ; lisp version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 38) '(0 0xFFFFFF))
            (img-clear numb_box)

        ))
        ((eq info_screen_num 2) (progn
            (txt-block-l text_box 1 0 0  font_9x14 "Skate")
            (disp-render text_box (+ x_offset 1) (+ y_offset -1) '(0 0xFFFFFF))
            (img-clear text_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n sk_fw "%.2f")) ; hardware version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 12) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n sk_hw "%.2f")) ; firmware version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 25) '(0 0xFFFFFF))
            (img-clear numb_box)

            (txt-block-l numb_box 1 0 0  font_9x14 (str-from-n sk_lisp "%.2f")) ; lisp version
            (disp-render numb_box (+ x_offset 77) (+ y_offset 38) '(0 0xFFFFFF))
            (img-clear numb_box)

        ))
        ((eq info_screen_num 3) (progn
            (if (= iteration_data 0) {
                (disp-clear)
                (setq iteration_data 1)

                (def text_box (img-buffer 'indexed2 127 14))
                (txt-block-l text_box 1 0 0  font_9x14 "Hold SAVE 2sec")
                (disp-render text_box (+ x_offset 1) (+ y_offset 35) '(0 0xFFFFFF))

                (def text_box (img-buffer 'indexed2 36 14))

                (txt-block-l text_box 1 0 0  font_9x14 "EXIT")
                (disp-render text_box (+ x_offset 1) (+ y_offset 49) '(0 0xFFFFFF))
                (img-clear text_box)

                (txt-block-l text_box 1 0 0  font_9x14 "SAVE")
                (disp-render text_box (+ x_offset 90) (+ y_offset 49) '(0 0xFFFFFF))
                (img-clear text_box)

                (setq date_aux (eeprom-read-f data_index))
                (print date_aux)
            })
            (if (eq ppm_menu_stat 0.0) {
                (def text_box (img-buffer 'indexed2 115 14))
                (txt-block-l text_box 1 0 0  font_9x14 "Data-Rate[ms]")
                (disp-render text_box (+ x_offset 5) (+ y_offset 0) '(0 0xFFFFFF))
                (img-clear text_box)

                (if (and (> (get-adc-raw) 3000) (< date_aux 0.121)){
                    (sleep 0.1)
                    (setq date_aux (+ date_aux 0.001))
                })
                (if (and (< (get-adc-raw) 1000) (> date_aux 0.031)){
                    (sleep 0.1)
                    (setq date_aux (- date_aux 0.001))
                })

            (def text_box (img-buffer 'indexed2 36 14))
            (txt-block-l text_box 1 10 0  font_9x14  (str-from-n (to-i (* date_aux 1000)) "%02d"))
            (disp-render text_box (+ x_offset 40) (+ y_offset 18) '(0 0xFFFFFF))
            (img-clear text_box)
            })

            (if (eq ppm_menu_stat 1.0) {
                (setq ppm_menu_stat 1.0)

                (def text_box (img-buffer 'indexed2 115 14))
                (txt-block-l text_box 1 0 0  font_9x14 "PPM-MODE")
                (disp-render text_box (+ x_offset 5) (+ y_offset 0) '(0 0xFFFFFF))
                (img-clear text_box)

                (if (eq (to-i ppm_status) 1) {
                    (def text_box (img-buffer 'indexed2 115 14))
                    (txt-block-l text_box 1 10 0  font_9x14  "ENABLE")
                    (disp-render text_box (+ x_offset 10) (+ y_offset 18) '(0 0xFFFFFF))
                }
                {
                    (def text_box (img-buffer 'indexed2 115 14))
                    (txt-block-l text_box 1 10 0  font_9x14  "DISABLE")
                    (disp-render text_box (+ x_offset 10) (+ y_offset 18) '(0 0xFFFFFF))
                })

                (if  (> (get-adc-raw) 3000){ ;3000
                     (sleep 0.1)
                     (def text_box (img-buffer 'indexed2 115 14))
                     (txt-block-l text_box 1 10 0  font_9x14  "ENABLE")
                     (disp-render text_box (+ x_offset 10) (+ y_offset 18) '(0 0xFFFFFF))
                     (img-clear text_box)
                     (setq ppm_status 1.0)
                })
                (if (< (get-adc-raw) 2000){ ; 1000, 0.031
                    (sleep 0.1)
                    (def text_box (img-buffer 'indexed2 115 14))
                    (txt-block-l text_box 1 10 0  font_9x14  "DISABLE")
                    (disp-render text_box (+ x_offset 10) (+ y_offset 18) '(0 0xFFFFFF))
                    (img-clear text_box)
                    (setq ppm_status 0.0)
                })
              })

            (if (> (get-adc 1) 2.8) {
             (setq ppm_menu_stat 1.0)
            })

            (if(< (get-adc 1) 0.2) {
                (setq ppm_menu_stat 1.0) ; *EDIT - Changed 0.0 -> 1.0 to access PPM Menu*
                (setq iteration_data 0.0)
                (sleep 1)
            })
        ))
    )

(if (= iteration_data 0) {
    (if (= cfg_pressed_short 1){
        (setq cfg_pressed_short 0)
        (setq info_screen_num (+ info_screen_num 3))
        (if (> info_screen_num 3)
            {
            (setq info_screen_num 0)
            (setq iteration_data 0)
            (setq firts_iteration_info 0)
        })
    })}
    {
     (if (= cfg_pressed_long 1){
        (setq cfg_pressed_long 0)
        (setq cfg_pressed_short 0)
        (eeprom-store-f 12 date_aux)
        (setq data_rate (eeprom-read-f 12))
        (if(eq ppm_menu_stat 1.0)
        {
          (eeprom-store-i 13 ppm_status)
          (setq ppm_status (eeprom-read-i 13))
        })
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_info 0)
        (setq iteration_data 0)
        (setq info_screen_num 0)
        (setq ppm_menu_stat 0.0)
     })
 })
    (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (disp-clear)
        (setq firts_iteration 0)
        (setq iteration_data 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_info 0)
        (setq info_screen_num 0)
        (setq ppm_menu_stat 0.0)
     })
})
@const-end