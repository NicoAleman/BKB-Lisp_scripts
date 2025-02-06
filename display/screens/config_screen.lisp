(def menu_sub_index 0)
(def firts_iteration 0)
(def enter_menu 0)
(def thum_stick_prescaler 0)
(define menu_count 7)

@const-start
(defun config_screen(){
    (if (= firts_iteration 0){
        (disp-clear)
        (def title_box (img-buffer 'indexed2 128 30))
        (def text_box (img-buffer 'indexed2 45 14))
        (txt-block-l text_box 1 0 0  font_9x14 "EXIT")
        (disp-render text_box (+ x_offset 0) (+ y_offset 53) '(0 0xFFFFFF))
        (img-clear text_box)

        (txt-block-l text_box 1 0 0  font_9x14 "ENTER")
        (disp-render text_box (+ x_offset 80) (+ y_offset 53) '(0 0xFFFFFF))
        (img-clear text_box)

        (setq firts_iteration 1)

    })

    (cond
        ((eq menu_sub_index 0)
            (progn
                (if(= enter_menu 0){
                    (txt-block-c title_box 1 64 0  font_20x30 "CALIB")
                    (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                    (img-clear title_box)
                    (if (= cfg_pressed_short 1){
                        (setq cfg_pressed_short 0)
                        (setq cfg_pressed_long 0)
                        (disp-clear)
                        (setq enter_menu 1)
                    })
                }
                {
                (calib_screen)
                })

        ))
        ((eq menu_sub_index 2)
        (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "SYSTEM")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (info_screen)
             })

          ))
         ((eq menu_sub_index 1)
         (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "BOARD")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (esk8_screen)
             })
         ))
         ((eq menu_sub_index 3)
         (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "REMOTE")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (remote_screen)
             })
          ))
         ((eq menu_sub_index 4)
         (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "PAIR")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (pairing_screen)
             })
          ))
         ((eq menu_sub_index 5)
         (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "B.SAVE")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (batt_save_screen)
             })
          ))
         ((eq menu_sub_index 6)
         (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "PPM")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (PPM_screen)
             })
          ))
         ((eq menu_sub_index 7)
         (progn
            (if(= enter_menu 0){
                (txt-block-c title_box 1 64 0  font_20x30 "UART")
                (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
                (img-clear title_box)
                (if (= cfg_pressed_short 1){
                    (setq cfg_pressed_short 0)
                    (setq cfg_pressed_long 0)
                    (disp-clear)
                    (setq enter_menu 1)
                 })
             }
             {
             (UART_screen)
             })
          ))
        ;;  ((eq menu_sub_index 8)
        ;;  (progn
        ;;     (if(= enter_menu 0){
        ;;         (txt-block-c title_box 1 64 0  font_20x30 "SAFETY")
        ;;         (disp-render title_box (+ x_offset 0) (+ y_offset 15) '(0 0xFFFFFF))
        ;;         (img-clear title_box)
        ;;         (if (= cfg_pressed_short 1){
        ;;             (setq cfg_pressed_short 0)
        ;;             (setq cfg_pressed_long 0)
        ;;             (disp-clear)
        ;;             (setq enter_menu 1)
        ;;          })
        ;;      }
        ;;      {
        ;;      (safe_throttle_screen)
        ;;      })
        ;;   ))
    )

    (setq thum_stick_prescaler (+ thum_stick_prescaler 1))
    (if (and (> thum_stick_prescaler 10) (= enter_menu 0)){
        (if(< (get-adc 0) 0.8){
            (setq menu_sub_index (+ menu_sub_index 1))
            (if (> menu_sub_index menu_count) {
                (setq menu_sub_index 0)
            })
        })

        (if(> (get-adc 0) 2){
            (setq menu_sub_index (- menu_sub_index 1))
            (if (< menu_sub_index 0) {
                (setq menu_sub_index menu_count)
            })
        })
        (setq thum_stick_prescaler 0)
    })

    (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (setq cfg_pressed_long 0)
        (setq first_draw 1)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq menu_index 0)
    })
})
@const-end