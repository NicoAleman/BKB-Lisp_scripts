
(def exit 1)
(def firts_iteration_pair 0)
(def iteration_counter_pair 0)

(defun pairing_screen (){
    (if (= firts_iteration_pair 0){
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
        (setq pairing_key_R   0)
        (setq signal_level -1000)
        (setq peer (list 255 255 255 255 255 255))
        (esp-now-add-peer peer)
        (def text_box (img-buffer 'indexed2 127 14))
        (setq firts_iteration_pair 1)
     })

     (if (and (= pairing_key_R 127) (> signal_level -80)){
        (txt-block-l text_box 1 0 0  font_9x14 "Pair found!")
        (disp-render text_box (+ x_offset 1) (+ y_offset 1) '(0 0xFFFFFF))
        (img-clear text_box)
        (def aux_mac 0)
        (setq aux_mac(ix pair_source 3))
        (txt-block-l text_box 1 10 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
        (setq aux_mac(ix pair_source 4))
        (txt-block-c text_box 1 64 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
        (setq aux_mac(ix pair_source 5))
        (txt-block-l text_box 1 93 0  font_9x14  (str-from-n (to-i aux_mac) "%03d"))
        (disp-render text_box (+ x_offset 0) (+ y_offset 17) '(0 0xFFFFFF))
        (img-clear text_box)

        (setq peer pair_source)
     }
     {
        (setq pairing_key_R   0)
        (setq signal_level -1000)
        (disp-render text_box (+ x_offset 0) (+ y_offset 17) '(0 0xFFFFFF))
        (txt-block-l text_box 1 0 0 font_9x14 "Searching")
        (txt-block-l text_box 1 83 0 font_9x14  "   ")

        (setq iteration_counter_pair (+ iteration_counter_pair 1))
        (if (> iteration_counter_pair 40) (setq iteration_counter_pair 0))
        (if (> iteration_counter_pair 10){
            (txt-block-l text_box 1 83 0 font_9x14  ".")
            (if (> iteration_counter_pair 20){
                (txt-block-l text_box 1 92 0 font_9x14  ".")
                (if (> iteration_counter_pair 30){
                    (txt-block-l text_box 1 101 0 font_9x14  ".")
                )}
             )}
        )}

        (disp-render text_box (+ x_offset 1) (+ y_offset 1) '(0 0xFFFFFF))
        (img-clear text_box)

        (var pairing_buff (bufcreate 9))
        (bufset-f32 pairing_buff 0 0.00001); for some reason when the first element of the buffer is 0, the rest is filled with 0
        (bufset-i8 pairing_buff 6 64)
        (bufset-i8 pairing_buff 7 ppm_status) ; add ppm and uart status to avoid missing data information about the communication
        (bufset-i8 pairing_buff 8 uart_status);
        (esp-now-send peer pairing_buff)
        (free pairing_buff)

     })

     (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_pair 0)
        (setq mac_0 (to-i (eeprom-read-i pair0_add)))
        (setq mac_1 (to-i (eeprom-read-i pair1_add)))
        (setq mac_2 (to-i (eeprom-read-i pair2_add)))
        (setq mac_3 (to-i (eeprom-read-i pair3_add)))
        (setq mac_4 (to-i (eeprom-read-i pair4_add)))
        (setq mac_5 (to-i (eeprom-read-i pair5_add)))

        (setq peer (list mac_0 mac_1 mac_2 mac_3 mac_4 mac_5))
        (esp-now-add-peer peer)

     })

     (if (= cfg_pressed_long 1){
        (setq cfg_pressed_long 0)
        (setq cfg_pressed_short 0)
        (eeprom-store-i pair0_add (ix peer 0))
        (eeprom-store-i pair1_add (ix peer 1))
        (eeprom-store-i pair2_add (ix peer 2))
        (eeprom-store-i pair3_add (ix peer 3))
        (eeprom-store-i pair4_add (ix peer 4))
        (eeprom-store-i pair5_add (ix peer 5))
        (esp-now-add-peer peer)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_pair 0)

     })
})


