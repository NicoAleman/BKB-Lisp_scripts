(def exit 1)
(def firts_iteration_pair 0)
(def iteration_counter_pair 0)
(def last_animation_update 0)  ; Track last animation update
(def last_pairing_screen_update 0)  ; Track last screen update for internal rate limiting
(def pairing_debug_counter 0)  ; For debugging crashes
(def last_pairing_key_check 0)  ; Track pairing key changes
(def pairing_found 0)  ; Flag to stop processing more pairing packets once found
(def pair_screen_displayed 0)  ; Flag to only log "Displaying pair found screen" once

(defun pairing_screen (){
    ; Increment debug counter and log periodically
    (setq pairing_debug_counter (+ pairing_debug_counter 1))
    (if (= (- pairing_debug_counter (* (/ pairing_debug_counter 100) 100)) 0)  ; Log every 100 calls
        (print (list "Pairing debug:" pairing_debug_counter "time:" (systime) "pairing_key_R:" pairing_key_R "signal_level:" signal_level "found:" pairing_found))
    )
    
    ; Check if pairing_key_R has changed AND it came from a legitimate broadcast (only if we haven't found a pair yet)
    (if (and (= pairing_found 0) (= pairing_broadcast_received 1) (not (= pairing_key_R last_pairing_key_check))) {
        (print (list "Pairing broadcast received from" last_pairing_key_check "to" pairing_key_R "signal:" signal_level))
        (setq last_pairing_key_check pairing_key_R)
        (setq pairing_broadcast_received 0)  ; Reset the flag
        
        ; Check if we've found a valid pair
        (if (and (= pairing_key_R 127) (> signal_level -80)) {
            (print "PAIRING FOUND! Pairing instantly...")
            (setq pairing_found 1)
            
            ; Send pairing response immediately (key 128)
            (print "About to send pairing response...")
            (var temp_buffer (bufcreate 11))
            (if temp_buffer {
                (bufset-f32 temp_buffer 0 0.0)    ; throttle
                (bufset-i8 temp_buffer 4 1)       ; direction
                (bufset-i8 temp_buffer 5 0)       ; torque mode
                (bufset-i8 temp_buffer 6 128)     ; pairing response key
                (bufset-i8 temp_buffer 7 0)       ; ppm status
                (bufset-i8 temp_buffer 8 0)       ; uart status
                (bufset-i8 temp_buffer 9 0)       ; button state
                
                (print "Buffer created, sending pairing response...")
                (esp-now-send pair_source temp_buffer)
                (print "Pairing response sent, freeing buffer...")
                (free temp_buffer)
                (print "Pairing response completed successfully")
            }
            {
                (print "ERROR: Failed to create pairing response buffer!")
            })
            
            ; Save pairing data to EEPROM immediately
            (eeprom-store-i pair0_add (ix pair_source 0))
            (eeprom-store-i pair1_add (ix pair_source 1))
            (eeprom-store-i pair2_add (ix pair_source 2))
            (eeprom-store-i pair3_add (ix pair_source 3))
            (eeprom-store-i pair4_add (ix pair_source 4))
            (eeprom-store-i pair5_add (ix pair_source 5))
            
            ; Update peer and ensure ESP-NOW is properly configured
            (setq peer pair_source)
            (esp-now-add-peer peer)
            (sleep 0.1)  ; Small delay to ensure ESP-NOW updates
            (print "Pairing saved to EEPROM and ESP-NOW updated")
        }
        {
            (print (list "Pairing conditions not met: pairing_key_R=" pairing_key_R "signal_level=" signal_level "need key=127 and signal>-80"))
        })
    })
    
    ; Internal rate limiting for pairing screen only - limit to 10Hz (100ms)
    (if (< (- (systime) last_pairing_screen_update) 100)
        (sleep (/ (- 100 (- (systime) last_pairing_screen_update)) 1000.0))
    )
    (setq last_pairing_screen_update (systime))
    
    (if (= firts_iteration_pair 0){
        ; Create buffers
        (def text_box (img-buffer 'indexed2 127 14))
        (def text_box_small (img-buffer 'indexed2 36 14))
        
        ; Render EXIT button
        (txt-block-l text_box_small 1 0 0  font_9x14 "EXIT")
        (disp-render text_box_small (+ x_offset 1) (+ y_offset 49) '(0 0xFFFFFF))
        (img-clear text_box_small)
        
        ; Initialize pairing variables (but don't reset pairing_found if it was already set)
        (setq pairing_key_R   0)
        (setq signal_level -1000)
        (setq last_pairing_key_check 0)
        (if (not (= pairing_found 1)) (setq pairing_found 0))  ; Don't reset if already paired
        (setq pair_screen_displayed 0)  ; Reset display flag
        (setq pairing_broadcast_received 0)  ; Reset broadcast flag
        
        ; Set up ESP-NOW to listen for broadcast packets during pairing
        (setq peer (list 255 255 255 255 255 255))  ; Broadcast address
        (esp-now-add-peer peer)
        
        (setq last_animation_update (systime))
        
        ; Show "Searching..." text immediately on first load
        (img-clear text_box)
        (txt-block-l text_box 1 0 0 font_9x14 "Searching...")
        (disp-render text_box (+ x_offset 0) (+ y_offset 17) '(0 0xFFFFFF))
        
        (print "Now listening for Pairing requests (*Cold boot board/receiver to pair*)...")
        (setq firts_iteration_pair 1)
    })

    ; Show pair found screen if we detected a valid pair
    (if (= pairing_found 1) {
        (if (= pair_screen_displayed 0) {
             (print "Displaying receiver paired screen")
             (setq pair_screen_displayed 1)
        })
         
        (txt-block-l text_box 1 0 0  font_9x14 "Board Paired!")
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
        ; Only show listening animation if we haven't found a pair yet
        (if (= pairing_found 0) {            
            ; Only update animation every 100ms (10fps)
            (if (> (- (systime) last_animation_update) 100) {
                (setq iteration_counter_pair (+ iteration_counter_pair 1))
                (if (> iteration_counter_pair 20) (setq iteration_counter_pair 0))  ; 20 Frame Loop
                
                (img-clear text_box)
                (txt-block-l text_box 1 0 0 font_9x14 "Searching")
                
                ; Faster ellipses animation - reduced frame thresholds
                (cond 
                    ((> iteration_counter_pair 15)  ; 15 frames in
                        (txt-block-l text_box 1 83 0 font_9x14 "..."))
                    ((> iteration_counter_pair 10)  ; 10 frames in
                        (txt-block-l text_box 1 83 0 font_9x14 ".."))
                    ((> iteration_counter_pair 5)   ; 5 frames in
                        (txt-block-l text_box 1 83 0 font_9x14 "."))
                    (t 
                        (txt-block-l text_box 1 83 0 font_9x14 "   ")))
                
                (disp-render text_box (+ x_offset 0) (+ y_offset 17) '(0 0xFFFFFF))
                (setq last_animation_update (systime))
            })
        })
    })

    (if (= on_pressed_short 1){
        (setq on_pressed_short 0)
        (disp-clear)
        (setq firts_iteration 0)
        (setq menu_sub_index 0)
        (setq enter_menu 0)
        (setq firts_iteration_pair 0)
        (setq pairing_debug_counter 0)  ; Reset debug counter
        (setq pairing_found 0)  ; Reset pairing found flag
        (setq pair_screen_displayed 0)  ; Reset display flag
        
        ; Restore normal ESP-NOW peer configuration
        (print "Restoring normal ESP-NOW configuration")
        
        ; If we just paired successfully, peer is already set correctly, so don't reload from EEPROM
        (if (= pairing_found 0) {
            (setq mac_0 (to-i (eeprom-read-i pair0_add)))
            (setq mac_1 (to-i (eeprom-read-i pair1_add)))
            (setq mac_2 (to-i (eeprom-read-i pair2_add)))
            (setq mac_3 (to-i (eeprom-read-i pair3_add)))
            (setq mac_4 (to-i (eeprom-read-i pair4_add)))
            (setq mac_5 (to-i (eeprom-read-i pair5_add)))
            (setq peer (list mac_0 mac_1 mac_2 mac_3 mac_4 mac_5))
        })

        ; Ensure the peer is added (whether from fresh pairing or EEPROM)
        (esp-now-add-peer peer)
        (print "Restored peer configuration:")
        (print peer)
    })

    ; SAVE button logic removed - pairing now happens instantly
})


