; includes
(define lisp_V 1.21)
(define THR_TIMEOUT 2.5) ; 5 for 160Mhz
(def UNITS 0); 0--> imperial 1--> metric

(def mac_0 0)
(def mac_1 0)
(def mac_2 0)
(def mac_3 0)
(def mac_4 0)
(def mac_5 0)
; Un comment the mac to connect with
;(def peer '(255 255 255 255 255 255)) ; Mac broadcast for pairing
(def peer '(0 0 0 0 0 0));


(import "pkg::disp-text@://vesc_packages/lib_disp_ui/disp_ui.vescpkg" 'disp-text)
(import "pkg::disp-text@://vesc_packages/lib_disp_ui/disp_ui.vescpkg" 'disp-text)
(import "res/BKB_LOGO.jpg" 'logo)
(import "res/CUSTOM_LOGO.jpg" 'logo_custom)
(import "res/display_init.lisp" 'display_init)
(import "res/soc_box.lisp" 'soc_box)
(import "fonts/font_9x14.bin" font_9x14)
(import "fonts/font_11x14_b.bin" font_11x14_b)
(import "fonts/font_20x30.bin" font_20x30)
(import "fonts/font_13x16_b.bin" font_13x16_b)
(import "res/speed_box.lisp" 'speed_box)
(import "res/dir_box.lisp" 'dir_box)
(import "res/conn_box.lisp" 'conn_box)
(import "res/mode_box.lisp" 'mode_box)
(import "res/amps_box.lisp" 'amps_box)
(import "res/trip_box.lisp" 'trip_box)
(import "screens/calib_screen.lisp" 'calib_screen)
(import "res/on_off_secuence.lisp" 'on_off_secuence)
(import "res/read_inputs.lisp" 'read_inputs)
(import "screens/main_screen.lisp" 'main_screen)
(import "screens/config_screen.lisp" 'config_screen)
(import "screens/info_screen.lisp" 'info_screen)
(import "screens/esk8_screen.lisp" 'esk8_screen)
(import "screens/remote_screen.lisp" 'remote_screen)
(import "res/eeprom_init.lisp" 'eeprom_init)
(import "res/throttle.lisp" 'throttle)
(import "res/esp_now.lisp" 'esp_now)
(import "screens/pairing_screen.lisp" 'pairing_screen)
(import "screens/batt_save_screen.lisp" 'battery_saver_screen)
(import "screens/PPM_screen.lisp" 'ppm_screen)
(import "screens/UART_screen.lisp" 'uart_screen)
(import "screens/CANBUS_screen.lisp" 'canbus_screen)
(import "screens/safe_throttle_screen.lisp" 'safe_throttle_screen)
(read-eval-program disp-text)
(read-eval-program speed_box)
(read-eval-program display_init)
(read-eval-program soc_box)
(read-eval-program dir_box)
(read-eval-program conn_box)
(read-eval-program mode_box)
(read-eval-program amps_box)
(read-eval-program trip_box)
(read-eval-program calib_screen)
(read-eval-program on_off_secuence)
(read-eval-program read_inputs)
(read-eval-program main_screen)
(read-eval-program config_screen)
(read-eval-program info_screen)
(read-eval-program esk8_screen)
(read-eval-program remote_screen)
(read-eval-program eeprom_init)
(read-eval-program throttle)
(read-eval-program esp_now)
(read-eval-program pairing_screen)
(read-eval-program battery_saver_screen)
(read-eval-program ppm_screen)
(read-eval-program uart_screen)
(read-eval-program canbus_screen)
(read-eval-program safe_throttle_screen)
; display initialization
(display_init)

(def last_screen_update 0)
(def time_since_screen_update 0)
(def SCREEN_REFRESH_INTERVAL 100) ; 100ms = 10Hz

(def direction 1)
(def menu_index 0)
(def main_prescaler 0)
(def torq_mode 0)
(def data_rate 0.0)
(eeprom_init)

;init mac and pair

(print "Self mac" (get-mac-addr)) ; self mac address
(setq mac_0 (to-i (eeprom-read-i pair0_add)))
(setq mac_1 (to-i (eeprom-read-i pair1_add)))
(setq mac_2 (to-i (eeprom-read-i pair2_add)))
(setq mac_3 (to-i (eeprom-read-i pair3_add)))
(setq mac_4 (to-i (eeprom-read-i pair4_add)))
(setq mac_5 (to-i (eeprom-read-i pair5_add)))
(setq peer (list mac_0 mac_1 mac_2 mac_3 mac_4 mac_5))
(print "Peer mac" peer)
(throttle_init)
(setq torq_mode (to-i(eeprom-read-i torq_mode_add)))
(setq data_rate (to-float(eeprom-read-f data_index))) ; load default data rate value
(setq ppm_status (to-i(eeprom-read-i ppm_status_add))); load default ppm state value
(setq uart_status (to-i (eeprom-read-i uart_status_add))); load default uart state value
(setq batt_saver (to-i(eeprom-read-i batt_saver_add))); load defaul battery saver config
(setq poles_config (to-i (eeprom-read-i poles_add))) ; load default poles value
(setq wheel_diam_config (to-float (eeprom-read-f wheel_diam_add))) ; load default wheel diammeter
(setq pulley_config (to-float (eeprom-read-f pulley_add))) ; load default pulley value
(setq batt_type_config (to-i (eeprom-read-i batt_type_add))) ; load default batt_type
(setq safety_status (to-i (eeprom-read-i safety_status_add))) ; load the safety status for throttle

(esp_now_init)
; display thread
(defun display_th(){
    (loopwhile t {
        ;; (setq main_prescaler (+ main_prescaler 1))
        (cond 
            ((eq menu_index 0) (progn 
                (draw_main_screen)

                ; Handle direction change on short cfg button press
                (if (and (= cfg_pressed_short 1) (< (get-adc-raw) (+ (eeprom-read-i min_cal_add) 80))) {
                    (setq direction (if (= direction 1) 0 1))
                    (setq cfg_pressed_short 0)
                })

                ; Handle torque mode change on short on button press
                (if (and (= on_pressed_short 1) (< (get-adc-raw) (+ (eeprom-read-i min_cal_add) 80))) {
                    (setq torq_mode (+ torq_mode 1))
                    (if (> torq_mode 2)
                        (setq torq_mode 0)
                    )
                    (eeprom-store-i torq_mode_add torq_mode)
                    (setq on_pressed_short 0)
                })

                ; Enter config mode on long press of cfg button
                (if (= cfg_pressed_long 1) {
                    (setq cfg_pressed_long 0)
                    (setq menu_index 1)
                })))
            ((eq menu_index 1) (config_screen)))

        ; Only power off on long press of on button
        (if (= on_pressed_long 1) {
            (off_sequence)
            (setq on_pressed_long 0)
        })

        (setq sleep_time data_rate)

        ; Sleep if not enough time has passed (elapsed time is less than our interval)
        (setq last_screen_update (systime))
        (setq time_since_screen_update (- (systime) last_screen_update))
        (if (and (eq menu_index 0) (> SCREEN_REFRESH_INTERVAL time_since_screen_update))
            (sleep (/ (- SCREEN_REFRESH_INTERVAL time_since_screen_update) 1000.0))  ; Convert ms to seconds
        )
    })
})

(defun inputs_th(){
    (loopwhile t{
        ; read on button
        (read_on)
        (read_cfg)
        (read_thum)
        (sleep 0.1)
    })
})

; spawn threads
(spawn 100 display_th)
(spawn 50 inputs_th)