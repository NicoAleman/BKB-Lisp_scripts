(def last_update_throttle 0.0)
(def throttle_time_out 0.0)
(def throttle_status 0) ; 0 for inhibited 1 for active
(def throttle 0.0)
(def raw_throttle 0.0)
(def change 0.0)
(def joy_Y 0.0)
(def vt_joy_Y 0.0)
(def joy_min 0)
(def joy_mid 0)
(def joy_max 0)

;; For logging in VESC Tool
(def vt_throttle_raw 0.0)
(def vt_throttle_filtered 0.0)

;; Filter coefficient (0.0 to 1.0)
;; Higher values = less filtering but lower latency
;; Lower values = more filtering but higher latency
;; (def STANDBY_THRESHOLD 0.005)
(def IDLE_THRESHOLD 0.01)
(def NOISE_THRESHOLD 0.10)    ; Scale from Small Alpha to Large Alpha as rate of change approaches this threshold
(def SMALL_CHANGE_ALPHA 0.1)  ; heavy filtering for noise / small changes
(def LARGE_CHANGE_ALPHA 1.0)  ; very light filtering for real, rapid movements

(def data_send_prescaler 0) 
(def vt_data_send_interval 1)
(def DATA_SEND_FAST 1)   ; Send Data 1 out of 1 Throttle Loops
(def DATA_SEND_SLOW 2)   ; Send Data 1 out of 2 Throttle Loops
(def DATA_SEND_IDLE 10)  ; Send Data 1 out of 10 Throttle Loops
;; (def DATA_SEND_STANDBY 20)  ; Send Data 1 out of 20 Throttle Loops

(def last_throttle_update 0)
(def THROTTLE_REFRESH_INTERVAL 20) ; 20ms = 50Hz

(defun throttle_th(){
    (loopwhile t{
        ; Return if not enough time has passed
        (if (> THROTTLE_REFRESH_INTERVAL (- (systime) last_throttle_update))
            nil
            (progn
                (setq last_throttle_update (systime))
                
                (setq joy_min (eeprom-read-i min_cal_add))
                (setq joy_mid (eeprom-read-i mid_cal_add))
                (setq joy_max (eeprom-read-i max_cal_add))
                (if (= menu_index 0) ;don't read the throttle if not in the main screean
                    (setq joy_Y (get-adc-raw))
                    (setq joy_Y joy_mid)
                )

                (setq vt_joy_Y joy_Y)

                (if (and (= thum_pressed_short 1) (= menu_index 0)){
                    (setq thum_pressed_short 0)
                    (setq throttle_time_out THR_TIMEOUT);feed timeout
                })
                (if (and (= throttle_status 1) (> joy_Y (+ joy_mid 100))){
                    (setq throttle_time_out THR_TIMEOUT);feed timeout
                })

                (setq throttle_time_out (- throttle_time_out (secs-since last_update_throttle)))
                (setq last_update_throttle (systime))

                (if (not-eq menu_index 0)
                    (setq throttle_time_out 0)
                )

                (if(<= throttle_time_out 0){
                    ;; (setq throttle_status safety_status) ; disable
                    (setq throttle_status 1) ;; Force Enable Throttle (Safety Switch Removed)
                }
                {
                    (setq throttle_status 1) ; enable
                })

                (if(= throttle_status 1){
                    (if (> joy_Y joy_mid)
                        (setq raw_throttle (utils_map joy_Y joy_mid joy_max 0.0 1.0))
                        (setq raw_throttle (* (utils_map joy_Y joy_mid joy_min 0.0 1.0) -1))
                    )
                }
                {
                    (if (<= joy_Y joy_mid)
                        (setq raw_throttle (* (utils_map joy_Y joy_mid joy_min 0.0 1.0) -1))
                    )
                })

                (setq change (abs (- raw_throttle throttle)))

                ;; Apply adaptive filter with smooth transition
                (if (<= change NOISE_THRESHOLD)
                    ;; Scale alpha smoothly between SMALL_CHANGE_ALPHA and LARGE_CHANGE_ALPHA
                    (let (
                        ;; Calculate how close we are to the threshold (0.0 to 1.0)
                        (scale_factor (/ change NOISE_THRESHOLD))
                        ;; Calculate the total range between our two alpha values
                        (alpha_range (- LARGE_CHANGE_ALPHA SMALL_CHANGE_ALPHA))
                        ;; Calculate the current alpha by interpolating between SMALL and LARGE
                        (current_alpha (+ SMALL_CHANGE_ALPHA (* scale_factor alpha_range)))
                    ){
                        ;; Apply EMA filter with our interpolated alpha
                        (setq throttle (+ (* current_alpha raw_throttle)                 ; Weight of new value
                                        (* (- 1.0 current_alpha) throttle)))             ; Weight of old value
                    })
                    
                    ;; For changes beyond NOISE_THRESHOLD, use LARGE_CHANGE_ALPHA
                    (setq throttle (+ (* LARGE_CHANGE_ALPHA raw_throttle)                ; Weight of new value
                                    (* (- 1.0 LARGE_CHANGE_ALPHA) throttle)))            ; Weight of old value
                )

                ;; Log raw and filtered throttle values for VESC Tool
                (setq vt_throttle_raw raw_throttle)
                (setq vt_throttle_filtered throttle)

                (cond ; Set data send interval based on throttle change magnitude
                    ((or(= batt_saver 0)(>= change NOISE_THRESHOLD))
                        (setq vt_data_send_interval DATA_SEND_FAST))
                    ((>= change IDLE_THRESHOLD)
                        (setq vt_data_send_interval DATA_SEND_SLOW))
                    (t
                        (setq vt_data_send_interval DATA_SEND_IDLE))
                )

                ;; Send data at prescribed interval
                (setq data_send_prescaler (+ data_send_prescaler 1))
                (if (= (mod data_send_prescaler vt_data_send_interval) 0)
                    (data_send)
                )
            )
        )
    })
})

(defun throttle_init(){
     (spawn 100 throttle_th)
})
