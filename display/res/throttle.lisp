
(def last_update_throttle 0.0)
(def throttle_time_out 0.0)
(def throttle_status 0) ; 0 for inhibited 1 for active
(def throttle 0.0)
(def joy_Y 0.0)
(def joy_min 0)
(def joy_mid 0)
(def joy_max 0)

(defun throttle_th(){
    (loopwhile t{

        (setq joy_min (eeprom-read-i min_cal_add))
        (setq joy_mid (eeprom-read-i mid_cal_add))
        (setq joy_max (eeprom-read-i max_cal_add))
        (if (= menu_index 0) ;don't read the throttle if not in the main screean
            (setq joy_Y (get-adc-raw))
            (setq joy_Y joy_mid)
        )

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
            (setq throttle_status 1) ; *EDIT - CHANGED TO NO TIME OUT (0 -> 1)*
        }
        {
            (setq throttle_status 1) ; enable
        })

        (if(= throttle_status 1){
            (if (> joy_Y joy_mid)
                (setq throttle (utils_map joy_Y joy_mid joy_max 0.0 1.0))
                (setq throttle (* (utils_map joy_Y joy_mid joy_min 0.0 1.0) -1))
            )
        }
        {
            (if (<= joy_Y joy_mid)
                (setq throttle (* (utils_map joy_Y joy_mid joy_min 0.0 1.0) -1))
            )
        })
    (sleep 0.05);
    })
})

(defun throttle_init(){
     (spawn 50 throttle_th)
})
