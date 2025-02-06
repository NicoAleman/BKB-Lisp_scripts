(def speed_color 1)

(def last_charging_state 0)

(def last_displayed_torq_mode 0)
(def last_displayed_pairing_status 0)
(def last_displayed_direction 0)
(def last_displayed_soc 0.0)
(def last_board_soc 0.0)
(def last_displayed_current 0)
(def last_displayed_speed 0.0)
(def last_displayed_trip 0.0)

(def first_draw 1)  ; Flag for first drawing

@const-start
(defun draw_main_screen(){
    (setq last_screen_update (systime))

    ;; ;; *EDIT - REMOVED GREEN COLOR FOR SAFETY SWITCH DISABLED*
    ;; (if(= throttle_status 1)
    ;;     (setq speed_color 2)
    ;;     (setq speed_color 1)
    ;; )

    ;; THROTTLE SCALE ;;
    (if (or (!= torq_mode last_displayed_torq_mode)
            (= first_draw 1))
        (progn
            (write_mode torq_mode (+ x_offset 2) (+ y_offset 20))
            (setq last_displayed_torq_mode torq_mode)
        )
    )
    ;; ;;;;;;;;;;;;;; ;;

    ;; CONNECTION STATUS ;;
    (if (> (secs-since last_peer_packet) 1)
        (setq pairing_status 0)
    )
    (if (or (!= pairing_status last_displayed_pairing_status)
            (= first_draw 1))
        (progn
            (write_online pairing_status (+ x_offset 95) (+ y_offset 19))
            (setq last_displayed_pairing_status pairing_status)
        )
    )
    ;; ;;;;;;;;;;;;;;;;; ;;

    ;; DIRECTION ;;
    (if (or (!= direction last_displayed_direction)
            (= first_draw 1))
        (progn
            (write_direction direction (+ x_offset 59) (+ y_offset 0))
            (setq last_displayed_direction direction)
        )
    )
    ;; ;;;;;;;;;; ;;

    ;; REMOTE SOC ;;
    (def current_soc (read_SOC))
    (def current_display_soc (to-i (* current_soc 100)))  ; For comparing displayed values

    ; Update when: first draw, display value changed, or charging
    (if (or (!= current_display_soc last_displayed_soc)
            (= (isCharging) 1)
            (= first_draw 1))
        (progn
            (bat_soc current_soc 2.5 4.25 0 1 (+ x_offset 85) (+ y_offset 0))
            (setq last_displayed_soc current_display_soc)
        )
    )
    (setq last_charging_state (isCharging))
    ;; ;;;;;;;;;; ;;

    ;; BOARD SOC ;;
    (setq vin_min (* batt_type_config 3.0))
    (setq vin_max (* batt_type_config 4.2))
    (def current_board_soc (to-i (* vin 100)))  ; For comparing displayed values

    ; Update when: first draw or displayed value changed
    (if (or (!= current_board_soc last_board_soc)
            (= first_draw 1))
        (progn
            (bat_soc vin vin_min vin_max 0 0 (+ x_offset 8) (+ y_offset 0))
            (setq last_board_soc current_board_soc)
        )
    )
    ;; ;;;;;;;;;; ;;

    ;; MOTOR CURRENT ;;
    (def current_display_current (to-i I_motor))  ; For comparing displayed values
    (if (or (!= current_display_current last_displayed_current)
            (= first_draw 1))
        (progn
            (write_amps I_motor (+ x_offset 3) (+ y_offset 50))
            (setq last_displayed_current current_display_current)
        )
    )
    ;; ;;;;;;;;;; ;;

    ;; SPEED ;;
    (def current_speed (if (= UNITS 1) (speed_cal) (* (speed_cal) 0.621)))

    (def current_display_speed (to-i (* current_speed 10)))  ; For comparing displayed values
    (if (or (!= current_display_speed last_displayed_speed)
            (= first_draw 1))
        (progn
            (write-speed current_speed UNITS (+ x_offset (if (= UNITS 1) 33 28)) (+ y_offset 19) speed_color)
            (setq last_displayed_speed current_display_speed)
        )
    )
    ;; ;;;;; ;;

    ;; TRIP ;;
    (def current_trip (if (= UNITS 1) (distance) (* distance 0.621)))

    (def current_display_trip (to-i (* current_trip 10)))  ; For comparing displayed values
    (if (or (!= current_display_trip last_displayed_trip)
            (= first_draw 1))
        (progn
            (write_trip current_trip UNITS (+ x_offset 63) (+ y_offset 50))
            (setq last_displayed_trip current_display_trip)
        )
    )
    ;; ;;;; ;;

    (setq first_draw 0)
})
@const-end
