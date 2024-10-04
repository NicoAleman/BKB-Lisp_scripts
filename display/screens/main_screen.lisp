@const-start
(defun draw_main_screen(){
    (def speed_color)
    (if(= throttle_status 1)
        ;; (setq speed_color 2) ; *EDIT - REMOVED GREEN COLOR FOR THROTTLE ENABLED*
        (setq speed_color 1)
    )
    (write_mode torq_mode (+ x_offset 2) (+ y_offset 20))

    (if (> (secs-since last_peer_packet) 1)
       (setq pairing_status 0)
    )
    (write_online pairing_status (+ x_offset 95) (+ y_offset 19))
    (write_direction direction (+ x_offset 59) (+ y_offset 0))
    (bat_soc (read_SOC) 2.5 4.25 0 1 (+ x_offset 85) (+ y_offset 0))
    (setq vin_min (* batt_type 2.5)); set min and max voltage according series cells
    (setq vin_max (* batt_type 4.2))
    (bat_soc vin vin_min vin_max 0 0  (+ x_offset 8) (+ y_offset 0))
    (write_amps I_motor (+ x_offset 3) (+ y_offset 50))
    (if (= UNITS 1){
         (write-speed (speed_cal) 1 (+ x_offset 33) (+ y_offset 19) speed_color)          ; km
         (write_trip distance 1 (+ x_offset 63) (+ y_offset 50))
    }
    {
        (write-speed (* (speed_cal) 0.621) 0 (+ x_offset 33) (+ y_offset 19) speed_color)  ; miles
        (write_trip distance 0 (+ x_offset 63) (+ y_offset 50))
    })
})
@const-end
