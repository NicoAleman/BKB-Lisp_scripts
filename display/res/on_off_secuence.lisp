(def on_count 0)
(def break_flag 1)
(def input_on)
(define on_butt 10)
(define latch_pin 20)
(def temp_trip 0.0)
@const-start
(defun off_sequence(){
    
    (setq temp_trip (eeprom-read-f total_trip_add))
    (setq temp_trip (+ temp_trip (if (= distance 0) 0 (/ distance 1000))))

    (eeprom-store-f total_trip_add (to-float temp_trip)) ; store the total trip in Km;

    (gpio-write latch_pin 0)
    ;configure USB pins as output and set them to low
    (gpio-configure 18 'pin-mode-out)
    (gpio-configure 19 'pin-mode-out)
    (gpio-write 18 0)
    (gpio-write 19 0)
    (print "off")
    (sleep 1) ;the MCU will turn off
          
})

(defun on_sequence(){
    (loopwhile (= break_flag 1) {
        (gpio-write 20 0)
        (if (= (gpio-read 10) 1)
            (setq on_count (+ on_count 1))
            (progn
            ;is chargin, show chargin screen, else shut down
            (off_sequence)
            )
        )
        (sleep 0.1)
        (if(> on_count 20)
            (setq break_flag 0) 
        )
    })
    (gpio-write 20 1)  ;latch output
    (disp-render-jpg logo (+ x_offset 0) (+ y_offset 12))
    (sleep 2)
      
})
@const-end