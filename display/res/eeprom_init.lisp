(def test_value)
(define min_cal_add 1)
(define mid_cal_add 2)
(define max_cal_add 3)
(define torq_mode_add 4)
(define total_trip_add 5)
(define pair0_add 6)
(define pair1_add 7)
(define pair2_add 8)
(define pair3_add 9)
(define pair4_add 10)
(define pair5_add 11)
(define data_index 12)
(define ppm_status_add  13)
(define batt_saver_add  14)
(define uart_status_add 15)
(define poles_add       16)
(define wheel_diam_add  17)
(define pulley_add      18)
(define batt_type_add   19)
(define safety_status_add 20)

(defun eeprom_check(){
    (setq test_value (to-i (eeprom-read-i 1)))
    (if(< test_value 1){
        (print "eeprom 1 error, writing default")
        (eeprom-store-i 1 20)
    })
    (setq test_value (to-i (eeprom-read-i 2)))
    (if(or (< test_value 1)(> test_value 4096)){
        (print "eeprom 2 error, writing default")
        (eeprom-store-i 2 2048)
    })
    (setq test_value (to-i (eeprom-read-i 3)))
    (if(or (< test_value 1)(> test_value 4096)){
        (print "eeprom 3 error, writing default")
        (eeprom-store-i 3 4076)
    })
    (setq test_value (to-i (eeprom-read-i 4)))
    (if(or (< test_value 1)(> test_value 4)){
        (print "eeprom 4 error, writing default")
        (eeprom-store-i 4 1)
    })
    (setq test_value (to-i (eeprom-read-i 6)))
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 5 error, writing default")
        (eeprom-store-i 6 0)
    })
    (setq test_value (to-i (eeprom-read-i 7)))
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 6 error, writing default")
        (eeprom-store-i 7 0)
    })
    (setq test_value (to-i (eeprom-read-i 8)))
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 7 error, writing default")
        (eeprom-store-i 8 0)
    })
    (setq test_value (to-i (eeprom-read-i 9)))
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 9 error, writing default")
        (eeprom-store-i 9 0)
    })
    (setq test_value (to-i (eeprom-read-i 10)))
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 10 error, writing default")
        (eeprom-store-i 10 0)
    })
    (setq test_value (to-i (eeprom-read-i 11)))
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 11 error, writing default")
        (eeprom-store-i 11 0)
    })
    (setq test_value (to-float (eeprom-read-f 12)))
    (if(or (< test_value 0.03)(> test_value 0.120)){
        (print "eeprom 12 error, writing default")
        (eeprom-store-f 12 0.06)
    })
    (setq test_value (to-i (eeprom-read-i 13)))
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 13 error, writing default")
        (eeprom-store-i 13 0)
    })
    (setq test_value (to-i (eeprom-read-i 14)))
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 14 error, writing default")
        (eeprom-store-i 14 0)
    })
    (setq test_value (to-i (eeprom-read-i 15)))
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 15 error, writing default")
        (eeprom-store-i 15 0)
    })
    (setq test_value (to-i (eeprom-read-i 16))) ; motor poles
    (if(or (< test_value 16)(> test_value 50)){
        (print "eeprom 16 error (motor poles), writing default: 30")
        (eeprom-store-i 16 30)
    })
    (setq test_value (to-float (eeprom-read-f 17))) ; wheel diameter
    (if(or (< test_value 0.2)(> test_value 0.8)){
        (print "eeprom 17 error (wheel diameter), writing default: 0.280")
        (eeprom-store-f 17 0.280)
    })
    (setq test_value (to-float (eeprom-read-f 18))) ; gear ratio
    (if(not-eq test_value 1.0){ ; Force Off (Onewheel-only)
        (print "eeprom 18 error (gear ratio), writing default: 1.0")
        (eeprom-store-f 18 1.0)
    })
    (setq test_value (to-i (eeprom-read-i 19))) ; s-count
    (if(or (< test_value 10)(> test_value 100)){
        (print "eeprom 19 error (s-count), writing default: 18")
        (eeprom-store-i 19 18)
    })
    (setq test_value (to-i (eeprom-read-i 20))) ; safety status
    (if(not-eq test_value 0){ ; Force Off (Onewheel-only)
        (print "eeprom 20 error (safety status), writing default: 0")
        (eeprom-store-i 20 0)
    })
})

(defun eeprom_init(){

    (setq test_value (to-i (eeprom-read-i 32)))
    (if(not-eq test_value 0xFFFF){
            (print "memoty not initialized, writing default values")
            (eeprom-store-i 1 20)
            (eeprom-store-i 2 2048)
            (eeprom-store-i 3 4076)
            (eeprom-store-i 4 1)
            (eeprom-store-f 5 0.0)
            (eeprom-store-i 6 0)
            (eeprom-store-i 7 0)
            (eeprom-store-i 8 0)
            (eeprom-store-i 9 0)
            (eeprom-store-i 10 0)
            (eeprom-store-i 11 0)
            (eeprom-store-f 12 0.06); default data rate
            (eeprom-store-i 13 0) ; ppm status
            (eeprom-store-i 14 0)
            (eeprom-store-i 15 0)
            (eeprom-store-i 16 30) ; motor poles
            (eeprom-store-i 17 0.280) ; wheel diameter
            (eeprom-store-i 18 1.0) ; gear ratio
            (eeprom-store-i 19 18) ; s-count
            (eeprom-store-i 20 0) ; Default Safety-Switch to Disabled
            (eeprom-store-i 21 0)
            (eeprom-store-i 22 0)
            (eeprom-store-i 23 0)
            (eeprom-store-i 24 0)
            (eeprom-store-i 25 0)
            (eeprom-store-i 26 0)
            (eeprom-store-i 27 0)
            (eeprom-store-i 29 0)
            (eeprom-store-i 30 0)
            (eeprom-store-i 31 0)
            (eeprom-store-i 32 0xFFFF)

    })
    (eeprom_check)
)}
