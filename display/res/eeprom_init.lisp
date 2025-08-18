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
(define units_add 21)

; Default values
(define default_min_cal 100) ; Default based on typical production remotes
(define default_mid_cal 2500) ; Default based on typical production remotes
(define default_max_cal 3900) ; Default based on typical production remotes
(define default_torq_mode 2) ; Default to High (no limiting to forward throttle, AKA Nose Down tilting)
(define default_total_trip 0.0)
(define default_pair_mac 0) ; Default to 0 (no pairing)
(define default_data_rate 0.06)
(define default_ppm_status 0) ; Default OFF (Assume UART by default)
(define default_batt_saver 1) ; Default ON
(define default_uart_status 1) ; Default ON
(define default_motor_poles 30) ; 30 Poles
(define default_wheel_diam 0.280) ; 280mm
(define default_gear_ratio 1.0) ; 1:1
(define default_s_count 18)
(define default_safety_switch 0)
(define init_flag 0xFFFE) ; Switched to 0xFFFE for v1.50 release to load new defaults
(define default_units 0) ; Default to Imperial (0 = Imperial, 1 = Metric)

(defun eeprom_check(){
    (setq test_value (to-i (eeprom-read-i 1))) ; Calibration (Min)
    (if(< test_value 1){
        (print "eeprom 1 error, writing default")
        (eeprom-store-i 1 default_min_cal)
    })
    (setq test_value (to-i (eeprom-read-i 2))) ; Calibration (Mid)
    (if(or (< test_value 1)(> test_value 4096)){
        (print "eeprom 2 error, writing default")
        (eeprom-store-i 2 default_mid_cal)
    })
    (setq test_value (to-i (eeprom-read-i 3))) ; Calibration (Max)
    (if(or (< test_value 1)(> test_value 4096)){
        (print "eeprom 3 error, writing default")
        (eeprom-store-i 3 default_max_cal)
    })
    (setq test_value (to-i (eeprom-read-i 4))) ; Torque Mode (Now used as Throttle Mode: 0 = Low, 1 = Mid, 2 = High, 3 = Sport [UNUSED])
    (if(or (< test_value 0)(> test_value 2)){
        (print "eeprom 4 error, writing default")
        (eeprom-store-i 4 default_torq_mode)
    })
    (setq test_value (to-i (eeprom-read-i 6))) ; Pairing MAC 0
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 5 error, writing default")
        (eeprom-store-i 6 default_pair_mac)
    })
    (setq test_value (to-i (eeprom-read-i 7))) ; Pairing MAC 1
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 6 error, writing default")
        (eeprom-store-i 7 default_pair_mac)
    })
    (setq test_value (to-i (eeprom-read-i 8))) ; Pairing MAC 2
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 7 error, writing default")
        (eeprom-store-i 8 default_pair_mac)
    })
    (setq test_value (to-i (eeprom-read-i 9))) ; Pairing MAC 3
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 9 error, writing default")
        (eeprom-store-i 9 default_pair_mac)
    })
    (setq test_value (to-i (eeprom-read-i 10))) ; Pairing MAC 4
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 10 error, writing default")
        (eeprom-store-i 10 default_pair_mac)
    })
    (setq test_value (to-i (eeprom-read-i 11))) ; Pairing MAC 5
    (if(or (< test_value 0)(> test_value 255)){
        (print "eeprom 11 error, writing default")
        (eeprom-store-i 11 default_pair_mac)
    })
    (setq test_value (to-float (eeprom-read-f 12))) ; Data Rate [UNUSED]
    (if(or (< test_value 0.03)(> test_value 0.120)){
        (print "eeprom 12 error, writing default")
        (eeprom-store-f 12 default_data_rate)
    })
    (setq test_value (to-i (eeprom-read-i 13))) ; PPM Status
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 13 error, writing default")
        (eeprom-store-i 13 default_ppm_status)
    })
    (setq test_value (to-i (eeprom-read-i 14))) ; Battery Saver
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 14 error, writing default")
        (eeprom-store-i 14 default_batt_saver)
    })
    (setq test_value (to-i (eeprom-read-i 15))) ; UART Status
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 15 error, writing default")
        (eeprom-store-i 15 default_uart_status)
    })
    (setq test_value (to-i (eeprom-read-i 16))) ; Motor Poles
    (if(or (< test_value 16)(> test_value 50)){
        (print "eeprom 16 error (motor poles), writing default: 30")
        (eeprom-store-i 16 default_motor_poles)
    })
    (setq test_value (to-float (eeprom-read-f 17))) ; Wheel Diameter
    (if(or (< test_value 0.2)(> test_value 0.8)){
        (print "eeprom 17 error (wheel diameter), writing default: 0.280")
        (eeprom-store-f 17 default_wheel_diam)
    })
    (setq test_value (to-float (eeprom-read-f 18))) ; Gear Ratio
    (if(not-eq test_value 1.0){ ; Force Off (Onewheel-only)
        (print "eeprom 18 error (gear ratio), writing default: 1.0")
        (eeprom-store-f 18 default_gear_ratio)
    })
    (setq test_value (to-i (eeprom-read-i 19))) ; S-Count
    (if(or (< test_value 10)(> test_value 100)){
        (print "eeprom 19 error (s-count), writing default: 18")
        (eeprom-store-i 19 default_s_count)
    })
    (setq test_value (to-i (eeprom-read-i 20))) ; Safety Switch
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 20 error (safety status), writing default: 0")
        (eeprom-store-i 20 default_safety_switch)
    })
    (setq test_value (to-i (eeprom-read-i 21))) ; Units
    (if(or (< test_value 0)(> test_value 1)){
        (print "eeprom 21 error, writing default")
        (eeprom-store-i 21 default_units)
    })
})

(defun eeprom_init(){

    (setq test_value (to-i (eeprom-read-i 32)))

    ; MEMORY NOT INITIALIZED
    (if(< test_value init_flag){
            (print "Memory not initialized, writing default values")
            (eeprom-store-i 1 default_min_cal) ; Calibration (Min)
            (eeprom-store-i 2 default_mid_cal) ; Calibration (Mid)
            (eeprom-store-i 3 default_max_cal) ; Calibration (Max)
            (eeprom-store-i 4 default_torq_mode) ; Torque Mode (Now used as Throttle Mode)
            (eeprom-store-f 5 default_total_trip) ; Total Trip
            (eeprom-store-i 6 default_pair_mac) ; Pairing MAC 0
            (eeprom-store-i 7 default_pair_mac) ; Pairing MAC 1
            (eeprom-store-i 8 default_pair_mac) ; Pairing MAC 2
            (eeprom-store-i 9 default_pair_mac) ; Pairing MAC 3
            (eeprom-store-i 10 default_pair_mac) ; Pairing MAC 4
            (eeprom-store-i 11 default_pair_mac) ; Pairing MAC 5
            (eeprom-store-f 12 default_data_rate) ; Data Rate [UNUSED]
            (eeprom-store-i 13 default_ppm_status) ; PPM Status
            (eeprom-store-i 14 default_batt_saver) ; Battery Saver
            (eeprom-store-i 15 default_uart_status) ; UART Status
            (eeprom-store-i 16 default_motor_poles) ; Motor Poles
            (eeprom-store-f 17 default_wheel_diam) ; Wheel Diameter
            (eeprom-store-f 18 default_gear_ratio) ; Gear Ratio
            (eeprom-store-i 19 default_s_count) ; S-Count
            (eeprom-store-i 20 default_safety_switch) ; Safety Switch
            (eeprom-store-i 21 default_units) ; Units
            (eeprom-store-i 22 0)
            (eeprom-store-i 23 0)
            (eeprom-store-i 24 0)
            (eeprom-store-i 25 0)
            (eeprom-store-i 26 0)
            (eeprom-store-i 27 0)
            (eeprom-store-i 29 0)
            (eeprom-store-i 30 0)
            (eeprom-store-i 31 0)
            (eeprom-store-i 32 init_flag) ; Initialization Flag
    })

    ; NEW MEMORY DEFAULTS - MAINTAIN EXISTING PAIRING MAC (v1.50)
    (if(> test_value init_flag){
            (print "First time on Onewheel Version (1.50+), writing new default values [maintaining pairing MAC]")
            (eeprom-store-i 1 default_min_cal) ; Calibration (Min)
            (eeprom-store-i 2 default_mid_cal) ; Calibration (Mid)
            (eeprom-store-i 3 default_max_cal) ; Calibration (Max)
            (eeprom-store-i 4 default_torq_mode) ; Torque Mode (Now used as Throttle Mode)
            (eeprom-store-f 12 default_data_rate) ; Data Rate [UNUSED]
            (eeprom-store-i 13 default_ppm_status) ; PPM Status
            (eeprom-store-i 14 default_batt_saver) ; Battery Saver
            (eeprom-store-i 15 default_uart_status) ; UART Status
            (eeprom-store-i 16 default_motor_poles) ; Motor Poles
            (eeprom-store-f 17 default_wheel_diam) ; Wheel Diameter
            (eeprom-store-f 18 default_gear_ratio) ; Gear Ratio
            (eeprom-store-i 19 default_s_count) ; S-Count
            (eeprom-store-i 20 default_safety_switch) ; Safety Switch
            (eeprom-store-i 21 default_units) ; Units
            (eeprom-store-i 22 0)
            (eeprom-store-i 23 0)
            (eeprom-store-i 24 0)
            (eeprom-store-i 25 0)
            (eeprom-store-i 26 0)
            (eeprom-store-i 27 0)
            (eeprom-store-i 29 0)
            (eeprom-store-i 30 0)
            (eeprom-store-i 31 0)
            (eeprom-store-i 32 init_flag) ; Initialization Flag
    })

    (eeprom_check)
)}
