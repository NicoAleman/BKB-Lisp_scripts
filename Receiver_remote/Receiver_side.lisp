;Script adapted for FW versions starting from 6.00. For FW version 6.0.5 or higher
;it is necessary to install the code-server. code-server allows the execution of
;other functionalities that are not implemented in 6.00 version.
(sleep 10)
(def FW_VERSION 6.02)

(import "UART_protocol/uart_protocol.lisp" 'uart_protocol)
(read-eval-program uart_protocol)

(if (>= FW_VERSION 6.05){
(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)
})

; values to send
; current motor (get value)
; conn status (online)
; battery level [V]
; poles
; pulley
; wheel diameter
; battery type
; FW, HW , LISP version
; ESC version
; distance Km

(def can-id -1)
(def peer '())
(def broadcast_add '(255 255 255 255 255 255))
(def mac-tx          '())
(def set_cur         0.0)
(def data (bufcreate 40))
(def set_ana         0.0)
(def vin             0.0)
(def rpm             0.0)
(def temp            0.0)
(def speed           0.0)
(def distance        1.0)
(def button_state    0.0)
(def enable_throttle 0.0)
(def I_motor         0.0)
(def poles           14.0)
(def pulley          2.66)
(def wheel_diam      0.105)
(def batt_type       3.0)
(def rec_fw_may      0.0)
(def rec_fw_min      0.0)
(def rec_lisp_may    0.0)
(def rec_lisp_min    0.0)
(def skate_fw_may    0.0)
(def skate_fw_min    0.0)
(def time            0.0)
(def secs            0.0)
(def last_time       0.0)
(def throttle        0.0)
(def direction         0)
(def torq_mode         0)
(def aux      1.0)
(def aux_1    1.0) ; to avoid division by zero in speed algorithm in the remote side
(def flag_l      0)
(def flag_m      0)
(def flag_h      0)
(def flag_s      0)
(def first_start 1)
(def is_data_received 0.0)
(def store_mac  0)
(def mac_0       0)
(def mac_1       0)
(def mac_2       0)
(def mac_3       0)
(def mac_4       0)
(def mac_5       0)
(def mac_aux    '())
(def load_mac   0)
(def cont 0)
(def pairing_status 1)
(def pairing_key    0)
(def data_rate 0.01)
(def signal_level   0)
(def throttle_ppm 0.0)
(def throttle_dead_band 0.0)
(def throttle_scale 0)
(def dead 0.0)
(def ppm_input 0.0)
(def ppm_status 0.0)
(def ppm_config 0.0)
(def uart_status 0)
(def uart_status_init 0)
(def no_app_config 0.0)
(def can_enabled 0)
(def start_time 0)
(def last_package_received 0.0)
(to-u64 last_package_received)
(def PPM_timeout 0.3) ; [sec] time out for PPM received

(def is_uart_start     0)
(def is_ppm_start      0)
(def currenT_val 0.0)
;TODO: List all can devices and check if the listed ID's belong to an ESC controller.
;it can be done through FW version, HW or so.
;define a master ESC in case a dual controller is connected.
;Could be defined with the esc which has connected the receiver.

(defun utils_map(x in_min in_max out_min out_max)
(/ (* (- x in_min) (- out_max out_min)) (+ (- in_max in_min) out_min))
)

(defun dead_band(value tres max)
(if (< (abs value) tres) {
      (setq value 0.0)
    }
    {(setq dead (/ max (- max tres)))
      (if (> value 0.0)
            (setq value (+ (* dead value) (* max (- 1.0 dead))))
            (setq value (* -1.0 (+ (* dead (* -1.0 value)) (* max (- 1.0 dead)))))
      )
     }
    )
  (return value)
 )

(defun ppm-start(freq duty chan pin res) {
  (pwm-start freq duty chan pin res); for PPM functionality
})

(defun utils_truncate ( pwr min max)
 (progn
  (cond ((> pwr max) (setq Throttle_ppm max))
        ((< pwr min) (setq Throttle_ppm min))
  )
 )
)

(defunret scan-can-device (can-id) { ;
    (if (< can-id 0) {
        (var can-devices (can-scan))
        (setq can-id (first (can-scan)))
    })

    (can-cmd can-id "(conf-set 'can-status-msgs-r1 0x3F )") ; set the CAN msg status that will be shown in the remote.

    (return can-id)
})

(defun ppm_scale_range (ppm_scale) {
    (cond
        ((eq ppm_scale 0.25)(utils_truncate throttle_ppm 0.1 0.625))
        ((eq ppm_scale 0.50)(utils_truncate throttle_ppm 0.1 0.75))
        ((eq ppm_scale 0.75)(utils_truncate throttle_ppm 0.1 0.875))
        ((eq ppm_scale 1.0) (utils_truncate throttle_ppm 0.1 0.97))
       }
    )
 )

(defun data-received (data) {

    (setq last_package_received (systime)) ; save time stamp of the last package received
    (setq throttle     (bufget-f32 data 0  'little-endian))
    (setq direction    (bufget-i8  data 4))
    (setq torq_mode    (bufget-i8  data 5)) ; torque mode
    (setq pairing_key  (bufget-i8  data 6)) ; get the pairing key 67
    (setq ppm_status   (bufget-i8  data 7)) ; get the ppm mode.
    (setq uart_status  (bufget-i8  data 8)) ; get the uart mode.
    (setq throttle_ppm (utils_map throttle -1.0 1.0 0.0 1.0))
    ;(utils_truncate throttle_ppm 0.1 0.97) ; truncate the values for the throttle ppm
    (ppm_scale_range throttle_scale)
    (setq throttle_dead_band (dead_band throttle 0.2 1.0))

    (if (eq uart_status 1) {
        (if (= is_uart_start 0) {
            (setq is_ppm_start 0)
            (pwm-stop 0)
            (uart-init)
            (print "Uart started")
            (setq is_uart_start 1)
            (eeprom-store-i 6 uart_status) ; store uart status to be used when the receiver starts
            (setq uart_status_init (to-i(eeprom-read-i 6)))
         })
        (if (< throttle 0.02) {
            (setq COMM_SET_CURRENT_ID 7) ; current brake
            (setq current_val 10000) ; this current brake is set to 10A, (configurable by menu?)
            }
            {
            (setq COMM_SET_CURRENT_ID 84) ; current rel
            (setq current_val 100000)
            })
        (if(= direction 1)(setq direction 1)(setq direction -1))
        (uart-send)
        (setq no_app_config 0.0)
    })

    (if (eq ppm_status 1) {
        (if (= is_ppm_start 0) {
            (uart-stop)
            (setq is_uart_start 0)
            (ppm-start 50 throttle_ppm 0 21 13)
            (print "ppm started")
            (setq is_ppm_start 1)
            (eeprom-store-i 7 ppm_status)
         })
        (pwm-set-duty throttle_ppm 0);
        (setq no_app_config 0.0)
    })

    ;; Disable Dangerous CAN functionality for Onewheels (set current is very dangerous)

    ;; (if(and (eq uart_status 0)(eq ppm_status 0)) {
    ;;    (setq is_ppm_start 0)
    ;;    (setq is_uart_start 0)
    ;;    (eeprom-store-i 6 uart_status)
    ;;    (eeprom-store-i 7 ppm_status)
    ;;    (setq uart_status_init (to-i(eeprom-read-i 6)))

    ;;     (if (eq no_app_config 0.0) {
    ;;         (print "CAN-BUS started")
    ;;         (setq can-id (to-i (eeprom-read-i 8))) ; load CAN-BUS id to avoid error when switch from UART to CAN after a power cycling
    ;;         (can-cmd can-id "(conf-set 'app-to-use 0)")(setq no_app_config 1.0)
    ;;     })

    ;;     (if (>= FW_VERSION 6.05) {
    ;;         (rcode-run-noret can-id (list 'set-remote-state (* throttle throttle_scale) 0 0 0  direction)) ; to use with FW 6.05+
    ;;         }
    ;;     {
    ;;         (if (> throttle_dead_band 0.2 ) {
    ;;             (if (= direction 1) (setq direction 1)(setq direction -1))
    ;;             (canset-current-rel can-id (*(* throttle_dead_band direction) throttle_scale ))
    ;;         }
    ;;         {
    ;;         (if (and (< throttle_dead_band 0.2)(> throttle_dead_band -0.2)) {
    ;;             (canset-current-rel can-id 0.0)
    ;;             })
    ;;         })
    ;;         (if(< throttle_dead_band -0.2) {
    ;;             (canset-brake-rel can-id throttle_dead_band)
    ;;         })
    ;;     })
    })

  (free data)
 }
)

;Parameters from ESC to be shown in the remote

(defun data_to_send (data_send) {

  (if (= uart_status 0) {
      ;(print "No uart enable")
      (setq rpm     (canget-rpm can-id))
      (setq vin     (canget-vin can-id))
      (setq temp    (canget-temp-fet can-id))
      (setq speed   (canget-speed can-id))
      (setq I_motor (canget-current can-id))

      (bufset-f32 data_send 0 (+ rpm 0.01)); from CAN
      (bufset-f32 data_send 4  vin); from CAN
      (bufset-f32 data_send 12 I_motor) ; from CAN
      (bufset-f32 data_send 32 distance); from CAN
      (print distance)
     }
     {
      (bufset-f32 data_send 0  (to-float erpm_l));erpm from UART
      (bufset-f32 data_send 4  (/ (to-float voltage) 10.0)); from UART
      (bufset-f32 data_send 12 (/ (to-float current) 100)); from UART
      (bufset-f32 data_send 32  (to-float distance-uart)); from UART
      ;(bufset-f32 data_send 32 (/ (to-float (* odometer 0.000621371)) 1)); from UART
     }

     )

      (bufset-f32 data_send 8  temp)
      (bufset-i8  data_send 16 poles)
      (bufset-f32 data_send 17 pulley)
      (bufset-f32 data_send 21 wheel_diam)
      (bufset-i8  data_send 25 batt_type)
      (bufset-i8  data_send 26 rec_fw_may)
      (bufset-i8  data_send 27 rec_fw_min )
      (bufset-i8  data_send 28 rec_lisp_may)
      (bufset-i8  data_send 29 rec_lisp_min)
      (bufset-i8  data_send 30 skate_fw_may)
      (bufset-i8  data_send 31 skate_fw_min)
      (bufset-i8  data_send 36 127); sends 127 as pairing key
      (bufset-i8  data_send 37 pairing_status) ; send connection status

    (if (= pairing_key 64) { ;64
      (esp-now-send mac-tx data_send)
     }
    )
  }
)

(defun proc-data (src des data rssi) {
    ;(print (list "src:" src  "des:" des "data:" data "rssi:" rssi))

    (if (eq des broadcast_add) {
        (print "broadcasting")
    })
    (setq signal_level rssi)
    (setq pairing_key  (bufget-i8 data 6))

    (if (eq src mac-tx) {
        ;(print (list "src:" src  "des:" des "data:" data "rssi:" rssi))
        (setq is_data_received 1.0); to ensure when a data is received the ESP start sending
        (data-received data)
    })

    (setq cont (+ cont 1))

    (if (< cont 10) {
        (print "listening")
     (if (and (= pairing_key 64)(> signal_level -80)) {

        (eeprom-store-i 0 (ix src 0))
        (eeprom-store-i 1 (ix src 1))
        (eeprom-store-i 2 (ix src 2))
        (eeprom-store-i 3 (ix src 3))
        (eeprom-store-i 4 (ix src 4))
        (eeprom-store-i 5 (ix src 5))

        (eeprom-set-mac)
        (setq mac-tx (list mac_0 mac_1 mac_2 mac_3 mac_4 mac_5)) ; desired mac for pairing
        (print mac-tx)
    }

    { (eeprom-set-mac)    ; load a default mac address when time pairing is finished
      (setq mac-tx (list mac_0 mac_1 mac_2 mac_3 mac_4 mac_5)) ;
      (print "use default mac")
      (print mac-tx)
      (setq cont 0)
    }
   )
  }
 )
    ;(print mac-tx)
    (esp-now-add-peer mac-tx)
 }
)

(defun event-handler ()
    (loopwhile t
        (recv
           ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (proc-data src des data rssi))
           (_ nil)
)))

(defun main () {
    (print "Self mac" (get-mac-addr))
    (setq uart_status_init (to-i(eeprom-read-i 6)))
    (if (= uart_status_init 0) {
        (setq can-id (scan-can-device can-id)) ; when ppm is enabled just use
        (eeprom-store-i 8 can-id)
        (print "Can device:" can-id)
        (print "Listening...")}
    )
    (esp-now-start)
    (esp-now-add-peer broadcast_add)

    (event-register-handler (spawn event-handler))
    (event-enable 'event-esp-now-rx)

    (param-motor)
    (loop-state)
  }
)

; all motor info or esc info could be added in this thread
(defun param-motor () {
    (loopwhile-thd 60 t {

    (if (>= FW_VERSION 6.05) {
        (if (not-eq distance timeout) {
            (setq distance (rcode-run can-id 0.1 '(get-dist-abs)))
        })
        (if (eq distance timeout) {
            (print "dist:")(print distance)(setq distance aux)
        }
        {
            (setq aux distance)
        })
    }
    {
        (setq distance (canget-dist can-id)) ; for version 6.00 is not absolute
    })
   (if (>= FW_VERSION 6.05) {
      (if (not-eq poles timeout) {
            (setq poles (rcode-run can-id 0.1 '(conf-get 'si-motor-poles)))
               })
            (if (eq poles timeout) {(print "poles:")(print poles)(setq poles aux_1)
               }
            {(setq aux_1 poles)}
            )

    (if (not-eq pulley timeout) {
            (setq pulley (rcode-run can-id 0.1 '(conf-get 'si-gear-ratio)))
               })
            (if (eq pulley timeout) {(print "pulley:")(print pulley)(setq pulley aux_1)
               }
            {(setq aux_1 pulley)}
            )

    (if (not-eq wheel_diam timeout) {
            (setq wheel_diam (rcode-run can-id 0.1 '(conf-get 'si-wheel-diameter)))
               })
            (if (eq wheel_diam timeout) {(print "wheel_diam:")(print wheel_diam)(setq wheel_diam aux_1)
               }
            {(setq aux_1 wheel_diam)}
            )

    (if (not-eq batt_type timeout) {
            (setq batt_type (rcode-run can-id 0.1 '(conf-get 'si-battery-cells)))
               })
            (if (eq batt_type timeout) {(print "batt_type:")(print batt_type)(setq batt_type aux_1)
               }
            {(setq aux_1 batt_type)}
            )
         }
       )
     (sleep 1.0)
    }
   )
  }
 )

(defun eeprom-init-mac () {
    (eeprom-store-i 0 255) ; here initialize MAC address, starts the pairing mode with a power cycle. The remote
    (eeprom-store-i 1 255) ; needs to add the peer as broadcasting.
    (eeprom-store-i 2 255)
    (eeprom-store-i 3 255)
    (eeprom-store-i 4 255)
    (eeprom-store-i 5 255)
   }
)

(defun eeprom-set-mac() {
    (setq mac_0 (to-i (eeprom-read-i 0)))
    (setq mac_1 (to-i (eeprom-read-i 1)))
    (setq mac_2 (to-i (eeprom-read-i 2)))
    (setq mac_3 (to-i (eeprom-read-i 3)))
    (setq mac_4 (to-i (eeprom-read-i 4)))
    (setq mac_5 (to-i (eeprom-read-i 5)))
})

(define set_torq_prescaler 0)
(defun set-motor-torque() {

    (setq set_torq_prescaler (+ set_torq_prescaler 1))

    (if (= set_torq_prescaler 10){
        (setq set_torq_prescaler 0)
        (cond
            ((eq torq_mode 0){
                (if (= flag_l 0){ ; avoid re entry
                    ;(rcode-run-noret can-id  '(conf-set 'l-current-max-scale 0.25))
                    ;(can-cmd can-id (str-from-n 0.25 "(conf-set 'l-current-max-scale %.2f)"))
                    (setq flag_l 1)
                    (setq flag_m 0)
                    (setq flag_h 0)
                    (setq flag_s 0)
                    (setq throttle_scale 0.25)
                })
            })
            ((eq torq_mode 1){
                (if (= flag_m 0){ ; avoid re entry
                    ;(rcode-run-noret can-id '(conf-set 'l-current-max-scale 0.50))
                    ;(can-cmd can-id (str-from-n 0.50 "(conf-set 'l-current-max-scale %.2f)")) ; for Version 6.00
                    (setq flag_l 0)
                    (setq flag_m 1)
                    (setq flag_h 0)
                    (setq flag_s 0)
                    (setq throttle_scale 0.50)
                })
            })
            ((eq torq_mode 2){
                (if (= flag_h 0){ ; avoid re entry
                    ;(rcode-run-noret can-id '(conf-set 'l-current-max-scale 0.75))
                    ;(can-cmd can-id (str-from-n 0.75 "(conf-set 'l-current-max-scale %.2f)")) ; for Version 6.00
                    (setq flag_l 0)
                    (setq flag_m 0)
                    (setq flag_h 1)
                    (setq flag_s 0)
                    (setq throttle_scale 0.75)
                })
            })
            ((eq torq_mode 3){
                (if (= flag_s 0){ ; avoid re entry
                    ;(rcode-run-noret can-id '(conf-set 'l-current-max-scale 1.0))
                    ;(can-cmd can-id (str-from-n 1.0 "(conf-set 'l-current-max-scale %.2f)")) ; for Version 6.00
                    (setq flag_l 0)
                    (setq flag_m 0)
                    (setq flag_h 0)
                    (setq flag_s 1)
                    (setq throttle_scale 1.0)
                })
            })
         )
     })
 })

(defun loop-state () {
    (loopwhile-thd 50 t {
        (var data_send (bufcreate 55))
        (if (= is_data_received 1.0) {
            (data_to_send data_send)
            (set-motor-torque)
            (setq is_data_received 0.0)
         }
        {;else
           (if (> (secs-since last_package_received) PPM_timeout){
                (print "PPM time out")
                (pwm-stop 0)
            })
        })
        (free data_send)
        (sleep 0.02)
       }
     )
  }
)

;starts from here
(main)
