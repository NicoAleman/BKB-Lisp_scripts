(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

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

(def can-id 13)
;(def other-peer '(255 255 255 255 255 255))
;(def peer '(52 183 218 163 205 411)); Mac board n 1
;(def peer '(52 183 218 163 95 233)) ; Mac board n 2
;(def peer '(52 183 218 164 13 205)) ; Mac board n 3
;(def peer '(52 183 218 164 59 205)) ; Mac board n 4
(def peer '(52 183 218 164 59 197)) ; Mac board n 5
;(def peer '(52 183 218 164 10 141)) ; Mac board n 9
(def mac-tx          '())
(def set_cur         0.0)
(def set_ana         0.0)
(def vin             0.0)
(def rpm             0.0)
(def temp            0.0)
(def speed           0.0)
(def distance        0.0)
(def button_state    0.0)
(def enable_throttle 0.0)
(def I_motor         0.0)
(def poles           0.0)
(def pulley          0.0)
(def wheel_diam      0.0)
(def batt_type       0.0)
(def rec_fw_may      0.0)
(def rec_fw_min      0.0)
(def rec_lisp_may    0.0)
(def rec_lisp_min    0.0)
(def skate_fw_may    0.0)
(def skate_fw_min    0.0)
(def time            0.0)
(def secs            0.0)
(def last_time       0.0)
(def throttle 0.0)
(def direction 0)
(def torq_mode 0)
(def aux      0.0)
(def flag_l      0)
(def flag_m      0)
(def flag_h      0)
(def flag_s      0)
(esp-now-start)
(print (get-mac-addr))
;(esp-now-add-peer other-peer)
(esp-now-add-peer peer)

(defun data-received (data) {

     (setq throttle     (bufget-f32 data 0  'little-endian))
     (setq direction    (bufget-i8  data 4))
     (setq torq_mode    (bufget-i8  data 5)) ; torque mode


     (rcode-run-noret can-id (list 'set-remote-state throttle 0 0 0 direction))
     (free data)
  }
)

;Parameters from ESC to be shown in the remote

(defun data_to_send (data) {

      (setq rpm     (canget-rpm can-id))
      (setq vin     (canget-vin can-id))
      (setq temp    (canget-temp-fet can-id))
      (setq speed   (canget-speed can-id))
      ;(setq distance (rcode-run can-id 0.0 '(get-dist-abs)))

      (if (not-eq distance timeout) {
            (setq distance (rcode-run can-id 0.1 '(get-dist-abs)))
               })
            (if (eq distance timeout) {(setq distance aux)
               }
            {(setq aux distance)}
            )
       ;(sleep 0.2)

      (setq I_motor (canget-current can-id))

      (bufset-f32 data 0  (+ rpm 0.01))
      (bufset-f32 data 4  vin)
      (bufset-f32 data 8  temp)
      (bufset-f32 data 12 I_motor)
      (bufset-i8  data 16 poles)
      (bufset-f32 data 17 pulley)
      (bufset-f32 data 21 wheel_diam)
      (bufset-i8  data 25 batt_type)
      (bufset-i8  data 26 rec_fw_may)
      (bufset-i8  data 27 rec_fw_min )
      (bufset-i8  data 28 rec_lisp_may)
      (bufset-i8  data 29 rec_lisp_min)
      (bufset-i8  data 30 skate_fw_may)
      (bufset-i8  data 31 skate_fw_min)
      (bufset-f32 data 32 distance)
      (esp-now-send mac-tx data)
  }
)

(defun proc-data (src des data rssi) {
     ;(print (list "src:" src  "des:" des "data:" data "rssi:" rssi))
     (setq mac-tx src)
     (data-received data)
     (esp-now-add-peer mac-tx)
   })

(defun event-handler ()
    (loopwhile t
        (recv
           ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (proc-data src des data rssi))
           (_ nil)
)))

(defun main () {
    (event-register-handler (spawn event-handler))
    (event-enable 'event-esp-now-rx)
    (loop-state)
    (param-motor)
    (set-motor-torque)
  }
)

; all motor info or esc info could be added in this thread
(defun param-motor () {
    (loopwhile-thd 50 t {
       (setq poles       (rcode-run can-id 0.0 '(conf-get 'si-motor-poles))); read the config parameters
       (setq pulley      (rcode-run can-id 0.0 '(conf-get 'si-gear-ratio))); read the config parameters
       (setq wheel_diam  (rcode-run can-id 0.0 '(conf-get 'si-wheel-diameter))); read the config parameters
       (setq batt_type   (rcode-run can-id 0.0 '(conf-get 'si-battery-cells)))
       (sleep 1.0)
    }
   )
  }
 )


(defun set-motor-torque() {

    (loopwhile-thd 50 t {
     (if (and (= torq_mode 0.0) (= flag_l 0)) {
    (rcode-run-noret can-id  '(conf-set 'l-current-max-scale 0.25)) ; 0.15
     (setq flag_l 1) (setq flag_s 0)
     (print "0")
      }
     )
     (if (and (= torq_mode 1.0) (= flag_m 0)) {
     (rcode-run-noret can-id '(conf-set 'l-current-max-scale 0.50)) ; 0.18
     (setq flag_m 1) (setq flag_l 0)
     (print "1")
       }
      )
     (if (and (= torq_mode 2.0) (= flag_h 0)) {
     (rcode-run-noret can-id '(conf-set 'l-current-max-scale 0.75)) ; 0.22
     (setq flag_h 1) (setq flag_m 0)
      (print "2")
       }
      )
     (if (and (= torq_mode 3.0) (= flag_s 0)) {
     (rcode-run-noret can-id '(conf-set 'l-current-max-scale 1.0)) ; 0.35
     (setq flag_s 1) (setq flag_h 0)
     (print "3")
       }
      )
   (sleep 0.5)
    }
   )
  }
 )

(defun loop-state () {
    (loopwhile-thd 50 t {
        (var data (bufcreate 40))
        (data_to_send data)
        ;(print (list "Input Voltage" (rcode-run can-id 0.1 '(get-batt))))
        (free data)
        (sleep 0.2)
    }
   )
  }
 )

;starts from here
(main)



