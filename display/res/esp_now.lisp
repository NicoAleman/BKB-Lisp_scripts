(def mac-rx '())
(def data (bufcreate 55))
(def rpm          0.0)
(def cur_set      0.0)
(def vin          0.0)
(def rpm          0.0)
(def temp         0.0)
(def speed        0.0)
(def enable_throttle 0)
(def dist         0.0)
(def dist         0.0)
(def i_motor      0.0)
(def poles        14)
(def pulley       2.66)
(def wheel_diam   0.105)
(def batt_type    3)
(def rec_fw_may   0)
(def rec_fw_min   0)
(def rec_lisp_may 0)
(def rec_lisp_min 0)
(def rec_hw_name  "")
(def skate_fw_may 0)
(def skate_fw_min 0)
(def skate_hw_name "")
(def distance     0.0)
(def js_x         0.0); x axis
(def js_y         0.0); y axis
(def counter      0.0)
(def counter_1    0.0)
(def val   1.0)
(def val_1 0.0)
(def sleep_time   140.0)
(def pairing_status 0)
(def pairing_key_T 64)
(def pairing_key_R 0)
(def signal_level 0)
(def pair_source '(0 0 0 0 0 0))
(def broadcast_add '(255 255 255 255 255 255))
(def last_peer_packet 0.0)
(def is_data_received 0)
(def batt_saver)

; For logging in VESC Tool
(def vt_throttle_data 0.0)
(def vt_throttle_final 0.0)

(defun esp_now_init(){
    (esp-now-start)
    (esp-now-add-peer peer) ; add here the mac for the receiver, keep in mind this when pairing mode
    (event-register-handler (spawn event-handler))
    (event-enable 'event-esp-now-rx)

})

(defun data_received (data) {
    (setq is_data_received 1); added 9/24
    (setq rpm             (bufget-f32 data 0))
    (setq vin             (bufget-f32 data 4))
    (setq temp            (bufget-f32 data 8))
    (setq i_motor         (bufget-f32 data 12))
    (if(not-eq (bufget-i8  data 16) 0)
        (setq poles (bufget-i8  data 16))
    )
    (if (not-eq (bufget-f32 data 17) 0.0 )
        (setq pulley (bufget-f32 data 17))
    )
    (if (not-eq (bufget-f32 data 21) 0.0)
        (setq wheel_diam (bufget-f32 data 21))
    )
    (if (not-eq (bufget-i8  data 25) 0)
        (setq batt_type (bufget-i8  data 25))
    )
    (setq rec_fw_may      (bufget-i8  data 26))
    (setq rec_fw_min      (bufget-i8  data 27))
    (setq rec_lisp_may    (bufget-i8  data 28))
    (setq rec_lisp_min    (bufget-i8  data 29))
    (setq skate_fw_may    (bufget-i8  data 30))
    (setq skate_fw_min    (bufget-i8  data 31))
    (setq distance        (bufget-f32  data 32))
    (setq pairing_key_R    (bufget-i8  data 36))
    (setq pairing_status   (bufget-i8  data 37))
    (free data)
})


(defun proc-data (src des data rssi) {

    (setq pair_source src)
    (setq signal_level rssi)
    (setq pairing_key_R    (bufget-i8  data 36))

    (if (eq src peer) {
        ;(print (list "src:" src  "des:" des "data:" data "rssi:" rssi))
        (data_received data)
        (setq last_peer_packet (systime))
    })
})

(defun event-handler ()
    (loopwhile t
        (recv
           ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (proc-data src des data rssi))
           (_ nil)
)))


(defun data_send() {
     (var data_send (bufcreate 11))
     (var current_throttle throttle)

     (setq vt_throttle_data current_throttle)
     (bufset-f32 data_send 0 throttle      'little-endian); throttle
     (bufset-i8 data_send 4 direction     ); direction
     (bufset-i8 data_send 5 torq_mode     ); torque mode
     (bufset-i8 data_send 6 pairing_key_T)
     (bufset-i8 data_send 7 ppm_status); send the ppm status
     (bufset-i8 data_send 8 uart_status); send the uart status
     (bufset-i8 data_send 9 return_analog); current button state (CFG Button + Thumb Button)

     (esp-now-send peer data_send)
     (free data_send)
     (setq vt_throttle_final current_throttle)

     (if (= batt_saver 1){
        (if (= menu_index 0){
            (gpio-hold 20 1) ; latch the gpio_pin_20 before enter in light sleep mode
            (gpio-hold-deepsleep 1)
            (sleep-light sleep_time)   ;turn off the radio(wifi,bt), enter in light sleep mode.
            (sleep 0.01)
            (wifi-start)
            (gpio-hold 20 0)
            (gpio-hold-deepsleep 0)

         })
     })
})