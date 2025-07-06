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
(def pairing_broadcast_received 0)  ; Flag to indicate if pairing_key_R came from a legitimate broadcast
(def broadcast_add '(255 255 255 255 255 255))
(def last_peer_packet 0.0)
(def is_data_received 0)
(def batt_saver)

; For logging in VESC Tool
(def vt_throttle_data 0.0)
(def vt_throttle_final 0.0)
(def vt_wifi_state 0)  ; 1.0 when WiFi is on, 0.0 when off

(def data_send_buffer (bufcreate 11)) ; Create once and reuse

(def wifi_lock 0)  ; Lock for WiFi state changes
(def waiting_for_response 0)  ; Track if we're waiting for a response

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
    
    ; Always try to read pairing key for debugging
    (var temp_pairing_key (bufget-i8 data 36))
    
    ; Check packet types
    (var is_from_peer (eq src peer))
    (var is_broadcast (eq des broadcast_add))
    (var is_pairing_packet (= temp_pairing_key 127))
    (var is_pairing_mode (and (= menu_index 1) (= pairing_found 0))) ; Only accept pairing if we haven't found one yet
    
    ; Handle packets from our configured peer (normal operation)
    (if is_from_peer {
        (setq pairing_key_R temp_pairing_key)
        (data_received data)
        (setq last_peer_packet (systime))
        (setq waiting_for_response 0)  ; Got our response
        (safe-wifi-stop)  ; Try to stop WiFi if conditions allow
    }
    {
        ; Handle pairing packets only when in pairing mode AND it's a true broadcast
        ; (Pairing packets are broadcast to 255,255,255,255,255,255, not sent to specific remote MAC)
        (if (and is_pairing_mode is_broadcast is_pairing_packet) {
            (print (list "PAIRING PACKET FOUND from:" src "rssi:" rssi "pairing_key:" temp_pairing_key "broadcast_dest:" des))
            (setq pairing_key_R temp_pairing_key)
            (setq pairing_broadcast_received 1)  ; Mark that this came from a legitimate broadcast
            (data_received data)
        }
        {
            ; Silently ignore all other packets including:
            ; - Regular data packets (sent to specific MAC, not broadcast)
            ; - Pairing packets when not in pairing mode
        })
    })
})

(defun event-handler ()
    (loopwhile t
        (recv
           ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (proc-data src des data rssi))
           (_ nil)
)))

(defun ensure-wifi-on () {
    (if (= vt_wifi_state 0) {
        (wifi-start)
        (setq vt_wifi_state 1)
    })
})

(defun safe-wifi-stop () {
    (if (and (= wifi_lock 0) (or (= waiting_for_response 0) (= pairing_status 0)) (= menu_index 0)) {
        (wifi-stop)
        (setq vt_wifi_state 0)
    })
})

(defun data_send() {
     (setq wifi_lock 1)  ; Acquire lock
     (ensure-wifi-on)
    
     (var current_throttle throttle)
     (var throttle_to_send throttle)
     (if (= (isCharging) 1)
        (setq throttle_to_send 0.0)
     )

     (setq vt_throttle_data current_throttle)
     (bufset-f32 data_send_buffer 0 throttle_to_send    'little-endian) ; throttle
     (bufset-i8 data_send_buffer 4 direction     ) ; direction
     (bufset-i8 data_send_buffer 5 torq_mode     ) ; torque mode
     (bufset-i8 data_send_buffer 6 pairing_key_T)
     (bufset-i8 data_send_buffer 7 ppm_status) ; send the ppm status
     (bufset-i8 data_send_buffer 8 uart_status) ; send the uart status
     (bufset-i8 data_send_buffer 9 return_analog) ; current button state

     (esp-now-send peer data_send_buffer)
     (setq vt_throttle_final current_throttle)

     (if (= pairing_status 0) {
         ; If not paired, schedule a delayed wifi stop
         (spawn (lambda () {
             (sleep 0.1)
             (safe-wifi-stop)
         }))
     } {
         ; If paired, wait for response from receiver
         (setq waiting_for_response 1)
     })

     (setq wifi_lock 0)  ; Release lock

    ;;  ; Not convinced that this helps, disabling for now
    ;;  (if (= batt_saver 1){
    ;;     (if (= menu_index 0) {
    ;;         ;;  (gpio-hold 20 1) ; latch the gpio_pin_20 before light sleep
    ;;         ;;  (gpio-hold-deepsleep 1)
    ;;         (sleep-light 0.02) ; Very short light sleep to save power
    ;;         (wifi-start)
    ;;         ;;  (gpio-hold 20 0)
    ;;         ;;  (gpio-hold-deepsleep 0)
    ;;     })
    ;;  })
})