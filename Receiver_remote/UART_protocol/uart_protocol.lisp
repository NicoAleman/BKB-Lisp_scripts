; Implement UART protocol to send current commands and get some datas from the ESC.

; Data structure
(def COMM_SET_CURRENT       6)
(def COMM_SET_BRAKE_CURRENT 7)
(def COMM_GET_VALUES       47)
(def BUF-SIZE 80)


(define buffer         (bufcreate 10))
(define buffer_motor   (bufcreate 10))
(define checksum       (bufcreate 5))
(define uart-buf       (bufcreate BUF-SIZE))

(def crc_buffer '())
(def crc_buffer_data '())

(def voltage)
(def voltage-uart)
(def erpm_l)
(def erpm_h)
(def temp)
(def temp-fet)
(def distance-uart)
(def distance-uart-abs)
(def speed-uart)
(def duty-cycle)
(def current)
(def batt-current)
(def batt-level)
(def ah-tot)
(def ah-charge-tot)
(def wh-tot)
(def wh-charge-tot)
(def pid-pos)
(def fault-code)
(def controller-id)
(def num-vescs)
(def wh-batt-left)
(def odometer)

(def is-data-send 0)

(defun uart-init () {
(uart-start 0 20 21 115200)
})

@const-start
(defun get-uart-values () {

        (define buffer-data-rcv (bufcreate 6))
        (define checksum-data-rcv (bufcreate 1))

        (bufset-i8 buffer-data-rcv 0 2) ; start byte
        (bufset-i8 buffer-data-rcv 1 1) ; payload length
        (bufset-i8 buffer-data-rcv 2 COMM_GET_VALUES) ; get the whole data packet from the ESC

        (bufset-i8 checksum-data-rcv 0 (bufget-i8 buffer-data-rcv 2))

        (setq crc_buffer_data (crc16 checksum-data-rcv))

        (bufset-i16 buffer-data-rcv 3 crc_buffer_data)
        (bufset-i8  buffer-data-rcv 5 3)

        (uart-write buffer-data-rcv)

        (setq is-data-send 1)
    }

)

(defun uart-send () {

        (bufset-i8  buffer 0 2) ; start byte
        (bufset-i8  buffer 1 5) ; payload length
        (bufset-i8  buffer 2 COMM_SET_CURRENT) ; command id, 0x06 for COMM_SET_CURRENT
        (bufset-i32 buffer 3 (*(*(* throttle_dead_band 10000) direction) 1)) ; Data current in mA

        (bufset-i8  checksum  0 (bufget-i8 buffer 2))
        (bufset-i32 checksum  1 (bufget-i32 buffer 3))

        (setq crc_buffer (crc16 checksum)) ; command id+payload data to calculate the checksum
        (bufset-i16 buffer 7 crc_buffer)
        (bufset-i8  buffer 9 3)
        (uart-write buffer)
    }
)


(defun load-buffer() {

        (setq temp-fet           (bufget-i16    uart-buf 3))  ; Temp FET
        (setq temp               (bufget-i16    uart-buf 5))  ; Motor TEMP
        (setq current            (bufget-i32    uart-buf 7))  ; total current motor
        (setq batt-current       (bufget-i32    uart-buf 11)) ; batt current
        (setq duty-cycle         (bufget-i16    uart-buf 15)) ; duty cycle in percents
        (setq erpm_l             (bufget-i32    uart-buf 17)) ; get the ERPM
        (setq speed-uart         (bufget-i32    uart-buf 21)) ; in m/s
        (setq voltage            (bufget-i16    uart-buf 25)) ; get the IN  voltage
        (setq batt-level         (bufget-i16    uart-buf 27)) ; batt-level
        (setq ah-tot             (bufget-i32    uart-buf 29)) ; ah-tot
        (setq ah-charge-tot      (bufget-i32    uart-buf 33)) ; ah-charge-tot
        (setq wh-tot             (bufget-i32    uart-buf 37)) ; wh-tot
        (setq wh-charge-tot      (bufget-i32    uart-buf 41)) ; wh-charge-tot
        (setq distance-uart      (bufget-i32    uart-buf 45)) ; distance
        (setq distance-uart-abs  (bufget-u32    uart-buf 49)) ; Absolute distance
        (setq pid-pos            (bufget-i32    uart-buf 53)) ; pid-pos
        (setq fault-code         (bufget-i8     uart-buf 57)) ; fault code
        (setq controller-id      (bufget-i8     uart-buf 58)) ; controller ID
        (setq num-vescs          (bufget-i8     uart-buf 59)) ; num vescs
        (setq wh-batt-left       (bufget-i32    uart-buf 60)) ; wh-batt left
        (setq odometer           (bufget-i32    uart-buf 64)) ; odometer
   }
  )

@const-end

(defun read-thd ()
    (loopwhile t {
        (get-uart-values)
        (if ( = is-data-send 1) {
            (uart-read uart-buf 75 0);(uart-read-bytes uart-buf 75 0) ; this function is blocked when data is not received
            (load-buffer)
            (setq is-data-send 0)
            (bufclear uart-buf);to avoid locked values when the UART connection is lost
        })
        (sleep 0.01)
}))

(spawn 150 read-thd) ; Run reader in its own thread

