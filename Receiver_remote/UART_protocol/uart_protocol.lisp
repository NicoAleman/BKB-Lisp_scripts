; Implement UART protocol to send current commands and get some datas from the ESC.

; Data structure
(def COMM_SET_CURRENT_ID  84); 84 is for current relative
(def COMM_SET_BRAKE_CURRENT 7)
(def COMM_GET_VALUES       4)
(def BUF-SIZE 84)


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
(def throttle_range)
(def tacho_abs)

(def is-data-send 0)

(defun uart-init () {
(uart-start 0 20 21 115200)
})

@const-start
(defun get-uart-values () { ; COMM_ID is for 4 (COMM_GET_VALUES) or 47(COMM_GET_SETUP_VALUES)

        (define buffer-data-rcv (bufcreate 6))
        (define checksum-data-rcv (bufcreate 1))

        (bufset-i8 buffer-data-rcv 0 2) ; start byte
        (bufset-i8 buffer-data-rcv 1 1) ; payload length
        (bufset-i8 buffer-data-rcv 2 COMM_GET_VALUES);(bufset-i8 buffer-data-rcv 2 COMM_GET_VALUES) ; get the whole data packet from the ESC

        (bufset-i8 checksum-data-rcv 0 (bufget-i8 buffer-data-rcv 2))

        (setq crc_buffer_data (crc16 checksum-data-rcv))

        (bufset-i16 buffer-data-rcv 3 crc_buffer_data)
        (bufset-i8  buffer-data-rcv 5 3)

        (uart-write buffer-data-rcv)

        (setq is-data-send 1)
    }

)

; for throttle_range we need to know about the current scale
(defun uart-send () {
        (setq throttle_range (* throttle_dead_band throttle_scale))
        (bufset-i8  buffer 0 2) ; start byte
        (bufset-i8  buffer 1 5) ; payload length
        (bufset-i8  buffer 2 COMM_SET_CURRENT_ID) ; COMM_SET_CURRENT_ID 84, is for relative
        (bufset-i32 buffer 3 (*(* throttle_range current_val) direction)) ; Data current relative
        (bufset-i8  checksum  0 (bufget-i8 buffer 2))
        (bufset-i32 checksum  1 (bufget-i32 buffer 3))

        (setq crc_buffer (crc16 checksum)) ; command id+payload data to calculate the checksum
        (bufset-i16 buffer 7 crc_buffer)
        (bufset-i8  buffer 9 3)
        (uart-write buffer)
    }
)
; This function is to get the datas coming from COMM_GET_VALUES_SETUP
(defun load-buffer() {
         (setq current       (bufget-i32   uart-buf 7))  ; total current motor (current in)
         (setq voltage       (bufget-i16   uart-buf 29)) ; get the IN  voltage
         (setq erpm_l        (bufget-i32   uart-buf 25)) ; get the ERPM
         (setq distance-uart (bufget-i32   uart-buf 51))
   }
  )

@const-end

(defun read-thd ()
    (loopwhile t {
        (get-uart-values)
        (if ( = is-data-send 1) {
            (uart-read uart-buf 84 0);(uart-read-bytes uart-buf 75 0) ; this function is blocked when data is not received
            (load-buffer)
            (setq is-data-send 0)
        })
        (bufclear uart-buf);to avoid locked values when the UART connection is lost
        (sleep 0.05)
}))

(spawn 150 read-thd) ; Run reader in its own thread



