; Implement UART protocol to send current commands and get some datas from the ESC.

; Data structure
(def COMM_SET_CURRENT_ID    84); 84 is for current relative
(def COMM_SET_BRAKE_CURRENT 7)
(def COMM_GET_VALUES        4)
(def COMM_SET_CHUCK_DATA    35) ; VESC UART Remote Data
(def BUF-SIZE 84)

; Set buffer sizes to accommodate SET_CHUCK_DATA packet
(define buffer         (bufcreate 20))
(define buffer_motor   (bufcreate 10))
(define checksum       (bufcreate 18))
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

(defun uart-send () {
    (bufset-i8  buffer 0 COMM_SET_CHUCK_DATA) ; Chuck data command
    (bufset-i8  buffer 1 128) ; js_x (neutral)
    (bufset-i8  buffer 2 throttle_uart) ; js_y (0-255)
    (bufset-i8  buffer 3 (if (or (= button_state 1) (= button_state 3)) 1 0)) ; bt_c (button C - Thumb Click)
    (bufset-i8  buffer 4 (if (or (= button_state 2) (= button_state 3)) 1 0)) ; bt_z (button Z - Cfg Button)

    ; Acceleration data (6 bytes of zeros)
    (bufset-i8  buffer 5 0) ; acc_x low byte
    (bufset-i8  buffer 6 0) ; acc_x high byte
    (bufset-i8  buffer 7 0) ; acc_y low byte
    (bufset-i8  buffer 8 0) ; acc_y high byte
    (bufset-i8  buffer 9 0) ; acc_z low byte
    (bufset-i8  buffer 10 0) ; acc_z high byte

    (bufset-i8  buffer 11 1) ; rev_has_state
    (bufset-i8  buffer 12 (if (= direction 1) 0 1)) ; is_rev
    
    ; Calculate CRC on just the payload
    (setq crc_buffer (crc16 buffer 13))
    
    ; Now build the complete packet
    (bufset-i8  checksum 0 2) ; start byte
    (bufset-i8  checksum 1 13) ; payload length (13 bytes total)
    ; Copy payload
    (bufset-i8 checksum 2 (bufget-i8 buffer 0))
    (bufset-i8 checksum 3 (bufget-i8 buffer 1))
    (bufset-i8 checksum 4 (bufget-i8 buffer 2))
    (bufset-i8 checksum 5 (bufget-i8 buffer 3))
    (bufset-i8 checksum 6 (bufget-i8 buffer 4))
    (bufset-i8 checksum 7 (bufget-i8 buffer 5))
    (bufset-i8 checksum 8 (bufget-i8 buffer 6))
    (bufset-i8 checksum 9 (bufget-i8 buffer 7))
    (bufset-i8 checksum 10 (bufget-i8 buffer 8))
    (bufset-i8 checksum 11 (bufget-i8 buffer 9))
    (bufset-i8 checksum 12 (bufget-i8 buffer 10))
    (bufset-i8 checksum 13 (bufget-i8 buffer 11))
    (bufset-i8 checksum 14 (bufget-i8 buffer 12))
    ; Add CRC and stop byte
    (bufset-i16 checksum 15 crc_buffer)
    (bufset-i8  checksum 17 3) ; stop byte

    (uart-write checksum)
})

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



