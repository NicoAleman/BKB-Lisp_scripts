(define on_button 10)
(define charger_pin 4)

(def on_pressed_short 0)
(def on_pressed_long 0)
(def cfg_pressed_short 0)
(def cfg_pressed_long 0)
(def thum_pressed_short 0)
(def thum_pressed_long 0)

(def secs 0.0)
(def last_update 0.0)
(define button_time_short 0.15) 
(define button_time_long 2.0) 
@const-start
(defun get-adc-raw() {
   (* (get-adc 0) 1422 )
})

(def button_state_cfg 0)  ; 0 = not pressed, 1 = pressed
(def button_state_on 0)   ; 0 = not pressed, 1 = pressed
(def press_start_time_cfg 0.0)
(def press_start_time_on 0.0)
(def last_short_on 0)       ; Track previous short press state
(def last_short_cfg 0)      ; Track previous short press state

(defun read_on(){
    (def current_state (eq (gpio-read on_button) 1))
    
    ; Auto-clear flags after one tick
    (if (and (= on_pressed_short 1) (= last_short_on 1)) {
        (setq on_pressed_short 0)
    })
    (setq last_short_on on_pressed_short)
    
    ; Button just pressed
    (if (and current_state (eq button_state_on 0)) {
        (setq press_start_time_on (systime))
        (setq button_state_on 1)
    })
    
    ; Check for long press while button is held
    (if (and current_state (eq button_state_on 1)) {
        (def press_duration (/ (- (systime) press_start_time_on) 1000.0))
        (if (>= press_duration button_time_long) {
            (setq on_pressed_long 1)
        })
    })
    
    ; Button just released
    (if (and (not current_state) (eq button_state_on 1)) {
        (def press_duration (/ (- (systime) press_start_time_on) 1000.0))
        (if (< press_duration button_time_long) {
            (setq on_pressed_short 1)  ; Only trigger short if no long press happened
        })
        (setq button_state_on 0)
    })
})
@const-end

(def analog_button 0.0)
(def return_analog 0)

@const-start
(defun read_analog_button(){
    (setq analog_button (get-adc 2))
    (if(> analog_button 1.85)
        (setq return_analog 0); no button is pressed
        (if(and (> analog_button 1.40) (< analog_button 1.85))
            (setq return_analog 1);thumb button is pressed
            (if(and (> analog_button 0.9) (< analog_button 1.4))
                (setq return_analog 2);cfg button is pressed
                (setq return_analog 3); both pressed
            ) 
        )
    )
})
@const-end

(def last_update_cfg 0)
(def secs_cfg 0)
(def button_state 0)  ; 0 = not pressed, 1 = pressed
(def press_start_time 0.0)

@const-start
(defun read_cfg(){
    (def current_state (eq (read_analog_button) 2))
    
    ; Auto-clear flags after one tick
    (if (and (= cfg_pressed_short 1) (= last_short_cfg 1)) {
        (setq cfg_pressed_short 0)
    })
    (setq last_short_cfg cfg_pressed_short)
    
    ; Button just pressed
    (if (and current_state (eq button_state_cfg 0)) {
        (setq press_start_time_cfg (systime))
        (setq button_state_cfg 1)
    })
    
    ; Check for long press while button is held
    (if (and current_state (eq button_state_cfg 1)) {
        (def press_duration (/ (- (systime) press_start_time_cfg) 1000.0))
        (if (>= press_duration button_time_long) {
            (setq cfg_pressed_long 1)
        })
    })
    
    ; Button just released
    (if (and (not current_state) (eq button_state_cfg 1)) {
        (def press_duration (/ (- (systime) press_start_time_cfg) 1000.0))
        (if (< press_duration button_time_long) {
            (setq cfg_pressed_short 1)  ; Only trigger short if no long press happened
        })
        (setq button_state_cfg 0)
    })
})
@const-end
(def last_update_thum 0)
(def secs_thum 0)
@const-start
(defun read_thum(){

    (if(= (read_analog_button) 1) {
        (setq secs_thum (secs-since last_update_thum))
        (if(and (>= secs_thum button_time_short ) (<= secs_thum (+ button_time_short 0.1))) {
            (setq thum_pressed_short 1)  
        })
        (if(and (>= secs_thum button_time_long ) (< secs_thum (+ button_time_long 0.1))) {
            (setq thum_pressed_long 1)  
        })
    }
    {;else
        (setq last_update_thum (systime))
        (setq thum_pressed_short 0)
        (setq thum_pressed_long 0)
    })           
})

(def last_soc_value 0.0)
(def first_soc_read 1)  ; Flag for first reading

(defun read_SOC(){
    (def new_soc (/ (get-adc 3) 0.4))
    
    ; On first read, initialize last_soc_value
    (if (= first_soc_read 1)
        (progn 
            (setq last_soc_value new_soc)
            (setq first_soc_read 0)
        )
    )
    
    ; Apply EMA filter: 95% old + 5% new
    (setq last_soc_value (+ (* 0.95 last_soc_value) 
                           (* 0.05 new_soc)))
    
    last_soc_value
})
@const-end
(def charging)

@const-start
(defun isCharging(){
   (if(= (gpio-read 4) 1)
        (setq charging 0)
        (setq charging 1)
    )
})
@const-end