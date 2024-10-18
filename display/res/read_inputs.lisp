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

(defun read_on(){

    (if(= (gpio-read on_button) 1) {
        (setq secs (secs-since last_update))
        (if(and (>= secs button_time_short ) (<= secs (+ button_time_short 0.1))) {
            (setq on_pressed_short 1)  
        })
        (if(and (>= secs button_time_long ) (< secs (+ button_time_long 0.1))) {
            (setq on_pressed_long 1)  
        })
    }
    {;else
        (setq last_update (systime))
        (setq on_pressed_short 0)
        (setq on_pressed_long 0)
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
@const-start
(defun read_cfg(){
    
    (if(= (read_analog_button) 2) {
        (setq secs_cfg (secs-since last_update_cfg))
        (if(and (>= secs_cfg button_time_short ) (<= secs_cfg (+ button_time_short 0.1))) {
            (setq cfg_pressed_short 1)  
        })
        (if(and (>= secs_cfg button_time_long ) (< secs_cfg (+ button_time_long 0.1))) {
            (setq cfg_pressed_long 1)  
        })
    }
    {;else
        (setq last_update_cfg (systime))
        (setq cfg_pressed_short 0)
        (setq cfg_pressed_long 0)
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

(defun read_SOC(){
    (/ (get-adc 3) 0.4) ; TODO implement low pass filter
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