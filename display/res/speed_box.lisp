; Write speed with unit 20x30 font
; speed  -> speed in mph or kph in float
; m_k    -> 0 = miles per houtr, 1 = kph
; px py  -> position in pixels
; color  -> 0 = black, 1 = white, 2 = green, 3 = red
(def speed_val 0.0)
@const-start
(defun speed_cal(){
    (if (and (not-eq poles_config 0) (not-eq pulley_config 0))
        (setq speed_val (*(* (/ (/ (abs rpm) (/ poles_config 2)) pulley_config) wheel_diam_config 0.18845)))
        (setq speed_val 0.0)
    )

})

(defun write-speed (speed m_k px py color){

    (def speed_box (img-buffer 'indexed4 60 30))
    (setq speed (to-i (* 10 speed)))
    (txt-block-l speed_box color 0 0 font_20x30 (str-from-n speed "%03d"))
    (txt-block-c speed_box color 40 0 font_20x30 ".")
    (disp-render speed_box px py '(0x00000 0xFFFFFF 0x00FF00 0xFF0000))
    (def unit_box (img-buffer 'indexed2 27 17))
    (if (= m_k 0)
            (txt-block-l unit_box 1 0 0 font_9x14 "mph")
            (txt-block-l unit_box 1 0 0 font_9x14 "kph")
    )
    (disp-render unit_box (+ px 63) (+ py 15) '(0x00000 0xFFFFFF))

})
@const-end