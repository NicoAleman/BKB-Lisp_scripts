@const-start
(defun write_trip (dist km_mi px py){

       (def trip_box (img-buffer 'indexed2 64 14))

       (setq dist (* dist 0.00001)) ;; Not sure why dist is scaled down so much? This fixes scale.

       (if (= km_mi 0)
            (setq dist (* dist 0.6213)))
       (txt-block-l trip_box 1 0 0 font_9x14 
           (str-from-n dist 
               (if (= km_mi 0)
                   (if (> dist 999.9) "999.9mi" "%5.1fmi")
                   (if (> dist 999.9) "999.9km" "%5.1fkm"))))

       (disp-render trip_box px py '(0 0xFFFFFF))
})
@const-end