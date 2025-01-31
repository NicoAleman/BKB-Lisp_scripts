@const-start
(defun write_amps (amp px py){
       (def amp_box (img-buffer 'indexed2 55 14))
       (txt-block-l amp_box 1 0 0 font_9x14 (str-from-n amp "%3.0fA")) ;; Could set to %1.0fA or %2.0fA if we want to be more left aligned more often
       (disp-render amp_box px py '(0 0xFFFFFF))
})
@const-end