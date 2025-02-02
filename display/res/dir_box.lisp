@const-start
(defun write_direction (dir px py){

    (def dir_box (img-buffer 'indexed4 24 16))
    (if (= dir 1)
        (progn
        (img-rectangle dir_box 0 0 23 15 2 '(filled) '(rounded 2) )
        (txt-block-c dir_box 0 12 8 font_11x14_b "1")
        )
        (progn
        (img-rectangle dir_box 0 0 23 15 1 '(filled) '(rounded 2))
        (txt-block-c dir_box 0 13 8 font_11x14_b "2")
        )
    )
    (disp-render dir_box px py '(0 0xFF0000 0x00FF00 0x0000FF))

})
@const-end