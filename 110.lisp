(defun rule110 (universe)
    (setq mynewset ())
    (loop for x in universe do
        (setq plusone ())
        (setq minusone ())
        (setq plusone (adjoin (+ x 1) plusone))
        (setq minusone (adjoin (- x 1) minusone))

        (if (subsetp plusone universe) nil (setq mynewset (adjoin x mynewset)))
        (if (subsetp minusone universe) nil (setq mynewset (adjoin x mynewset)))
        (if (subsetp minusone universe) nil (setq mynewset (adjoin (- x 1) mynewset)))
    )
    mynewset
)

(defun show (universe generations)
    (loop for x below generations do
        (setq currX ())
        (setq currX (adjoin x currX))
        (prin1 (if (subsetp currX universe) 1 0 ))
    )
    (terpri)
)


(setq maingenerations 20)
(setq mainuniverse ())
(setq mainuniverse (adjoin (- maingenerations 1) mainuniverse))

(loop for x below maingenerations do
    (show mainuniverse maingenerations)
    (setq mainuniverse (rule110 mainuniverse))
)

