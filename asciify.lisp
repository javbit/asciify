(require :imago)

(defun color-brightness (color)
  (let ((red (imago:color-red color))
	(green (imago:color-green color))
	(blue (imago:color-blue color)))
    (floor (+ red green blue) 3)))

(defun asciify (filename)
  (let ((image (imago:read-image filename)))
    (when image
      (format t "Successfully loaded ~s!~%" filename)
      (format t "Image size: ~sx~s~%"
	      (imago:image-width image)
	      (imago:image-height image))
      (imago:do-image-pixels
       (image color x y)
       (format t "~s~%" (color-brightness color))))))
