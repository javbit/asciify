(require :imago)

(defconstant CHARACTERS
  "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")

(defun brightness-to-character (Y)
  "Convert Y in [0, 256) to a character that best captures it."
  (char CHARACTERS (floor (* Y (length CHARACTERS)) 255)))

(defun color-brightness (color)
  (let ((red (imago:color-red color))
	(green (imago:color-green color))
	(blue (imago:color-blue color)))
    (floor (+ red green blue) 3)))

(defun asciify (filename)
  (let ((image (imago:resize (imago:read-image filename) 80 23)))
    (when image
      (format t "Successfully loaded ~s!~%" filename)
      (format t "Image size: ~sx~s~%"
	      (imago:image-width image)
	      (imago:image-height image))
      (imago:do-image-pixels
       (image color x y)
       (format t "~a"
	       (brightness-to-character (color-brightness color)))))))
