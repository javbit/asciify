(require :imago)

(defconstant CHARACTERS
  "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")

(defconstant Pr 0.2126)
(defconstant Pg 0.7152)
(defconstant Pb 0.0722)

(defun brightness->character (Y)
  "Convert Y in [0, 256) to a character that best captures it."
  (char CHARACTERS (floor (* Y (1- (length CHARACTERS))) 255)))

(defmacro with-rgb (color r g b &body body)
  `(let ((,r (imago:color-red ,color))
	 (,g (imago:color-green ,color))
	 (,b (imago:color-blue ,color)))
     ,@body))

(defun brightness (color)
  (with-rgb color red green blue
	    (floor (+ red green blue) 3)))

(defun lightness (color)
  (with-rgb color red green blue
	    (floor (+ (max red green blue)
		      (min red green blue))
		   2)))

(defun luminance (color)
  (with-rgb color red green blue
	    (floor (+ (* Pr red)
		      (* Pg green)
		      (* Pb blue)))))

(defun pixel-brightness (color method)
  (case method
	(:brightness (brightness color))
	(:lightness (lightness color))
	(:luminance (luminance color))
	(otherwise (imago:color-intensity color))))

(defun pixel->ascii (pixel method)
  "Convert PIXEL to ASCII character using CONVERSION method."
  (brightness->character (pixel-brightness pixel method)))

(defun escape ()
  (string #\Escape))

(defparameter *bg-color* 38)

(defun colorize (color)
  (let ((red (write-to-string (imago:color-red color)))
	(green (write-to-string (imago:color-green color)))
	(blue (write-to-string (imago:color-blue color)))
	(bg (write-to-string *bg-color*)))
    (concatenate 'string (escape) "[" bg ":2:" red ":" green ":" blue "m")))

(defun clear ()
  (concatenate 'string (escape) "[0m"))

(defun print-pixel (pixel method colorp)
  (let ((shape (string (pixel->ascii pixel method))))
    (write (if colorp
	       (concatenate 'string (colorize pixel) shape (clear))
	     shape)
	   :escape nil)))

(defun asciify (filename &optional method colorp)
  (let ((image (imago:resize (imago:read-image filename) 80 23)))
    (when image
      (imago:do-image-pixels (image pixel x y)
			     (print-pixel pixel method colorp)))))
