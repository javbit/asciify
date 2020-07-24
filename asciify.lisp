(require :imago)

(defun asciify (filename)
  (let ((image (imago:read-image filename)))
    (when image
      (format t "Successfully loaded ~s!~%" filename)
      (format t "Image size: ~sx~s~%"
	      (imago:image-width image)
	      (imago:image-height image))
      (imago:do-image-pixels
       (image color x y)
       (format t "~s ~s ~s~%"
	       (imago:color-red color)
	       (imago:color-green color)
	       (imago:color-blue color))))))
