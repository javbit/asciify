(require :imago)

(defun asciify (filename)
  (let ((image (imago:read-image filename)))
    (when image
      (format t "Successfully loaded ~s!~%" filename)
      (format t "Image size: ~sx~s~%"
	      (imago:image-width image)
	      (imago:image-height image)))))
