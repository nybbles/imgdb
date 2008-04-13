(in-package :imgdb-web)

(defun generate-tag-cloud (tags)
  (let ((mid-exponent
         (get-middle-exponent (mapcar #'(lambda (x) (third x)) tags))))
    (with-html-output-to-string (output nil)
      (loop for tag in tags
         do (let ((tag-name (first tag))
                  (link (second tag))
                  (quantity (third tag)))
              (htm
               (:span :class (get-cloud-tag-class quantity mid-exponent)
                      (:a :href link
                          (str tag-name))) " "))))))

(defun get-cloud-tag-class (quantity mid-exponent)
  (concatenate 'string
               "M"
               (write-to-string (- (floor (log quantity 10)) mid-exponent))))

(defun get-middle-exponent (quantities &optional (base 10))
  (let ((expts (mapcar #'(lambda (x) (floor (log x base))) quantities)))
    (round (average expts))))

(defun average (numbers)
  (/ (reduce #'+ numbers) (length numbers)))
