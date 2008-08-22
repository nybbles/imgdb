(in-package :imgdb-web)

(defun generate-tag-cloud (tags)
  (let* ((quantities (mapcar #'(lambda (x) (get-quantity x)) tags))
         (base (get-covering-base quantities))
         (mid-exponent
          (get-middle-exponent quantities base)))
    (with-html-output-to-string (output nil)
      (loop for tag in tags
         do (let ((tag-name (first tag))
                  (link (second tag))
                  (quantity (third tag))
                  (active (fourth tag)))
              (htm
               (:span :class
                      (get-cloud-tag-class active quantity mid-exponent base)
                      (:a :href link
                          :title 
                          (let ((num-str (if (= quantity 0) "No" quantity))
                                (imgs-str (if (= quantity 1) "image" "images")))
                            (format nil "~A ~A" num-str imgs-str))
                          (str tag-name))) " "))))))

(defun get-tags
    (tag constraints
     &key tag-value-fn tag-quantity-fn get-tags-fn)
  (let ((pruned-constraints (remove-constraint constraints :name tag)))
    (mapcar
     #'(lambda (x)
         (let* ((tag-value (funcall tag-value-fn x))
                (tag-quantity (funcall tag-quantity-fn x))
                (display-name (tag-value-to-display-name tag tag-value))
                (tag-is-active (find-constraint constraints
                                                :name tag :value tag-value))
                (tag-link
                 (get-query-link-for-constraints
                  (if tag-is-active
                      pruned-constraints
                      (cons (make-constraint tag tag-value)
                            pruned-constraints)))))
           (list display-name tag-link tag-quantity tag-is-active)))
     (funcall get-tags-fn))))

(defun get-cloud-tag-class (active quantity mid-exponent &optional (base 10))
  (concatenate 'string
               "M"
               (write-to-string
                (- (floor (if (= quantity 1)
                              0
                              (log quantity base)))
                   mid-exponent))
               (when active
                 "-active")))

(defun get-middle-exponent (quantities &optional (base 10))
  (let ((expts (mapcar #'(lambda (x)
                           (floor (if (= x 1) 0 (log x base))))
                       quantities)))
    (round (average expts))))

(defun average (numbers)
  (if (null numbers)
      0
      (/ (reduce #'+ numbers) (length numbers))))

(defun get-covering-base (numbers &optional (coverage 5))
  (assert (oddp coverage))
  (expt (average numbers) (/ 1 (/ (- coverage 1) 2))))

(defun get-quantity (tag)
  (third tag))
