(in-package :ffi-utils)

(defun foreign-buffer-to-vector (vector foreign-buffer foreign-buffer-size)
  (assert (equal (array-element-type vector) '(unsigned-byte 8)))
  (let ((num-elements-to-copy (min (length vector) foreign-buffer-size)))
    (loop for i from 0 to (- num-elements-to-copy 1)
          do (progn
               (setf (aref vector i)
                     (mem-aref foreign-buffer :unsigned-char i))))
    num-elements-to-copy))

(defmacro iterate-over-foreign-buffer
    ((vec vec-size vec-pos) (buf buf-size buf-pos) &body body)
  `(loop with ,vec = (make-array ,vec-size :element-type '(unsigned-byte 8))
      for ,buf-pos = 0 then (+ ,buf-pos ,vec-pos)
      until (= ,buf-pos ,buf-size)
      for ,vec-pos =
        (foreign-buffer-to-vector ,vec
                                  (inc-pointer ,buf ,buf-pos)
                                  (- ,buf-size ,buf-pos))
      do ,@body))
