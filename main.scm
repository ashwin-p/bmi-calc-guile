;; guile

(use-modules (ice-9 readline))
(activate-readline)

(define get-weight-unit
  (lambda ()
    (display "Enter your preferred unit for weight (kg/lbs): ")
    (let ((x (format #f "~a" (read))))
      (cond ((string=? (string-downcase x) "kg") "kg")
            ((string=? (string-downcase x) "lbs") "lbs")
            (else (get-weight-unit))))))
            
(define get-weight
  (lambda (unit)
    (display "Enter your weight: ")
    (let ((weight (read)))
      (cond ((number? weight)
             (cond ((string=? unit "kg") weight)
                   (else (* weight 0.45))))
            (else (get-weight unit))))))

(define get-height-unit
  (lambda ()
    (display "Enter your prefered unit for height (cm/ft in): ")
    (let ((x (format #f "~a" (readline))))
      (cond ((string=? (string-downcase x) "cm") "cm")
	    ((string=? (string-downcase x) "ft in") "ft in")
	    (else (get-height-unit))))))

(define get-height
  (lambda (unit)
    (display "Enter your height: ")
    (cond ((string=? unit "cm")
	   (let ((h (read)))
	     (cond ((number? h) (/ h 100))
		   (else (get-height unit)))))
	  (else (let ((ft (read)))
		  (cond ((number? ft)
			 (let ((in (read)))
			   (cond ((number? in) (+ (* ft 0.3048) (* in 0.0254)))
				 (else (get-height unit)))))
			(else (get-height unit))))))))

(define calc-bmi
  (lambda (weight height)
    (/ weight (* height height))))

(define get-status
  (lambda (bmi)
    (cond ((< bmi 18.5) "underweight.")
          ((< bmi 25) "normal.")
          ((< bmi 30) "overweight.")
          ((< bmi 35) "obese.")
          (else "extremely obese."))))

(let ((weight-unit (get-weight-unit))
      (height-unit (get-height-unit)))
  (let ((weight (get-weight weight-unit))
	(height (get-height height-unit)))
    (let ((bmi (calc-bmi weight height)))
      (format #t "Your BMI is ~,2f.\n" bmi)
      (format #t "You are ~a\n" (get-status bmi)))))
