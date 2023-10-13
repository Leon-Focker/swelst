
;; * bassline.lsp

(ql:quickload 'layers)
(in-package :ly)

(defparameter *intervals* (make-hash-table))

(defun set-intervals (key ls)
  (setf (gethash key *intervals*) ls))

(set-intervals 0 '(0 12 -12))
(set-intervals 1 '(7 -5))
(set-intervals 2 '(-7 5))
(set-intervals 3 '(4 -4 9 -9))
(set-intervals 4 '(3 -3 8 -8))
(set-intervals 5 '(1 -1 2 -2))
(set-intervals 6 '(10 -10 11 -11))
(set-intervals 7 '(6 -6))

;; length (of sequence)
;; first-note
;; abmitus (list of lowest, highest note)
;; dissonance-env (from 0 to 1)
;; variation-env (how many intervals are possible) (from 0 to 1)
(defun make-bassline (length first-note ambitus dissonance-env variation-env &optional (offset 0))
  (unless (integerp first-note)
    (error "first-note is not an integer but: ~a" first-note))
  (loop for i from 0 below length
     for progress = (/ i length)
     with next = first-note
     with last = first-note
     collect
       (let* ((dissonance (interpolate progress dissonance-env))
	      (variation (round (interpolate progress variation-env)))
	      (dissonance-keys (loop for i from (max 0 (round (- dissonance (/ variation 2))))
				  to (min 7 (round (+ dissonance (/ variation 2))))
				  collect i))
	      (intervals (loop for i in dissonance-keys append (gethash i *intervals*)))
	      (interval-cnt (length intervals)))
	 (prog1 next
	   (labels ((helper (cnt)
		      (setf next (+ last (nth (mod cnt interval-cnt) intervals)))
		      (unless (or (<= (first ambitus) next (second ambitus))
				  (= (- cnt (length intervals) offset) i))
			(helper (1+ cnt)))))
	     (helper (+ i offset)))
	   (setf last next)))))

(let* ((len 30)
       (dissonance-env '(0 1  .3 4  .5 2  .9 6  1 0))
       (variation-env '(0 0  .7 3  1 1)))
  (lists-to-midi 
   (append
    (make-bassline len 40 '(28 52) dissonance-env variation-env)
    (make-bassline len 47 '(35 59) dissonance-env variation-env len)
    (make-bassline len 44 '(32 56) dissonance-env variation-env (* 2 len)))
   '(1) (loop for i from 0 below len collect i) :file "/E/code/organ/lisp/line1.mid"))

(let* ((len 300)
       (dissonance-env '(0 1  .3 4  .5 2  .6 6  .7 1  1 0))
       (variation-env '(0 0  .7 3  1 1)))
  (lists-to-midi 
   (append
    (make-bassline len 40 '(28 52) dissonance-env variation-env)
    (make-bassline len 47 '(35 59) dissonance-env variation-env len)
    (make-bassline len 51 '(40 65) dissonance-env variation-env (* 2 len)))
   '(.1) (loop for i from 0 below len collect i) :file "/E/code/organ/lisp/line_2.mid"))

;; This sounds nice:
(let* ((len 300)
       (dissonance-env '(0 0  .5 7  1 0))
       (variation-env '(0 0  1 7)))
  (lists-to-midi 
   (append
    (make-bassline len 40 '(28 52) dissonance-env variation-env)
    (make-bassline len 47 '(35 59) dissonance-env variation-env len)
    (make-bassline len 51 '(40 65) dissonance-env variation-env (* 2 len)))
   '(.1)
   (loop for i from 0 below len collect (/ i 10))
   :file "/E/code/organ/lisp/line_3_nice.mid"))

;; append all basslines from length 1 to max-len:
(let* ((max-len 20)
       (voices 4)
       (dissonance-env '(0 1  .3 4  .5 2  .6 6  .7 1  1 0))
       (variation-env '(0 0  .7 3  1 1))
       (pitches '())
       (durations '(1 .5))
       (start-times '()))
   (setf pitches
	 (flatten
	  (loop for n from 1 to max-len collect
	       (append
		(make-bassline n 40 '(28 52) dissonance-env variation-env)
		(make-bassline n 47 '(35 59) dissonance-env variation-env n)
		(make-bassline n 44 '(32 56) dissonance-env variation-env (* 2 n))
		(make-bassline n 59 '(40 70) dissonance-env variation-env (* 3 n))))))
   (setf start-times
	 (flatten
	  (loop for n from 1 to max-len with start = 0
	     collect (loop repeat voices
			with ls = (loop for i from start below (+ start n) collect i)
			append ls)
	     do (incf start n))))
   (lists-to-midi pitches durations start-times 
   :file "/E/code/organ/lisp/lines_1.mid"))


;; permute the basslines:
(let* ((max-len 11)
       (voices 4)
       (dissonance-env '(0 1  .3 4  .5 2  .6 6  .7 1  1 0))
       (variation-env '(0 0  .7 3  1 1))
       (pitches '())
       (durations '(1))
       (start-times '())
       (permutation (remove-duplicates (procession 35 max-len))))
  (setf pitches
	(loop for n from 1 to max-len collect
	     (append
	      (make-bassline n 40 '(28 52) dissonance-env variation-env)
	      (make-bassline n 40 '(35 59) dissonance-env variation-env n)
	      (make-bassline n 40 '(32 56) dissonance-env variation-env (* 2 n))
	      (make-bassline n 40 '(40 70) dissonance-env variation-env (* 3 n)))))
  (setf start-times
	(loop for n from 1 to max-len with start = 0
	   collect (loop repeat voices
		      with ls = (loop for i from start below (+ start n) collect i)
		      append ls)
	   do (incf start n)))
  (setf pitches (flatten (re-order pitches permutation)))
  (setf start-times (flatten (re-order start-times permutation)))
  (lists-to-midi pitches durations start-times 
		 :file "/E/code/organ/lisp/lines_2.mid"))

;; make it a function!
(defun gen-bassline-permutations
    (first-notes ambitus-list
     max-len voices dissonance-env variation-env permutation file)
  (let* ((pitches '())
	 (durations '(1))
	 (start-times '()))
    ;; we do this to use the perks of re-order!
    (setf permutation (re-order (loop for i from 0 below max-len collect i)
				permutation))
    (setf pitches
	  (flatten 
	   (loop for n in permutation do (incf n) collect
		(loop for i from 0 below voices append
		     (make-bassline n (nth i first-notes) (nth i ambitus-list)
				    dissonance-env variation-env (* n i))))))
    (setf start-times
	  (flatten
	   (loop for n in permutation with start = 0
	      collect (loop repeat voices
			 with ls = (loop for i from start to (+ start n) collect i)
			 append ls)
	      do (incf start (1+ n)))))
    (lists-to-midi pitches durations start-times
		   :file file)))

;; some progressions:
(gen-bassline-permutations '(40 47 44 59) '((28 52) (35 59) (32 56) (40 70)) 11 4
			   '(0 1  .3 4  .5 2  .6 6  .7 1  1 0)
			   '(0 0  .7 3  1 1)
			   (remove-duplicates (procession 35 11))
			   "/E/code/organ/lisp/bassline_permutation_1.mid")

(gen-bassline-permutations '(47  48 49 50) '((28 52) (35 59) (32 56) (40 70)) 11 4
			   '(0 1  .3 4  .5 2  .6 6  .7 1  1 0)
			   '(0 0  .7 3  1 1)
			   (remove-duplicates (procession 35 11))
			   "/E/code/organ/lisp/bassline_permutation_2.mid")


;; ** three chords:
;; good chord2:
(let* ((max-len 11)
       (dissonance-env '(0 1  .3 4  .5 2  .6 6  .7 1  1 0))
       (variation-env '(0 0  .7 3  1 1))
       (permutation (remove-duplicates (procession 35 11))))
  
  (gen-bassline-permutations '(74 75 76 78) '((62 76) (50 64) (38 52) (26 40)) max-len 4
			     dissonance-env variation-env permutation
			     "/E/code/organ/lisp/bassline_permutation_chord_1.mid")
  (gen-bassline-permutations '(65 68 70 71 72) '((26 96) (26 96) (26 96) (26 96) (26 96)) max-len 5
			     dissonance-env variation-env permutation
			     "/E/code/organ/lisp/bassline_permutation_chord_2.mid")
  (gen-bassline-permutations '(67 69 61) '((50 90) (50 90) (50 90)) max-len 3
			     dissonance-env variation-env permutation
			     "/E/code/organ/lisp/bassline_permutation_chord_3.mid"))

;; nice :3
(let* ((max-len 11)
       (dissonance-env '(0 1       .3 2  .45 2  .5 6  .8 1  1 4))
       (variation-env '(0 1  .2 0  .3 2  .4 1       .6 1    1 5))
       (permutation '(0 1 2 3 4 5 6 7 8 9 10 11)))
  (gen-bassline-permutations '(74 75 76 78) '((62 76) (50 64) (38 52) (26 30)) max-len 4
			     dissonance-env variation-env permutation
			     "/E/code/organ/lisp/bassline_permutation_chord_11.mid")
  (gen-bassline-permutations '(65 68 70 71 72) '((26 96) (26 96) (26 96) (26 96) (26 96)) max-len 5
			     dissonance-env variation-env permutation
			     "/E/code/organ/lisp/bassline_permutation_chord_22.mid")
  (gen-bassline-permutations '(67 69 61) '((50 90) (50 90) (50 90)) max-len 3
			     dissonance-env variation-env permutation
			     "/E/code/organ/lisp/bassline_permutation_chord_33.mid"))
