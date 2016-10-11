;= Question =============================================================================
;
; 問題 5.7
; 
; このシミュレータを使い, 問題5.4で設計した計算機をテストせよ. 
; 
;= Answer ================================================================================

(load "./machine.scm")

(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

(print (set-register-contents! gcd-machine 'a 206))
(print (set-register-contents! gcd-machine 'b 40))
(print (start gcd-machine))
(print (get-register-contents gcd-machine 'a))

;; exercise 5.7 (section5.1)
;; a
(define expt-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '((assign continue (label expt-done))
      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label return))
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
      after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
      return
      (assign val (const 1))
      (goto (reg continue))
      expt-done)))

(print (set-register-contents! expt-machine 'b 3))
(print (set-register-contents! expt-machine 'n 4))
(print (start expt-machine))
(print (get-register-contents expt-machine 'val))
;=> 81

;; b
(define expt-machine
  (make-machine
    '(counter product n b)
    (list (list '= =) (list '* *) (list '- -))
    '((assign counter (reg n))
      (assign product (const 1))
      expt-iter
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign product (op *) (reg b) (reg product))
      (assign counter (op -) (reg counter) (const 1))
      (goto (label expt-iter))
      expt-done)))

(print (set-register-contents! expt-machine 'b 3))
(print (set-register-contents! expt-machine 'n 4))
(print (start expt-machine))
(print (get-register-contents expt-machine 'product))
;=> 81
