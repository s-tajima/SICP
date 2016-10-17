;= Question =============================================================================
;
; 問題 5.8
; 
; 次のレジスタ計算機の命令はラベルhereが複数回定義してあるので, 曖昧である:
; 
; start
;   (goto (label here))
; here
;   (assign a (const 3))
;   (goto (label there))
; here
;   (assign a (const 4))
;   (goto (label there))
; there
; 
; これまでに書いたシミュレータでは, 制御がthereに達した時レジスタa の内容はどうなるか. 
; extract-labels手続きを修正し, 
; 同じラベル名が二つの異る場所を指すように使われたら, エラーとするようにせよ. 

; 
;= Answer ================================================================================

(load "./machine.scm")

(define (make-label-entry label-name insts)
  (print "label-name: " label-name)
  (print "insts:" insts)
  (cons label-name insts))

(define test-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
      here
       (assign a (const 3))
       (goto (label there))
      here
       (assign a (const 4))
       (goto (label there))
       there)))


(print (set-register-contents! test-machine 'a 0))
(print (start test-machine))
(print (get-register-contents test-machine 'a))

