;= Question =============================================================================
;
; 問題 5.14
; 
; 図5.11に示す階乗計算機を使い, 
; 小さいnのいくつかに対しn!を計算するのに必要な退避の回数とスタックの最大深さを計測せよ. 
; そのデータからn>1でn!を計算するのに使う退避演算の全数と, 
; スタックの最大深さのnに関する式を決定せよ. 
; 
; このそれぞれはnの線形関数で, 二つの定数で決ることに注意しよう. 
; 統計量を印字するのに, 階乗計算機にスタックを初期化し, 
; 統計量を印字する命令を追加しなければならない. 
; 
; また(図5.4のGCD計算機でしたように)
; 毎回get-register-contents, set-register-contents!, とstartを起動しないでも, 
; 繰り返しnの値を読み込み, 階乗を計算し, 
; 結果を印字するように計算機を修正したくなるであろう. 
; 
;= Answer ================================================================================

(load "./machine.scm")

(define (fact n)
  ;; recursive factorial machine from fig. 5.11  
  (define recur-fact-machine
    (make-machine
      '(continue n val)
      (list (list '* *)
            (list '- -)
            (list '= =)
            (list 'display display)
            (list 'read read))
      '((assign continue (label fact-done))
        ;; (perform (op display) (const "\nallocate n:"))
        ;; (assign n (op read))
        fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
        after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
        base-case
        (assign val (const 1))
        (goto (reg continue))
        fact-done
        (perform (op print-stack-statistics)))))
  (set-register-contents! recur-fact-machine 'n n)
  (start recur-fact-machine)
  (newline)
  (display (get-register-contents recur-fact-machine 'val))
  ((recur-fact-machine 'stack) 'initialize))

(define (do-fact n)
  (if (= n 0)
      'done
      (begin
        (fact n)
        (do-fact (- n 1)))))

(do-fact 10)
