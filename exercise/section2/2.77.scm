;= Question =============================================================================
;
; Louis Reasonerはzを図2.24に示すオブジェクトとして,
; 式(magnitude z)を評価しようとした. 驚いたことに答の5の代りに,
; 型(complex)には演算magnitudeは対応する手続きはないという
; apply-genericのエラーメッセージを得た. 
; 
; この対話をAlyssa P. Hackerに見せると, 
; 彼女は「complex数に対して複素数の選択子は定義されていず, 
; polarとrectangularだけ定義されているのが問題である. 
; 
; これが働くようにするには, 
; complexパッケージに次のものを追加しなければならない」という:
; 
; (put 'real-part '(complex) real-part)
; (put 'imag-part '(complex) imag-part)
; (put 'magnitude '(complex) magnitude)
; (put 'angle '(complex) angle)
; 
; どうしてこれが働くか詳しく述べよ. 
; 例えばzが図2.24に示すオブジェクトとして, 式(magnitude z)を評価する時, 
; 呼び出されるすべての手続きをトレースせよ. 
; 特にapply-genericは何回呼び出されるか. 
; それぞれの場合どの手続きが振り分けられるか. 
; 
;= Prepared =============================================================================

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error
                  "No method for these types -- APPLY-GENERIC"
                  (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;= Answer ===============================================================================

