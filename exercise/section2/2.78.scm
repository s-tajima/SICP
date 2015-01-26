;= Question =============================================================================
;
; scheme-numberパッケージの内部手続きは, 実質的には基本手続き+, -などを呼び出すだけである. 
; 言語の基本手続きを直接呼び出すのは, 
; 型タグシステムでは各データオブジェクトに型をつける必要があるので不可能である. 
; 
; しかし実際のところ, Lispの実装は, 内部的に使う型つきシステムになっている. 
; 基本手続きsymbol?やnumber?などは, データオブジェクトがある型であるかどうかを決定する. 
; 
; 2.4.2節のtype-tag, contentsおよびattach-tagを修正し, 
; この汎用システムがSchemeの内部型システムの利点が使えるようにせよ. 
; つまり通常の数は, そのcarが記号scheme-numberである対ではなく, 
; Schemeの数として表現されている点の他は, システムは前の通り働く. 
; 
;= Prepared =============================================================================

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;= Answer ===============================================================================

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)
         'scheme-number)
        (else
          (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum)
         (cdr datum))
        ((number? datum)
         datum)))

