;= Question =============================================================================
;
; 問題 5.18
; 
; 5.2.1節のmake-register手続きを修正し, レジスタがトレース出来るようにせよ. 
; レジスタはトレースを開始と停止するメッセージを受け入れる. 
; レジスタをトレースする時は, レジスタへの値の代入では, レジスタ名, 
; レジスタの古い内容と代入する新しい内容を印字する. 
; 計算機モデルのインターフェースを拡張し, 
; 指示した計算機レジスタのトレースの開始と停止が出来るようにせよ. 
; 
;= Answer ================================================================================

(load "./machine.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; ex 5.15 counting instructions
        (instruction-count 0)
        ;; ex 5.16 instruction tracing, off by default
        (instruction-tracing-on #f))
    
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                ;; ex 5.15 counting instructions
                (set! instruction-count (+ instruction-count 1))
                ;; ex 5.16 instruction tracing
                (if instruction-tracing-on
                    (begin
                      (display "TRACE ")
                      ;; ex 5.17 if there is a label, print it too
                      (if (not (null? (instruction-label (car insts))))
                          (begin
                            (display "(LABEL ")
                            (display (instruction-label (car insts)))
                            (display ") ")))
                      (display (instruction-text (car insts)))
                      (newline)))
                (execute)))))
      
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)

              ;; ex 5.15 counting instructions
              ((eq? message 'get-count) instruction-count)
              ((eq? message 'reset-count)
               (lambda () (set! instruction-count 0)))

              ;; ex 5.16 instruction tracing
              ((eq? message 'trace-on)
               (set! instruction-tracing-on #t)
               'instruction-tracing-on)
              ((eq? message 'trace-off)
               (set! instruction-tracing-on #f)
               'instruction-tracing-off)
              ((eq? message 'trace?)
               instruction-tracing-on)
              
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; ex 5.17 instructions are now represented as a list of three items:
;; text, execution procedure, and label, if there is any
(define (make-instruction text) (list text '() '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cadr inst))
(define (instruction-label inst) (caddr inst))
(define (set-instruction-execution-proc! inst proc) (set-car! (cdr inst) proc))
(define (set-instruction-label! inst label) (set-car! (cddr inst) label))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (begin
                 ;; ex 5.17 if this isn't the last instruction (for instance,
                 ;; a label "done" as the last item in the register
                 ;; code text
                 (if (not (null? insts))
                     ;; set the label associated with this instruction
                     (set-instruction-label! (car insts) next-inst))
                 
                 (receive insts
                          (cons (make-label-entry next-inst
                                                  insts)
                                labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

;; ex 5.18
(define (make-register name)
  (let ((contents '*unassigned*)
        (register-trace-on #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if register-trace-on
                   (begin 
                     (display "REG TRACE: ")
                     (display name)
                     (display " OLD: ")
                     (display contents)
                     (display " NEW: ")
                     (display value)
                     (newline)))
               (set! contents value)))
            ;; ex 5.18
            ((eq? message 'trace-on)
             (set! register-trace-on #t))
            ((eq? message 'trace-off)
             (set! register-trace-on #f))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (set-register-trace-flag machine register-name flag)
  ((get-register machine register-name) flag))

(define fact-machine
  (make-machine
    '(continue val n)
    (list (list '= =) (list '- -) (list '* *))
    '(start
       (assign continue (label fact-done))
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
       fact-done)))

(fact-machine 'trace-on)
(set-register-trace-flag fact-machine 'n 'trace-on)
(set-register-trace-flag fact-machine 'val 'trace-on)
(set-register-contents! fact-machine 'n 3)
(start fact-machine)
(get-register-contents fact-machine 'val)
