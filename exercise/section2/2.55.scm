;= Question =============================================================================
; 
; 問題 2.55
; Eva Lu Atorは解釈系に対して式
; 
; (car ''abracadabra)
; 
; と入力した. 驚いたことに解釈系はquoteと印字してきた. なぜか. 
; 
;= Prepared =============================================================================

(print (car ''abracadabra))
(print ''abracadabra)
(print (car (quote 'abracadabra)))

;= Answer ===============================================================================

