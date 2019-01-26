#lang scheme

;1) get-record method for headquarters
(define (get-record-<key> given-key division)
  ((get 'get-record-<key> division) given-key))

;The record of each division must be structered so that the division name
; is at the first entry of the list. There must be the keys: name, address, and salary
; in any order
; For example:
(<division> <key-name> <key-address> <key-salary> ... <data>)

;2)

(define (get-salary-name given-key-name division data-list)
  ((get 'get-salary-name division) given-key-name data-list))

(put 'get-salary-name division1 get-salary-name)

;For example division A with data structure:
(<division> <key-name> <key-address> <key-salary> ...)

(define (get-salary-name given-key-name data-list)
  (define (key-name data)
    (cadr data))
  (if (equal? (key-name (car data-list))) (list (cadddr (car data-list)) true)
      (get-salary-name given-key-name (cdr data-list))))

(put 'get-salary-name 'division-A get-salary-name)

;test
(get-salary-name 'Rachel 'division-A data-list)

;3 For example if we have all division methods of find-employee-record
(define (find-employee-record given-key-name division data-list)
  ((get 'find-employee-record division) given-key-name data-list))

(define (find-employee-record-all given-key-name list-divisions all-div-data found?)
  (if (null? list-divisions) (display "No employee of that name found")
        (let ((x 
         find-employee-record given-key-name (car list-divisions) all-div-data))
          (if (cadr x) (car x)
              (find-employee-record-all given-key-name (cdr list-divisions) all-div-data false)))))

;4 Make new employee data and also assign what division they will be in. For employees staying
; in the new company, add those divisions from new company. If company will still be independent
; all data must have tag company and division

      