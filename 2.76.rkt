#lang scheme

;Add new types:
; explicit: require new constructors and selectors, and methods
; data-driven: just add new methods to put and get table generic
; message-passing: require new constructors and selectors and methods

;Add new ops:
; explicit: require new methods for every data type
; data-driven: add new methods and put in get table for every data type
; message-passing: add methods in the constructors

;data-driven is best to add new types
;message-passing is best to add new ops.

