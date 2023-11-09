#lang racket
(require "Utility.rkt")
(require "Parser.rkt")
(require "Processor.rkt")

define var_env

  '(;environment

    (;global variable scope

     (a 3) (b 4) (c 5)

       )

    )

  )

(define code '( call function (a) ( call function (r) a( (a))) (5)))

(define parsed (parser code))

(parsed)

(processor parsed var_env)
