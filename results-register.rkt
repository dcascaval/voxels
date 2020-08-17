; box 2-2
(define (synthed-func input0)
  (define r0 input0)
  (define r1 (translateZ+ r0))
  (define r2 (union r0 r1))
  (define r3 (translateX+ r2))
  (define r4 (union r2 r3))
  (define r5 (translateY+ r4))
  (define r6 (union r5 r4))
  r6))

; planes-3. This is expressible with mirrors but would otherwise
; be impossible with the standard translate functions.
(define (synthed-func input0)
  (define r0 input0)
  (define r1 (mirrorX r0))
  (define r2 (mirrorY r1))
  (define r3 (mirrorZ r2))
  r3))

; planes-3-chunk
; Combines mirror & diff
(define (synthed-func input0)
  (define r0 input0)
  (define r1 (mirrorX r0))
  (define r2 (mirrorY r1))
  (define r3 (mirrorZ r2))
  (define r4 (diff r3 r1))
  r4))

; true-mirror-box
(define (synthed-func input0)
  (define r0 input0)
  (define r1 (tY+ r0))
  (define r2 (tZ+ r1))
  (define r3 (tX+ r2))
  (define r4 (union r3 r0))
  (define r5 (mirrorX r4))
  (define r6 (mirrorY r5))
  (define r7 (mirrorZ r6))
  r7))

; unable to effectivly synthesize programs longer than 7 instructions.
; even 7 is only feasible when there is known to be a solution, otherwise
; it can take 12-13x as long to prove that there isn't one.
;
; unfortunately, the register language is not very expressive, and a depth-7
; AST can do much, much more than 7 instructions can (that is up to 2^7 registers,
; and the register language only advantages us when the AST subtrees are shared,
; which isn't the case with even slightly-irregular result shapes).