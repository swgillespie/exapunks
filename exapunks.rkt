#lang racket
(require racket/cmdline)

(define (pair-with? form key)
  (and (pair? form) (eq? (car form) key)))

(define (solution? form)
  (pair-with? form 'defsolution))

(define (solution-name form)
  (cadr form))

(define (solution-exas form)
  (cddr form))

(define (exa? form)
  (pair-with? form 'defexa))

(define (exa-name form)
  (cadr form))

(define (exa-instr-forms form)
  (cddr form))

(define (halt? form)
  (pair-with? form 'halt))

(define (grab? form)
  (pair-with? form 'grab))

(define (grab-number form)
  (cadr form))

(define (link? form)
  (pair-with? form 'link))

(define (link-number form)
  (cadr form))

(define (make? form)
  (pair-with? form 'make))

(define (kill? form)
  (pair-with? form 'kill))

(define (seek? form)
  (pair-with? form 'seek))

(define (seek-number form)
  (cadr form))

(define (copy? form)
  (pair-with? form 'copy))

(define (copy-src form)
  (cadr form))

(define (copy-dest form)
  (caddr form))

(define (loop? form)
  (pair-with? form 'loop))

(define (loop-body form)
  (cdr form))

(define (addi? form)
  (pair-with? form 'addi))

(define (subi? form)
  (pair-with? form 'subi))

(define (divi? form)
  (pair-with? form 'divi))

(define (modi? form)
  (pair-with? form 'modi))

(define (arith-arg-1 form)
  (cadr form))

(define (arith-arg-2 form)
  (caddr form))

(define (arith-arg-3 form)
  (cadddr form))

(define (repl? form)
  (pair-with? form 'repl))

(define (repl-target form)
  (cadr form))

(define (continue? form)
  (pair-with? form 'continue))

(define (continue-if? form)
  (pair-with? form 'continue-if))

(define (break? form)
  (pair-with? form 'break))

(define (break-if? form)
  (pair-with? form 'break-if))

(define (jump-if? form)
  (pair-with? form 'jump-if))

(define (jump-if-target form)
  (caddr form))

(define (unroll? form)
  (pair-with? form 'unroll))

(define (unroll-count form)
  (cadr form))

(define (unroll-body form)
  (cddr form))

(define (conditional-jump-expr form)
  (cadr form))

(define (condidional-eof? form)
  (eq? form 'eof))

(define (conditional-mrd? form)
  (eq? form 'mrd))

(define (make-label prefix)
  (string-append prefix "_" (symbol->string (gensym))))

(define (register? val)
  (or (eq? val 'x) (eq? val 't) (eq? val 'm) (eq? val 'f)))

(define (validate-reg val)
  (unless (register? val)
    (error "argument must be a register")))

(define (validate-num-or-reg val)
  (unless (or (register? val) (number? val))
    (error "argument must be number or register")))

(define (validate-label val)
  (unless (or (string? val) (symbol? val))
    (error "argument must be a label")))

(define (validate-simple-expression form)
  (if (or (pair-with? form '=) (pair-with? form '<) (pair-with? form '>))
    (begin
      (unless (= (length form) 3)
        (error "expression must have exactly two arguments"))
      (unless (andmap (lambda (f) (or (number? f) (register? f))) (cdr form))
        (error "expression operands must be registers or integers")))
    (unless (eq? form 'eof) (eq? form 'mrd)
      (error "expression must be a comparison expression, eof, or mrd"))))

(define (validate-in val candidates)
  (unless (memq val candidates)
    (error "expression must be one of" candidates)))

(define (expression-op form)
  (car form))

(define (expression-lhs form)
  (cadr form))

(define (expression-rhs form)
  (caddr form))

(define (gen-note str)
  (printf "NOTE ~a~n" str))

(define (gen-halt)
  (printf "HALT~n"))

(define (gen-grab num)
  (validate-num-or-reg num)
  (printf "GRAB ~a~n" num))

(define (gen-link num)
  (validate-num-or-reg num)
  (printf "LINK ~a~n" num))

(define (gen-mark label)
  (validate-label label)
  (printf "MARK ~a~n" label))

(define (gen-jump label)
  (validate-label label)
  (printf "JUMP ~a~n" label))

(define (gen-tjmp label)
  (validate-label label)
  (printf "TJMP ~a~n" label))

(define (gen-make)
  (printf "MAKE~n"))

(define (gen-seek num)
  (validate-num-or-reg num)
  (printf "SEEK ~a~n" num))

(define (gen-copy src dst)
  (validate-num-or-reg src)
  (validate-reg dst)
  (printf "COPY ~a ~a~n" src dst))

(define (gen-addi arg1 arg2 arg3)
  (gen-arith "ADDI" arg1 arg2 arg3))

(define (gen-subi arg1 arg2 arg3)
  (gen-arith "SUBI" arg1 arg2 arg3))

(define (gen-divi arg1 arg2 arg3)
  (gen-arith "DIVI" arg1 arg2 arg3))

(define (gen-modi arg1 arg2 arg3)
  (gen-arith "MODI" arg1 arg2 arg3))

(define (gen-test op lhs rhs)
  (validate-in op '(= < >))
  (validate-num-or-reg lhs)
  (validate-num-or-reg rhs)
  (printf "TEST ~a ~a ~a~n" lhs op rhs))

(define (gen-test-eof)
  (printf "TEST EOF~n"))

(define (gen-test-mrd)
  (printf "TEST MRD~n"))

(define (gen-arith mnemonic arg1 arg2 arg3)
  (validate-num-or-reg arg1)
  (validate-num-or-reg arg2)
  (validate-reg arg3)
  (printf "~a ~a ~a ~a~n" mnemonic arg1 arg2 arg3))

(define (gen-repl target)
  (validate-label target)
  (printf "REPL ~a~n" target))

(define (gen-kill)
  (printf "KILL~n"))

(define (compile-form form)
  (if (solution? form)
    (begin
      (printf "NOTE ---- Solution ~a ----~n" (solution-name form)) 
      (printf "NOTE Compiled by Sean Gillespie's exapunk compiler~n")
      (compile-solution form ))
    (error "unexpected top-level form, expected 'defsolution'")))

(define (compile-solution form)
  (map compile-exa (solution-exas form)))

(define (compile-exa form)
  (if (exa? form)
    (begin
      (printf "NOTE ---- Exa ~a ----~n" (exa-name form))
      (map compile-instr-form (exa-instr-forms form)))
    (error "unexpected exa-level form, expected 'defexa'")))

(define (compile-instr-form form)
  (cond
    ((halt? form) (gen-halt))
    ((grab? form) (gen-grab (grab-number form)))
    ((link? form) (gen-link (link-number form)))
    ((seek? form) (gen-seek (seek-number form)))
    ((copy? form) (gen-copy (copy-src form) (copy-dest form)))
    ((addi? form) (gen-addi (arith-arg-1 form) (arith-arg-2 form) (arith-arg-3 form)))
    ((subi? form) (gen-subi (arith-arg-1 form) (arith-arg-2 form) (arith-arg-3 form)))
    ((divi? form) (gen-divi (arith-arg-1 form) (arith-arg-2 form) (arith-arg-3 form)))
    ((modi? form) (gen-modi (arith-arg-1 form) (arith-arg-2 form) (arith-arg-3 form)))
    ((repl? form) (gen-repl (repl-target form)))
    ((kill? form) (gen-kill))
    ((unroll? form) (compile-unroll form))
    ((jump-if? form) (compile-jump-if form))
    ((loop? form) (compile-loop form))
    (else (error "unknown instruction form" form))))

(define (compile-loop form)
  (let ((loop-header-label (make-label "loop"))
        (loop-exit-label (make-label "loopexit")))
    (gen-mark loop-header-label)
    (let ((body (loop-body form)))
      (map (lambda (f) (compile-loop-instr f loop-header-label loop-exit-label)) body))
    (gen-jump loop-header-label)
    (gen-mark loop-exit-label)))

(define (compile-loop-instr form header exit)
  (cond
    ((break? form) (gen-jump exit))
    ((break-if? form) (compile-break-if form header exit))
    ((continue? form) (gen-jump header))
    ((continue-if? form) (compile-continue-if form header exit))
    (else (compile-instr-form form))))

(define (compile-break-if form header exit)
  (compile-conditional (conditional-jump-expr form))
  (gen-tjmp exit))

(define (compile-continue-if form header exit out-port)
  (compile-conditional (conditional-jump-expr form))
  (gen-tjmp exit))

(define (compile-jump-if form target)
  (compile-conditional (conditional-jump-expr form))
  (validate-label target)
  (gen-tjmp target))

(define (compile-conditional form)
  (validate-simple-expression form)
  (cond
    ((condidional-eof? form) (gen-test-eof))
    ((conditional-mrd? form) (gen-test-mrd))
    (else (compile-comparison form))))

(define (compile-comparison form)
  (let ((op (expression-op form))
        (lhs (expression-lhs form))
        (rhs (expression-rhs form)))
    (gen-test op lhs rhs)))

(define (compile-unroll form)
  (letrec ((do-unroll (lambda (body count)
                         (unless (= count 0)
                           (begin
                             (map compile-instr-form body)
                             (do-unroll body (- count 1)))))))
    (do-unroll (unroll-body form) (unroll-count form))))

(define (compile filename)
  (let* ((port (open-input-file filename))
         (syntax (read port)))
    (compile-form syntax)))

(define file-to-compile
  (command-line
   #:program "exapunks"
   #:args (filename)
   filename))

(void (compile file-to-compile))
