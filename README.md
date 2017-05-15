# git time-warp

lisping through time.




    ;; warp.lisp on branch "fn/inc"

    (lambda (x) (+ 1 x))


    ;; warp.lisp on "vars/two"

    (fn/inc 1)


    ;; warp.lisp on "master"

    (+ vars/two 3)  ;; => 5
