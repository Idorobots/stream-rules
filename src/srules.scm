;; The main etry point.

(load "../src/utils.scm")
(load "../src/rete.scm")
(load "../src/pubsub.scm")
(load "../src/streams.scm")

;; Example
(define facts (pub-sub))

(define (fact-matches? pattern)
  (matches? (source-subscribe! facts) pattern))

(define functional-language
  (also (fact-matches? '(is ?lang language))
        (fact-matches? '(has-paradigm ?lang functional))))
(define functional-language-rule
  (whenever functional-language
            (lambda (vars)
              (display "This language is functional: ")
              (display (get-binding vars '?lang))
              (newline))))

(define object-oriented-language
  (also (fact-matches? '(is ?lang language))
        (fact-matches? '(has-paradigm ?lang object-oriented))))
(define object-oriented-language-rule
  (whenever object-oriented-language
            (lambda (vars)
              (display "This language is object oriented: ")
              (display (get-binding vars '?lang))
              (newline))))

(define awesome-language-rule
  (whenever (also functional-language
                  (fact-matches? '(is-lispy? ?lang true)))
            (lambda (vars)
              (display "This language is awesome: ")
              (display (get-binding vars '?lang))
              (newline))))

(define weird-ass-language-rule
  (whenever (either (also functional-language object-oriented-language)
                    (fact-matches? '(is-lispy? ?lang false)))
            (lambda (vars)
              (display "This language sucks: ")
              (display (get-binding vars '?lang))
              (newline))))

(-> (either functional-language-rule
            object-oriented-language-rule
            weird-ass-language-rule
            awesome-language-rule)
    (run-with (sink identity)))

(map (curry publish facts)
     (list '(is scala language)
           '(has-paradigm scala functional)
           '(has-paradigm scala object-oriented)
           '(is-lispy? scala false)
           '(is clojure language)
           '(has-paradigm clojure functional)
           '(has-paradigm clojure object-oriented)
           '(is-lispy? clojure true)
           '(is haskell language)
           '(has-paradigm haskell functional)
           '(is-lispy? haskell false)
           '(is java language)
           '(has-paradigm java object-oriented)
           '(is-lispy? java false)
           '(is spartan language)
           '(has-paradigm spartan functional)
           '(is-lispy? spartan true)))
