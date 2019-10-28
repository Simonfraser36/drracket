(define-struct pr (first-name last-name))
;; A personnel record (PR) is a (make-pr Str Str)
;; An employee database (ED) is a (listof PR)

;; (pr-template pr)
;; pr-template: pr -> Any


(define (pr-template pr)
  (... (pr-first-name pr) ...
       (pr-last-name pr) ...))

;; (employee-template employee-database)
;; employee-template: ED -> Any
(define (employee-template employee-database)
  (cond [(empty? employee-database) ...]
        [(cons? employee-database)
         (... (pr-template employee-database) ...
              (employee-template (rest employee-database)) ...)]))

(define ed-location (list (make-pr "Jake" "Peterson")
                        (make-pr "Walter" "Jeffrey")
                        (make-pr "Jonas" "Jordan")
                        (make-pr "Fred" "Herbert")
                        (make-pr "Ford" "Williams")
                        (make-pr "Ford" "Houston")
                        (make-pr "Ford" "Jeffrey")))

(define ed-location2 (list (make-pr "Thomas" "Jefferson")
                         (make-pr "Don" "Williamson")
                         (make-pr "Simon" "Walter")
                         (make-pr "Fred" "Jackson")
                         (make-pr "Percy" "Jackson")))

(define ed-location3 (list (make-pr "Gord" "Downie")
                          (make-pr "Gab" "Baker")
                          (make-pr "Gordon" "Rolando")
                          (make-pr "Langston" "Hughes")
                          (make-pr "Carmen" "Hughes")
                          (make-pr "Gord" "MacDonald")
                          (make-pr "Rob" "Murphy")))

(define ed-location4 (list (make-pr "William" "Stirling")
                           (make-pr "Jed" "Simon")
                           (make-pr "Gord" "MacDonald")
                           (make-pr "Donald" "Fredrickson")
                           (make-pr "Rob" "Murphy")))

(define ed-location5 (list (make-pr "Davis" "Johnson")
                           (make-pr "Jane" "Letinberg")
                           (make-pr "Frank" "MacDonald")
                           (make-pr "Carl" "Fredrickson")
                           (make-pr "Robert" "Maker")))

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 2 (a) i.
;; ***************************************************
;;

;; helper function
;; (pr->string pr1 symbol) consumes a PR pr1 and a Sym sym1 and produces
;;     the string containing exactly the first name of pr1 followed by the
;;     last name of pr1 if sym1 is 'first and the string containing exactly
;;     the last name of pr1 followed by the first name of pr1 if sym1 is 'last.
;; pr->string: PR (anyof 'first 'last) -> Str

(define (pr->string pr1 symbol)
  (cond [(symbol=? symbol 'first)
         (string-append (pr-first-name pr1) (pr-last-name pr1))]
        [(symbol=? symbol 'last)
         (string-append (pr-last-name pr1) (pr-first-name pr1))]))

;; (pr=? pr1 pr2) consumes two PRs pr1 and pr2 and produces true if they are
;;     identical
;; pr=?: PR PR -> Bool
;; Examples
(check-expect (pr=? (make-pr "Gordon" "Ramsey")
                    (make-pr "Gordon" "Ramsey")) true)
(check-expect (pr=? (make-pr "Gordon" "Fleming")
                    (make-pr "Fleming" "Gordon")) false)

(define (pr=? pr1 pr2)
  (and (string=? (pr-first-name pr1) (pr-first-name pr2))
       (string=? (pr-last-name pr1) (pr-last-name pr2))))

;; Tests
(check-expect (pr=? (make-pr "George" "Fred")
                    (make-pr "George" "Houston")) false)
(check-expect (pr=? (make-pr "Gordon" "Tennyson")
                    (make-pr "Gord" "Tennyson")) false)
(check-expect (pr=? (make-pr "Gordon" "Flambert")
                    (make-pr "Geoffrey" "Flambert")) false)
(check-expect (pr=? (make-pr "Fred" "Fleming")
                    (make-pr "Freddie" "Fleming")) false)
(check-expect (pr=? (make-pr "Gordon" "Baker")
                    (make-pr "Gordon" "Baker")) true)
(check-expect (pr=? (make-pr "Jeff" "Fast")
                    (make-pr "Jeff" "Fast")) true)

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 2 (a) ii.
;; ***************************************************
;;



;; (pr<? pr1 pr2 symbol) consumes two PRs pr1 and pr2 and a Sym symbol. The
;;    Sym can either be 'first or 'last and determines whether the initial
;;    comparison should be based on the first/last name. The predicate
;;    produces true if the first PR precedes the second one lexicographically.
;;    If the initial comparison consumes two equal names, the predicate will
;;    consider the other name. If both PRs have identical first and last names,
;;    the predicate will produce false.
;; pr<?: PR PR Sym -> Bool
;;    requires: Sym is (anyof 'first 'last)
;; Examples
(check-expect
 (pr<? (make-pr "Gordon" "Town") (make-pr "Gordon" "Townsend") 'first) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Geoffrey" "Duwn") 'last) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Geoffrey" "Duwn") 'first) false)
(check-expect
 (pr<? (make-pr "Geoffrey" "Down") (make-pr "Geoffrey" "Down") 'first) false)

(define (pr<? pr1 pr2 symbol)
  (cond [(pr=? pr1 pr2) false]
        [(string<? (pr->string pr1 symbol) (pr->string pr2 symbol)) true]
        [else false]))

;; Tests (first all the possibilities for first)
(check-expect
 (pr<? (make-pr "Geoffrey" "Down") (make-pr "Gord" "Down") 'first) true)
(check-expect
 (pr<? (make-pr "Geoffrey" "Doe") (make-pr "Geoffrey" "Dow") 'first) true)
(check-expect
 (pr<? (make-pr "Geoffrey" "Ted") (make-pr "Geoffrey" "Ted") 'first) false)
(check-expect
 (pr<? (make-pr "Geoffrey" "Ted") (make-pr "Geoffrey" "Dom") 'first) false)
(check-expect
 (pr<? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Town") 'first) false)

;; checking all of the possibilities for last
(check-expect
 (pr<? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Town") 'last) true)
(check-expect
 (pr<? (make-pr "Geoffrey" "Down") (make-pr "Gord" "Down") 'last) true)
(check-expect
 (pr<? (make-pr "Geoffrey" "Ted") (make-pr "Geoffrey" "Ted") 'last) false)
(check-expect
 (pr<? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Down") 'last) false)
(check-expect
 (pr<? (make-pr "Ted" "Dom") (make-pr "Geoffrey" "Cotton") 'last) false)


;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 2 (a) ii.
;; ***************************************************
;;

;; (pr>? pr1 pr2 symbol) consumes two PRs pr1 and pr2 and a Sym symbol. The
;;    Sym can either be 'first or 'last and determines whether the initial
;;    comparison should be based on the first/last name. The predicate
;;    produces true if the first PR is lexicographically larger than the second
;;    PR. If the initial comparison consumes two equal names, the predicate will
;;    consider the other name. If both PRs have identical first and last names,
;;    the predicate will produce false.
;; pr>?: PR PR Sym -> Bool
;;    requires: Sym is (anyof 'first 'last)
;; Examples
(check-expect
 (pr>? (make-pr "Gordon" "Town") (make-pr "Gordon" "Townsend") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Geoffrey" "Duwn") 'last) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Geoffrey" "Duwn") 'first) true)
(check-expect
 (pr>? (make-pr "Geoffrey" "Down") (make-pr "Geoffrey" "Down") 'first) false)

(define (pr>? pr1 pr2 symbol)
  (cond [(pr=? pr1 pr2) false]
        [(string>? (pr->string pr1 symbol) (pr->string pr2 symbol))
         true]
        [else false]))

;; Tests (first all the possibilities for first)
(check-expect
 (pr>? (make-pr "Geoffrey" "Down") (make-pr "Gord" "Down") 'first) false)
(check-expect
 (pr>? (make-pr "Geoffrey" "Doe") (make-pr "Geoffrey" "Dow") 'first) false)
(check-expect
 (pr>? (make-pr "Geoffrey" "Ted") (make-pr "Geoffrey" "Ted") 'first) false)
(check-expect
 (pr>? (make-pr "Geoffrey" "Ted") (make-pr "Geoffrey" "Dom") 'first) true)
(check-expect
 (pr>? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Cotton") 'first) true)

;; checking all of the possibilities for last
(check-expect
 (pr>? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Town") 'last) false)
(check-expect
 (pr>? (make-pr "Geoffrey" "Down") (make-pr "Gord" "Down") 'last) false)
(check-expect
 (pr>? (make-pr "Geoffrey" "Ted") (make-pr "Geoffrey" "Ted") 'last) false)
(check-expect
 (pr>? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Down") 'last) true)
(check-expect
 (pr>? (make-pr "Ted" "Down") (make-pr "Geoffrey" "Cotton") 'last) true)

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 2 (b)
;; ***************************************************
;;


;; helper function (insert pr1 sorted-ed) consumes a PR pr1, a sorted ED
;;    ed, and produces the sorted ED containing pr1.

(define (insert pr1 sorted-ed symbol)
  (cond [(empty? sorted-ed) (cons pr1 sorted-ed)]
        [(pr<? pr1 (first sorted-ed) symbol) (cons pr1 sorted-ed)]
        [else (cons (first sorted-ed)
                    (insert pr1 (rest sorted-ed) symbol))]))

;; (sort-ed ed sym) consumes an ED ed and a Sym sym. The Sym can either
;;    be 'last, which would sort the employee database by last name, or
;;    'first, which would sort it by first name. If the initial comparison
;;    compares two equal names, the predicate will consider the other name.
;;    The function produces an ED.
;; sort-ed: ED Sym -> ED
;; Examples
(check-expect (sort-ed ed-location 'last)
              (list (make-pr "Fred" "Herbert")
                    (make-pr "Ford" "Houston")
                    (make-pr "Ford" "Jeffrey")
                    (make-pr "Walter" "Jeffrey")
                    (make-pr "Jonas" "Jordan")
                    (make-pr "Jake" "Peterson")
                    (make-pr "Ford" "Williams")))
(check-expect (sort-ed ed-location 'first)
              (list (make-pr "Ford" "Houston")
                    (make-pr "Ford" "Jeffrey")
                    (make-pr "Ford" "Williams")
                    (make-pr "Fred" "Herbert")
                    (make-pr "Jake" "Peterson")
                    (make-pr "Jonas" "Jordan")
                    (make-pr "Walter" "Jeffrey")))

(define (sort-ed ed sym)
  (cond [(empty? ed) empty]
        [else (insert (first ed) (sort-ed (rest ed) sym) sym)]))


;; Tests
(check-expect (sort-ed ed-location2 'first)
              (list (make-pr "Don" "Williamson")
                    (make-pr "Fred" "Jackson")
                    (make-pr "Percy" "Jackson")
                    (make-pr "Simon" "Walter")
                    (make-pr "Thomas" "Jefferson")))
(check-expect (sort-ed ed-location2 'last)
              (list (make-pr "Fred" "Jackson")
                    (make-pr "Percy" "Jackson")
                    (make-pr "Thomas" "Jefferson")
                    (make-pr "Simon" "Walter")
                    (make-pr "Don" "Williamson")))
(check-expect (sort-ed ed-location3 'last)
              (list (make-pr "Gab" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Carmen" "Hughes")
                    (make-pr "Langston" "Hughes")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Gordon" "Rolando")))

(check-expect (sort-ed ed-location3 'first)
              (list (make-pr "Carmen" "Hughes")
                    (make-pr "Gab" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Gordon" "Rolando")
                    (make-pr "Langston" "Hughes")
                    (make-pr "Rob" "Murphy")))

(check-expect (sort-ed ed-location4 'last)
              (list (make-pr "Donald" "Fredrickson")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Jed" "Simon")
                    (make-pr "William" "Stirling")))       
                    
(check-expect (sort-ed ed-location4 'first)
              (list (make-pr "Donald" "Fredrickson")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Jed" "Simon")
                    (make-pr "Rob" "Murphy")
                    (make-pr "William" "Stirling")))   

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 2 (c)
;; ***************************************************
;;

;; (merge-ed-sorted ed1 ed2) consumes two sorted EDs ed1 and ed2 and merges
;;     them into a single ED.
;; merge-ed-sorted: ED ED -> ED
;; Example

(check-expect (merge-ed-sorted (sort-ed ed-location3 'last)
                               (sort-ed ed-location4 'last))
              (list (make-pr "Gab" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Donald" "Fredrickson")
                    (make-pr "Carmen" "Hughes")
                    (make-pr "Langston" "Hughes")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Gordon" "Rolando")
                    (make-pr "Jed" "Simon")
                    (make-pr "William" "Stirling")))

(define (merge-ed-sorted ed1 ed2)
  (cond [(empty? ed1) ed2]
        [(empty? ed2) ed1]
        [(pr<? (first ed1) (first ed2) 'last)
         (cons (first ed1) (merge-ed-sorted (rest ed1) ed2))]
        [(pr<? (first ed2) (first ed1) 'last)
         (cons (first ed2) (merge-ed-sorted ed1 (rest ed2)))]
        [else
         (cons (first ed1) (merge-ed-sorted (rest ed1) (rest ed2)))]))

;; Test
(check-expect (merge-ed-sorted (sort-ed ed-location4 'last)
                               (sort-ed ed-location5 'last))
              (list (make-pr "Carl" "Fredrickson")
                    (make-pr "Donald" "Fredrickson")
                    (make-pr "Davis" "Johnson")
                    (make-pr "Jane" "Letinberg")
                    (make-pr "Frank" "MacDonald")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Robert" "Maker")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Jed" "Simon")
                    (make-pr "William" "Stirling")))

;; (merge-ed ed1 ed2) consumes two EDs ed1 and ed2, not necessarily sorted, and
;;     outputs a sorted ED containing all of the entries in at least one
;;     of ed1 and ed2
;; merge-ed: ED ED -> ED
;; Example
(check-expect (merge-ed ed-location2 ed-location4)
              (list (make-pr "Donald" "Fredrickson")
                    (make-pr "Fred" "Jackson")
                    (make-pr "Percy" "Jackson")
                    (make-pr "Thomas" "Jefferson")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Jed" "Simon")
                    (make-pr "William" "Stirling")
                    (make-pr "Simon" "Walter")
                    (make-pr "Don" "Williamson")))

(define (merge-ed ed1 ed2)
  (merge-ed-sorted (sort-ed ed1 'last) (sort-ed ed2 'last)))

;; Tests
(check-expect (merge-ed empty ed-location5)
              (list (make-pr "Carl" "Fredrickson")
                    (make-pr "Davis" "Johnson")
                    (make-pr "Jane" "Letinberg")
                    (make-pr "Frank" "MacDonald")
                    (make-pr "Robert" "Maker")))

(check-expect (merge-ed ed-location4 empty)
              (list (make-pr "Donald" "Fredrickson")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Jed" "Simon")
                    (make-pr "William" "Stirling")))
              
(check-expect (merge-ed ed-location2 ed-location5)
              (list (make-pr "Carl" "Fredrickson")
                    (make-pr "Fred" "Jackson")
                    (make-pr "Percy" "Jackson")
                    (make-pr "Thomas" "Jefferson")
                    (make-pr "Davis" "Johnson")
                    (make-pr "Jane" "Letinberg")
                    (make-pr "Frank" "MacDonald")
                    (make-pr "Robert" "Maker")
                    (make-pr "Simon" "Walter")
                    (make-pr "Don" "Williamson")))

(check-expect (merge-ed ed-location3 ed-location5)
              (list (make-pr "Gab" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Carl" "Fredrickson")
                    (make-pr "Carmen" "Hughes")
                    (make-pr "Langston" "Hughes")
                    (make-pr "Davis" "Johnson")
                    (make-pr "Jane" "Letinberg")
                    (make-pr "Frank" "MacDonald")
                    (make-pr "Gord" "MacDonald")
                    (make-pr "Robert" "Maker")
                    (make-pr "Rob" "Murphy")
                    (make-pr "Gordon" "Rolando")))

