;; A listofX is one of:
;; * empty
;; * (cons X listofX)
;; (listofX-template listofX)
;; listofX-template: listofX -> Any

(define (listofX-template listofX)
  (cond [(empty? listofX) ...]
        [(cons? listofX) (... (first listofX) ...
                              (listofX-template (rest listofX)) ...)]))

(define-struct goon (street-name abilities))
;; A Goon is a (make-goon Str Abilities)
;; An Abilities is a (list Nat Nat Nat), where the elements
;;   represent Loyalty, Wealth, and Influence.

;; (get-loyalty-abilities abilities)
(define (get-loyalty-abilities abilities)
  (first abilities))

;; (get-wealth-abilities abilities)
(define (get-wealth-abilities abilities)
  (first (rest abilities)))

;; (get-influence-abilities abilities)
(define (get-influence-abilities abilities)
  (first (rest (rest abilities))))


;; (abilities-template abilities)
;; abilities-template: Abilities -> Any

(define (abilities-template abilities)
  (... (get-loyalty-abilities abilities) ...
       (get-wealth-abilities abilities) ...
       (get-influence-abilities abilities) ...))

;; (goon-template goon)
;; goon-template: goon -> Any
(define (goon-template goon)
  (... (goon-street-name goon) ...
       (abilities-template (goon-abilities goon)) ...))

;; constants for goons
(define goon-sc (make-goon "Sneaky Clarence" (list 5 6 7)))
(define goon-stb (make-goon "Steward, the Blade" (list 3 9 10)))
(define goon-ff (make-goon "Fantastic Franky" (list 19 20 19)))
(define goon-ms (make-goon "Mister Sol" (list 15 8 8)))
(define goon-cftg (make-goon "Cousin Fred the Gallant" (list 4 6 10)))
(define goon-cg (make-goon "Cousin George" (list 3 5 12)))
(define goon-fl (make-goon "Flamboyant Louis" (list 4 6 10)))
(define goon-cs (make-goon "Courageous Steven" (list 20 20 18)))

;; constants for applicants
(define applicant-eo (make-goon "Enthusiastic Owen" (list 4 6 10)))
(define applicant-sfj (make-goon "Slick Freddie Jones" (list 20 20 18)))
(define applicant-sc (make-goon "Secretive Clark" (list 5 6 7)))

;; A Gang is a (listof Goon)
;; (gang-template gang)
;; gang-template: Gang -> Any

(define (gang-template gang)
  (cond [(empty? gang) ...]
        [(cons? gang) (... (goon-template (first gang)) ...
                           (gang-template (rest gang)) ...)]))

;; constants for gangs
(define my-gang (list goon-sc goon-stb goon-ms))
(define my-gang2
  (list goon-sc goon-stb goon-cftg goon-cg goon-fl))
(define my-gang3 (list goon-ff goon-cs goon-fl goon-cftg))
(define my-gang4 (list goon-sc goon-fl goon-cftg goon-cg))
(define gang-with-duplicates
  (list goon-sc goon-ms goon-fl goon-fl goon-fl goon-cftg goon-cg))

;; functions for job
;; (get-loyalty-job job)
(define (get-loyalty-job job)
  (first job))

;; (get-wealth-job job)
(define (get-wealth-job job)
  (first (rest job)))

;; (get-influence-job job)
(define (get-influence-job job)
  (first (rest (rest job))))

;; A Job is a (list Nat Nat Nat), where the elements represent
;;    required Loyalty, Wealth, and Influence

;; (job-template job)
;; job-template: Job -> Any
(define (job-template job)
  (... (get-loyalty-job job) ...
       (get-wealth-job job) ...
       (get-influence-job job) ...))

;; constants with Job data type
(define job-banker (list 6 21 2))
(define job-bartender (list 3 5 0))
(define job-cook (list 7 8 20))
(define job-mule (list 3 3 0))
(define job-dancer (list 10 13 10))
(define job-artisan (list 7 8 8))

;; A Job-list is a (listof Job)

;; (job-list-template job-list)
;; job-list-template: Job-list -> Any
(define (job-list-template job-list)
  (cond [(empty? job-list) ...]
        [(cons? job-list)
         (... (job-template (first job-list)) ...
              (job-list-template (rest job-list)) ...)]))

;; constants with Job-list data type
(define my-jobs (list job-cook job-banker job-bartender))
(define my-jobs2
  (list job-mule job-dancer job-bartender job-cook job-banker))
(define my-jobs3
  (list job-mule job-dancer job-bartender job-cook job-artisan))
(define my-jobs4
  (list job-mule job-dancer job-bartender))
(define my-jobs5 (list job-mule job-bartender job-artisan))

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 3 (a)
;; ***************************************************
;;

;; (can-get-job? goon1 job1) consumes a Goon goon1 and a Job
;;     job1 and produces false if at least one of the goon's
;;     abilities do not meet the requirements for the job
;;     and true otherwise.
;; can-get-job?: Goon Job -> Bool
;;     requires: (goon-abilities goon1) and job1 have the same length
(define (can-get-job? goon1 job1)
  (cond [(empty? job1) false]
        [(and (= (length (goon-abilities goon1)) 1)
              (>= (first (goon-abilities goon1))
                  (first job1))) true]
        [(>= (first (goon-abilities goon1))
             (first job1))
         (can-get-job?
          (make-goon (goon-street-name goon1)
                     (rest (goon-abilities goon1)))
          (rest job1))]
        [else false]))

;; (how-qualified goon1 job1) consumes a Goon goon1 and a Job
;;     job1 and produces the sum of the differences between the
;;     goon1's abilities and job1's requirements
;; how-qualified: Goon Job -> Nat
;;     requires: (can-get-job? goon1 job1) is true
;;     requires: (goon-abilities goon1) and job1 have the same length
(define (how-qualified goon1 job1)
  (cond [(empty? job1) 0]
        [else
         (+ (- (first (goon-abilities goon1))
               (first job1))
            (how-qualified
             (make-goon (goon-street-name goon1)
                        (rest (goon-abilities goon1)))
             (rest job1)))]))

;; (eval-goon goon1 job1) consumes a Goon goon1 and a Job
;;     job1 and produces false if at least one of the goon's
;;     abilities do not meet the requirements for the job.
;;     If all requirements are met, the function produces a Nat
;;     that expresses how qualified the goon is. The Nat is calculated
;;     by adding up all the differences between the goon's abilities and
;;     the job's requirements.
;; eval-goon: Goon Job -> (anyof false Nat)
;; Examples

(check-expect (eval-goon goon-sc job-banker) false)
(check-expect (eval-goon goon-cftg job-mule) 14)
(check-expect (eval-goon goon-cg job-mule) 14)

(define (eval-goon goon1 job1)
  (cond [(can-get-job? goon1 job1) (how-qualified goon1 job1)]
        [else false]))

;; Tests
;; Does not meet Loyalty requirement
(check-expect (eval-goon goon-sc job-cook) false)
(check-expect (eval-goon goon-cftg job-bartender) 12)
(check-expect (eval-goon goon-stb job-cook) false)
;; Does not meet Wealth requirement
(check-expect (eval-goon goon-ff job-banker) false)
(check-expect (eval-goon goon-ms job-banker) false)

;; Does not meet Influence requirement
(check-expect (eval-goon goon-ms job-cook) false)
(check-expect (eval-goon goon-ff job-cook) false)

;; Meets all requirements
(check-expect (eval-goon goon-ms job-mule) 25)
(check-expect (eval-goon goon-ff job-mule) 52)
(check-expect (eval-goon goon-ff job-bartender) 50)
(check-expect (eval-goon goon-sc job-mule) 12)
(check-expect (eval-goon goon-ff job-dancer) 25)
(check-expect (eval-goon goon-cs job-dancer) 25)

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 3 (b)
;; ***************************************************
;;

;; (find-best-goon gang1 job1 best-goon-so-far) consumes a Gang
;;    gang1, a Job job1, and a Goon best-goon-so-far. The function
;;    produces the Goon that is most qualified for the job job1.
;;    If multiple goons are equally qualified, the function produces the
;;    first qualified one from your gang. If no goon is qualified, the
;;    function produces false.
;; find-best-goon: Gang Job Goon -> (anyof Goon false)
;; Examples
(check-expect (find-best-goon my-gang job-banker (first my-gang)) false)
(check-expect (find-best-goon my-gang3 job-dancer (first my-gang3)) goon-ff)
(check-expect (find-best-goon my-gang2 job-mule (first my-gang2)) goon-stb)

(define (find-best-goon gang1 job1 best-goon-so-far)
  (cond [(empty? gang1) false]
        [(and (= (length gang1) 1)
              (number? (eval-goon best-goon-so-far job1)))
         best-goon-so-far]
        [(and (= (length gang1) 1)
              (not (number? (eval-goon best-goon-so-far job1)))
              (number? (eval-goon (first gang1) job1)))
         (first gang1)]
        [(or (and (cons? gang1)
                  (number? (eval-goon (first gang1) job1))
                  (number? (eval-goon best-goon-so-far job1))
                  (> (eval-goon (first gang1) job1)
                     (eval-goon best-goon-so-far job1)))
             (and (number? (eval-goon (first gang1) job1))
                  (not (number? (eval-goon best-goon-so-far job1)))))
         (find-best-goon (rest gang1) job1 (first gang1))]
         [else (find-best-goon (rest gang1) job1 best-goon-so-far)]))

;; Tests
(check-expect (find-best-goon my-gang3 job-dancer (first my-gang3))
              goon-ff)


;; (pick-goon gang1 job1) consumes a Gang gang1 and a Job job1.
;;      The function produces the Goon that is most qualified for the job.
;;      If multiple goons are equally qualified, the function produces the
;;      first qualified one from your gang. If no goon is qualified, the
;;      function produces false.
;; pick-goon: Gang Job -> (anyof Goon false)
;; Examples
(check-expect (pick-goon my-gang job-banker) false)
(check-expect (pick-goon my-gang3 job-dancer) goon-ff)
(check-expect (pick-goon my-gang2 job-mule) goon-stb)

(define (pick-goon gang1 job1)
  (find-best-goon gang1 job1 (first gang1)))

;; Tests
;; None are qualified
(check-expect (pick-goon my-gang4 job-cook) false)

;; One is qualified
(check-expect (pick-goon my-gang job-artisan) goon-ms)

;; First gang member is not qualified

;; second gang member is not qualified

;; At least two best qualified individuals
(check-expect (pick-goon my-gang3 job-dancer) goon-ff)
(check-expect (pick-goon my-gang4 job-mule) goon-fl)

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 3 (c)
;; ***************************************************
;;

;; (find-difficult-jobs gang1 job-list1) consumes a Gang gang1
;;    and a Job-list job-list1. It produces a Job-list that contains
;;    all jobs that are too difficult for the current gang members to
;;    complete.
;; find-difficult-jobs: Gang Job-list -> Job-list
;; Examples
(check-expect (find-difficult-jobs my-gang my-jobs2)
              (list job-dancer job-cook job-banker))
(check-expect (find-difficult-jobs my-gang3 my-jobs2)
              (list job-cook job-banker))

(define (find-difficult-jobs gang1 job-list1)
  (cond [(empty? job-list1) empty]
        [(not (goon? (pick-goon gang1 (first job-list1))))
         (cons (first job-list1)
               (find-difficult-jobs gang1 (rest job-list1)))]
        [else (find-difficult-jobs gang1 (rest job-list1))]))

;; Tests
;; empty case
(check-expect (find-difficult-jobs my-gang2 empty) empty)
;; No difficult jobs
(check-expect (find-difficult-jobs my-gang3 my-jobs4) empty)

;; No difficult jobs, though some jobs only meet the minimum requirements
;;     for at least one of Loyalty, Wealth, Influence

;; One difficult job

;; At least two difficult jobs

;;
;; ***************************************************
;;   Quan Cheng Taian (20835721)
;;   CS 135 Fall 2019
;;   Assignment 06, Problem 3 (d)
;; ***************************************************
;;

;; (hire? gang1 job-list1 applicant) consumes a Gang gang1,
;;     a Job-list job-list1, and a Goon applicant. The
;;     function produces true if the applicant is qualified to
;;     perform a job that none of the current gang-members can,
;;     and false if every job is already covered by at least one
;;    gang-member
;; hire?: Gang Job-list Goon -> Bool
;; Examples

(check-expect (hire? my-gang my-jobs4 applicant-sfj) true)
(check-expect (hire? my-gang3 my-jobs applicant-eo) false)

 (define (hire? gang1 job-list1 applicant)
   (cond [(and (not (empty? (find-difficult-jobs gang1 job-list1)))
               (> (length (find-difficult-jobs gang1 job-list1))
                  (length
                   (find-difficult-jobs
                    (cons applicant gang1) job-list1)))) true]
         [else false]))

;; Tests
;; All jobs are difficult

;; Some jobs are difficult, but applicant doesn't qualify

;; Some jobs are difficult, but applicant qualifies

;; No jobs are difficult
(check-expect (hire? my-gang3 my-jobs applicant-eo) false)
(check-expect (hire? my-gang3 my-jobs applicant-sfj) false)
(check-expect (hire? gang-with-duplicates my-jobs5 applicant-sfj) false)


  
