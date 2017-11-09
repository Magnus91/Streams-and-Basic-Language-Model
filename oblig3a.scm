(load "prekode3a.scm")

;; Oblig 3a
;; Magnus Remmem & Jonatan Hafell Budalen

;; OBS: Har lagt til trening av trær for oppgave (2e) nederst i programmet, og
;; vil derfor ta litt tid å runne programmet ferdig. Kan eventuelt kommenteres
;; ut før retting av siste oppgave.

;; (1a)

(define (list-to-stream lst)
  (let ((stream
         (if (null? lst)
             the-empty-stream
             (cons-stream
              (car lst)
              (list-to-stream (cdr lst))))))
    stream))

(define (stream-to-list stream . args)
  (let ((lst
         (if (stream-null? stream)
             '()
             (cond ((null? args)
                    (cons (stream-car stream) (stream-to-list (stream-cdr stream))))
                   ((> (car args) 0)
                    (cons (stream-car stream) (stream-to-list (stream-cdr stream) (- (car args) 1))))
                   (else '())))))
    lst))

;; (1b)

;; test streams
(define stream1 (list-to-stream '(1 2 3)))
(define stream2 (list-to-stream '(4 5 6 7)))

(define (stream-map proc . argstreams)
  (if (equal? #t (streams-null? argstreams))
      the-empty-stream
      (cons-stream
       (apply proc
              (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (streams-null? . argstreams)
  (if (equal? '(()) argstreams)
      #f
      (if (stream-null? (caar argstreams))
          #t
          (streams-null? (cdar argstreams)))))

;; Tester kjøring av stream-map med to streams med ulike lengder
(define mapped-stream (stream-map * stream1 stream2))
mapped-stream
(show-stream mapped-stream)

;; (1c)

;; Problemet er at han ikke har tatt hensyn til uendelige strømmer, da det aldri vil bli ferdig.

;; (1d)

(define (show x)
  (display x)
  (newline)
  x)

(define x
  (stream-map show
              (stream-interval 0 10)))

;; (define x (stream-map show (stream-interval 0 10))) gir 0
;; (stream-ref x 5) gir 1 2 3 4 5 5
;; (stream-ref x 7) gir 6 7 7
;; stream-interval delayer tallene mellom 0 og 10 med cons-stream og stream-ref x 5 forcer ut
;; tallene som har blitt delayed opp til 5 med stream-cdr, stream-ref x 7 tar tallene opp til 7, men
;; ikke 0 1 2 3 4 5 fordi de er allerede blitt forced ut.

;; (2a)
;; Implementasjon med binære trær ligger i (2e).

(define (make-lm)
  (list '*language-model*))


;; Brukte den innebygde assoc-prosedyren på string2, men lagde en assoc2
;; til string1 siden den inneholder en liste med selve stringen og en counter
;; med antall forekomster av ordet.
(define (lm-lookup-bigram lm string1 string2)
  (let ((word
         (assoc2 string1 (cdr lm))))
    (if word
        (let ((record
               (assoc string2 (cdr word))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (lm-lookup-word-count lm string)
  (cdar (assoc2 string (cdr lm))))

;; Brukte insert! fra 3.3.3 i SICP som inspirasjon,
;; men måtte endre litt for å kunne øke antall
;; forekomster av ord og par.
(define (lm-record-bigram! lm string1 string2)
  (let ((word (assoc2 string1 (cdr lm))))
    (if word
        (let ((record (assoc string2 (cdr word))))
          (set-cdr! (car word) (+ (cdar word) 1))
          (if record
              (set-cdr! record (+ (cdr record) 1))
              (set-cdr! word
                        (cons (cons string2 1)
                              (cdr word)))))
        (set-cdr! lm
                 (cons (list (cons string1 1)
                             (cons string2 1))
                       (cdr lm))))))

(define (assoc2 key records)
  (cond ((null? records) #f)
        ((equal? key (caaar records)) (car records))
        (else (assoc2 key (cdr records)))))

;; (2b)

(define brown (read-corpus "brown.txt"))

;; Går gjennom hver setning i en tekst og bruker
;; lm-record-bigram! for å registrere ord og par.
(define (lm-train! lm text)
  (define (traverse-sentence sentence)
    (if (null? (cdr sentence))
        (lm-train! lm (cdr text))
        (let ((string1 (car sentence))
              (string2 (cadr sentence)))
          (lm-record-bigram! lm string1 string2)
          (traverse-sentence (cdr sentence)))))
  (if (null? text)
      '"text done"
      (traverse-sentence (car text))))

;; (2c)
;; Valge å lage en destruktiv variant av denne prosedyren og er
;; dermed viktig å kalle prosedyrene i riktig rekkefølge.
(define (lm-estimate! lm)
  (define (estimate pairing count)
    (if (null? pairing)
        (lm-estimate! (cdr lm))
        (let ((estimation (/ (cdar pairing) count)))
          (set-cdr! (car pairing) estimation)
          (estimate (cdr pairing) count))))
  (if (null? (cdr lm))
      'done
      (estimate (cdadr lm) (cdaadr lm)))) ;; Litt spesielle car/cdr kall, men dette er for å få tak i de riktige elementene i datastrukturen.

;; (2d)

(define test-file (read-corpus "test.txt"))

;; Vi vet at denne inkluderer <s> og </s>, men det spiller liten rolle i forhold til "fall-back".
(define (lm-fall-back lm) 
  (define (count lm sum)
    (if (null? (cdr lm))
        (/ 1 sum)
        (count (cdr lm) (+ sum (cdaadr lm)))))
  (count lm 0))

;; Returnerer score til en setning beregnet av språkmodellen. Bruker lm-fall-back
;; Om ordpar ikke finnes i modellen.
(define (lm-score lm sentence)
  (define (calculate sentence total-score)
    (if (null? (cdr sentence))
        total-score
        (let ((score (lm-lookup-bigram lm (car sentence) (cadr sentence))))
          (if score
              (calculate (cdr sentence) (* total-score score))
              (calculate (cdr sentence) (* total-score (lm-fall-back lm)))))))
  (calculate sentence 1))

;; Printer resultatene for alle setninger i en tekst.
(define (lm-scores lm text)
  (cond ((null? text)
         'done)
        (else
         (display (lm-score lm (car text)))
         (newline)
         (lm-scores lm (cdr text)))))

;; Størst sjanse for setning nr 10 og minst sjanse for setning nr 9.

;; (2e)

(define wsj (read-corpus "wsj.txt"))
(define test (read-corpus "test.txt"))

;; Diverse prosedyrer for binærtre-strukturen.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (instances tree) (cadddr tree))
(define (pairs tree) (car (cddddr tree)))
(define (make-lm-tree entry left right instances pairs)
  (list entry left right instances pairs))

;; Implementert hvert enkeltord i et hovedtre, der hvert ord inneholder counter med
;; med antall forekomster og et eget "subtre" (eget par-tre) som inneholder sine
;; tilhørende par-ord og antall forekomster. Bruker en og samme lm-tree-insert!
;; prosedyre for hovedtre og par-trær og sjekker dette med en if-check.
;;
;; Resten av prosedyrene under er reviderte versjoner av hjelpeprosedyrene for lm
;; tilpasset binærtrevarianten.
;;
;; Lite problem vi har med strukturen vår nå er at man må opprette et tre med
;; lm-tree-insert! hvor man legger inn et vilkårlig ordpar. Hadde planer
;; om å fikse dette, men fikk brått dårlig tid før innlevering.
;; Eksempelkall under:
;; (define test-tree (lm-tree-insert! "hey" "there" '()))

(define (lm-tree-insert! string1 string2 tree)
  (cond ((null? tree)
         (make-lm-tree string1 '() '() 1 (make-lm-tree string2 '() '() 1 '())))

        ((string=? string1 (entry tree))
         (set-car! (cdddr tree) (+ 1 (instances tree))) ;;
         (if (not (null? string2))
             (lm-tree-insert! string2 '() (pairs tree)))) ;; string2 er null når man jobber med trærne for par

        ((string<? string1 (entry tree))
         (if (null? (left-branch tree))
             (if (null? string2)
                 (set-car! (cdr tree) (make-lm-tree string1 '() '() 1 '()))
                 (set-car! (cdr tree) (make-lm-tree string1 '() '() 1 (make-lm-tree string2 '() '() 1 '()))))
             (lm-tree-insert! string1 string2 (left-branch tree))))

        ((string>? string1 (entry tree))
          (if (null? (right-branch tree))
              (if (null? string2)
                 (set-car! (cddr tree) (make-lm-tree string1 '() '() 1 '()))
                 (set-car! (cddr tree) (make-lm-tree string1 '() '() 1 (make-lm-tree string2 '() '() 1 '()))))
              (lm-tree-insert! string1 string2 (right-branch tree))))))

(define (lm-tree-lookup string1 string2 tree)
  (cond ((null? tree) #f)

        ((string=? string1 (entry tree))
         (if (null? string2)
             (instances tree)
             (lm-tree-lookup string2 '() (pairs tree))))

        ((string<? string1 (entry tree))
         (lm-tree-lookup string1 string2 (left-branch tree)))

        ((string>? string1 (entry tree))
         (lm-tree-lookup string1 string2 (right-branch tree)))))

(define (lm-tree-train! tree text)
  (define (traverse-sentence sentence)
    (if (null? (cdr sentence))
        (lm-tree-train! tree (cdr text))
        (let ((string1 (car sentence))
              (string2 (cadr sentence)))
          (lm-tree-insert! string1 string2 tree)
          (traverse-sentence (cdr sentence)))))
  (if (null? text)
      '"text done"
      (traverse-sentence (car text))))

(define (lm-tree-estimate! tree)
  (define (estimate pair-tree count)
    (cond ((not (null? pair-tree))
           (set-car! (cdddr pair-tree) (/ (instances pair-tree) count))
           (estimate (left-branch pair-tree) count)
           (estimate (right-branch pair-tree) count))))
  (cond ((not (null? tree))
         (estimate (pairs tree) (instances tree))
         (lm-tree-estimate! (left-branch tree))
         (lm-tree-estimate! (right-branch tree)))))

(define (lm-tree-fall-back tree) 
  (define (count tree sum)
    (if (null? tree)
        0
        (+ (instances tree) (count (left-branch tree) sum) (count (right-branch tree) sum))))
  (let ((sum (count tree 0)))
    (/ 1 sum)))
         
(define (lm-tree-score tree sentence)
  (define (calculate sentence total-score)
    (if (null? (cdr sentence))
        total-score
        (let ((score (lm-tree-lookup (car sentence) (cadr sentence) tree))
              (fall-back (lm-tree-fall-back tree)))
          (if score
              (calculate (cdr sentence) (* total-score score))
              (calculate (cdr sentence) (* total-score fall-back))))))
  (calculate sentence 1))

(define (lm-tree-scores tree text)
  (cond ((null? text)
         'done)
        (else
         (display (lm-tree-score tree (car text)))
         (newline)
         (lm-tree-scores tree (cdr text)))))

;; Testkjøring for trening med wsj.txt og wsj.txt + brown.txt

(define wsj-tree (lm-tree-insert! "hey" "there" '()))
(display "Training wsj-tree with wsj.txt...\n")
(lm-tree-train! wsj-tree wsj)
(display "Estimating scores for wsj-tree...\n")
(lm-tree-estimate! wsj-tree)
(display "estimation done!\n")
(display "Scores for wsj-tree with test.txt:\n")
(lm-tree-scores wsj-tree test)

(define wsj-brown-tree (lm-tree-insert! "hey" "there" '()))
(display "Training wsj-brown-tree with wsj.txt...\n")
(lm-tree-train! wsj-brown-tree wsj)
(display "Training wsj-brown-tree with brown.txt...\n")
(lm-tree-train! wsj-brown-tree brown)
(display "Estimating scores for wsj-brown-tree...\n")
(lm-tree-estimate! wsj-brown-tree)
(display "estimation done!\n")
(display "Scores for wsj-brown-tree with test.txt:\n")
(lm-tree-scores wsj-brown-tree test)

;; Det er en tydelig forskjell i resultatene til wsj-tree og wsj-brown-tree.