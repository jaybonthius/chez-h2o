(library (h2o dates)
  (export time->http-date http-date->time)
  (import (chezscheme))

  (define day-names '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  (define long-day-names '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
  (define month-names '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

  ;; Chez's date procedures expect years from 1970; these (Howard
  ;; Hinnant's algorithms) count days from 1970-01-01 for any Gregorian
  ;; date.
  (define (days-from-civil year month day)
    (let* ([y (if (<= month 2) (- year 1) year)]
           [era (div y 400)]
           [year-of-era (- y (* era 400))]
           [day-of-year (+ (quotient (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) (- day 1))]
           [day-of-era (+ (* year-of-era 365) (quotient year-of-era 4) (- (quotient year-of-era 100))
                          day-of-year)])
      (+ (* era 146097) day-of-era -719468)))

  (define (civil-from-days days)
    (let* ([z (+ days 719468)]
           [era (div z 146097)]
           [day-of-era (- z (* era 146097))]
           [year-of-era (quotient (- day-of-era (quotient day-of-era 1460)
                                     (- (quotient day-of-era 36524)) (quotient day-of-era 146096))
                                  365)]
           [day-of-year (- day-of-era (+ (* 365 year-of-era) (quotient year-of-era 4)
                                         (- (quotient year-of-era 100))))]
           [mp (quotient (+ (* 5 day-of-year) 2) 153)]
           [day (+ (- day-of-year (quotient (+ (* 153 mp) 2) 5)) 1)]
           [month (if (< mp 10) (+ mp 3) (- mp 9))])
      (values (+ year-of-era (* era 400) (if (<= month 2) 1 0)) month day)))

  (define (two-digits n)
    (if (< n 10) (format "0~a" n) (number->string n)))

  (define (time->http-date time)
    (unless (and (time? time) (eq? (time-type time) 'time-utc))
      (assertion-violationf 'time->http-date "~s is not a time-utc time" time))
    (let* ([seconds (time-second time)]
           [days (div seconds 86400)]
           [of-day (mod seconds 86400)])
      (let-values ([(year month day) (civil-from-days days)])
        (format "~a, ~a ~a ~a ~a:~a:~a GMT"
          (vector-ref day-names (mod (+ days 3) 7))
          (two-digits day) (vector-ref month-names (- month 1)) (format "~4,'0d" year)
          (two-digits (quotient of-day 3600)) (two-digits (quotient (remainder of-day 3600) 60))
          (two-digits (remainder of-day 60))))))

  (define (leap-year? year)
    (and (= 0 (remainder year 4)) (or (not (= 0 (remainder year 100))) (= 0 (remainder year 400)))))

  (define (days-in-month year month)
    (case month
      [(2) (if (leap-year? year) 29 28)]
      [(4 6 9 11) 30]
      [else 31]))

  (define (index-of names s)
    (let loop ([i 0])
      (cond
        [(= i (vector-length names)) #f]
        [(string=? (vector-ref names i) s) i]
        [else (loop (+ i 1))])))

  (define (scan text pattern)
    (let loop ([pattern pattern] [i 0] [fields '()])
      (cond
        [(null? pattern) (and (= i (string-length text)) (reverse fields))]
        [(char? (car pattern))
         (and (< i (string-length text)) (char=? (string-ref text i) (car pattern))
              (loop (cdr pattern) (+ i 1) fields))]
        [else
         (let-values ([(value end) ((car pattern) text i)])
           (and value (loop (cdr pattern) end (cons value fields))))])))

  (define (digits count)
    (lambda (text i)
      (let ([end (+ i count)])
        (if (and (<= end (string-length text))
                 (let check ([j i]) (or (= j end) (and (char<=? #\0 (string-ref text j) #\9) (check (+ j 1))))))
            (values (string->number (substring text i end)) end)
            (values #f i)))))

  (define (space-padded-day text i)
    (if (and (< (+ i 1) (string-length text)) (char=? (string-ref text i) #\space))
        ((digits 1) text (+ i 1))
        ((digits 2) text i)))

  (define (word names)
    (lambda (text i)
      (let loop ([end i])
        (if (and (< end (string-length text)) (char-alphabetic? (string-ref text end)))
            (loop (+ end 1))
            (let ([index (index-of names (substring text i end))])
              (if index (values index end) (values #f i)))))))

  (define (string-pattern s) (string->list s))

  (define imf-fixdate
    `(,(word day-names) #\, #\space ,(digits 2) #\space ,(word month-names) #\space ,(digits 4)
      #\space ,(digits 2) #\: ,(digits 2) #\: ,(digits 2) ,@(string-pattern " GMT")))

  (define rfc850-date
    `(,(word long-day-names) #\, #\space ,(digits 2) #\- ,(word month-names) #\- ,(digits 2)
      #\space ,(digits 2) #\: ,(digits 2) #\: ,(digits 2) ,@(string-pattern " GMT")))

  (define asctime-date
    `(,(word day-names) #\space ,(word month-names) #\space ,space-padded-day
      #\space ,(digits 2) #\: ,(digits 2) #\: ,(digits 2) #\space ,(digits 4)))

  ;; RFC 9110 5.6.7: a two-digit year more than 50 years in the future is
  ;; the most recent past year with those digits.
  (define (full-year two-digit-year)
    (let* ([now (date-year (time-utc->date (current-time 'time-utc) 0))]
           [year (+ (- now (modulo now 100)) two-digit-year)])
      (if (> year (+ now 50)) (- year 100) year)))

  (define (fields->time year month day hour minute second)
    (and (<= 1 day (days-in-month year month)) (< hour 24) (< minute 60) (< second 61)
         (make-time 'time-utc 0
           (+ (* (days-from-civil year month day) 86400) (* hour 3600) (* minute 60) second))))

  (define (http-date->time text)
    (unless (string? text) (assertion-violationf 'http-date->time "~s is not a string" text))
    (cond
      [(scan text imf-fixdate)
       => (lambda (f)
            (apply (lambda (weekday day month year hour minute second)
                     (fields->time year (+ month 1) day hour minute second))
              f))]
      [(scan text rfc850-date)
       => (lambda (f)
            (apply (lambda (weekday day month year hour minute second)
                     (fields->time (full-year year) (+ month 1) day hour minute second))
              f))]
      [(scan text asctime-date)
       => (lambda (f)
            (apply (lambda (weekday month day hour minute second year)
                     (fields->time year (+ month 1) day hour minute second))
              f))]
      [else #f])))
