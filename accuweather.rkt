#lang racket

(require net/http-client json)
(require test-engine/racket-tests)

(define my-key "Jd75GWnbDfQm1RKaHfAPACinOKNGMBTa")

(define host "dataservice.accuweather.com")

(define uri "/locations/v1/cities/search")

#;(define-values (status header response)
    (http-sendrecv host (string-append uri "?apikey=" api-key "&q=" city)
                   #:ssl? #t))

(define (traverse-list-for-key json-list)
  (cond [(empty? json-list) "Poorly formed JSON"]
        [#t (cond [(hash-has-key? (car json-list) 'Key) (hash-ref
                                                         (car json-list)
                                                         'Key)]
                  [#t (traverse-list-for-key (rest json-list))])]))

;; (or/c #f (listof ....
;;[define testing-mode? [make-parameter #f]]

(define (call-send-recv endpoint)
  (http-sendrecv host endpoint #:ssl? #t))

;;  (cond
;;    [(in-testing-mode?)
;; look into test testing0-mode and let that have information that tells me
;; what to put into the result ports
;;     (testing-mode-call-send-recv end-point)]
;;    [else


(define (extract-location-key response)
  (let ([my-json (read-json response)])
    (cond [(empty? my-json) my-json]
          [#t (traverse-list-for-key my-json)])))

(define (check-status status response function)
  (cond [(bytes=? status #"HTTP/1.1 200 OK") (function response)]
        [else status]))

(define (get-location-key city api-key)
  (define-values (status header response)
    (call-send-recv (string-append "/locations/v1/cities/search?apikey="
                                   api-key "&q=" city)))
  (check-status status response extract-location-key))

(define (extract-current-temperature response)
  (let ([my-json (read-json response)])
    (cond [(empty? my-json) "Poorly formed JSON."]
          [else (hash-ref (hash-ref
                           (hash-ref (car my-json) 'Temperature)
                           'Metric) 'Value)])))

(define (get-current-temperature city)
  (define-values (status header response)
    (call-send-recv (string-append "/currentconditions/v1/"
                                   (get-location-key city my-key) "?apikey="
                                   my-key)))
  (check-status status response extract-current-temperature))

(define (which-is-warmer city1 city2)
  (let ([city1-temp (get-current-temperature city1)]
        [city2-temp (get-current-temperature city2)])
    (cond [(> city1-temp city2-temp) (string-append city1
                                                    " is warmer at "
                                                    (number->string city1-temp)
                                                    "C")]
          [else (string-append city2
                               " is warmer at "
                               (number->string city2-temp)
                               "C")])))

(define (get-temperature-forecast city)
  (define-values (status header response)
    (call-send-recv (string-append "/currentconditions/v1/"
                                   (get-location-key city my-key) "?apikey="
                                   my-key)))
  (check-status status response extract-current-temperature))

(define (get-5-day-forecast city)
  (define-values (status header response)
    (call-send-recv (string-append "/forecasts/v1/daily/5day/"
                                   (get-location-key city my-key) "?apikey="
                                   my-key)))
  (check-status status response extract-5day-forecast))

(define (extract-5day-forecast response)
  (do ([my-forecasts (hash-ref (read-json response) 'DailyForecasts)
                     (cdr my-forecasts)]
       [date-temperature-triplets '()
                                  (cons (list (hash-ref (car my-forecasts)
                                                        'Date)
                                              (hash-ref
                                               (hash-ref
                                                (hash-ref (car my-forecasts)
                                                          'Temperature)
                                                'Minimum)
                                               'Value)
                                              (hash-ref
                                               (hash-ref
                                                (hash-ref (car my-forecasts)
                                                          'Temperature)
                                                'Maximum)
                                               'Value))
                                        date-temperature-triplets)])
    ((empty? my-forecasts) (reverse date-temperature-triplets))))

(define (extract-dog-index response)
  (let ([my-json (read-json response)])
    (list (hash-ref (car my-json) 'Value)
          (hash-ref (car my-json) 'Category))))

(define (get-dog-walking-comfort city)
  (define-values (status header response)
    (call-send-recv (string-append "/indices/v1/daily/1day/"
                                   (get-location-key city my-key) "/43?apikey="
                                   my-key)))
  (check-status status response extract-dog-index))
                                                                   


#; (define (better-weather city1 city2)
     (do ([city1-forecast (get-5-day-forecast city1) (cdr city1-forecast)]
          [city2-forecast (get-5-day-forecast city2) (cdr city2-forecast)]
          [better-weather-list '() (cons (car (car city1-forecast)) (if (> (cadr city1-forecast))))])))
        
 

#;(module+ test
    (require rackunit)
    (parameterize ([testing-mode? (hash "endpoint1" #"fjdlafjkdl")])
      [check-equal? (get-temp... "Chicago" "Mumbai")
                    "Mumbai is warmer at 20.0C"]))