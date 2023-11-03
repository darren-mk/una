(in-package #:una)

(fvm:def-suite core-test
  :description "testing una core")

(fvm:in-suite core-test)

(fvm:test w-resp-body-test
  (fvm:is (equal (macroexpand '(w-resp-body (print "hi!")))
                 '(let ((body (unsigned-bytes->alist
                               (hct:raw-post-data))))
                   (print "hi!")))))

(fvm:test responsify-test
  (let ((expected (concatenate 'string
                               "{\"result\":\"success\","
                               "\"msg\":\"good job!\","
                               "\"data\":{\"age\":100}}"))
        (result (responsify :success "good job!"
                            (list (cons "age" 100)))))
    (fvm:is (string= expected result))))

(fvm:run! 'core-test)
