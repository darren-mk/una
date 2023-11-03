(in-package #:una)

#| env |#

(declaim (optimize safety))

(setf *print-case* :downcase)

(toggle-pretty-print-hash-table t)

#| global |#

(defparameter *key*
  (irc:ascii-string-to-byte-array "my-temp-secret"))

(defvar port-no
  3000)

(defparameter *tdb*
  (list))

#| type |#

(deftype http-result ()
  '(member :success :fail))

#| domain |#

(defclass consumer ()
  ((id :initarg :id :type small-number-array :accessor id-of)
   (fname :initarg :fname :type string :accessor fname-of)
   (lname :initarg :lname :type string :accessor lname-of)
   (email :initform "AAA" :type string :accessor email-of)
   (pcode :initarg :pcode :type string :accessor password-of)))

(defmethod alistify ((consumer consumer))
  (let ((id (id-of consumer))
        (fname (fname-of consumer)))
    `((:id . ,id) (:fname . ,fname))))

(defvar unauthorized-msg
  (list (cons :result :unauthorized)))

#| util |#

(defun tokenize (email)
  (jos:encode :hs256 *key*
              (list (cons :email email)
                    (cons :stamp (get-universal-time)))))

(defun authenticate (token)
  (handler-case (jos:decode :hs256 *key* token)
    (error () nil)))

(defun unsigned-bytes->alist (b)
  (with-input-from-string
      (s (bbl:octets-to-string b))
    (json:decode-json s)))

(defmacro w-resp-body (code)
  `(let ((body (unsigned-bytes->alist
                (hunchentoot:raw-post-data))))
     ,code))

(-> responsify (http-result string cons) *)
(defun responsify (result msg data)
  (jsn:encode-json-alist-to-string
   (list (cons :result result)
         (cons :msg msg)
         (cons :data data))))

#| middleware |#

(defun @json (next)
  (setf (hct:content-type*) "application/json")
  (funcall next)) 

(defun @auth (next)
  (let* ((headers (hunchentoot:headers-in*))
         (token (cdr (assoc :authorization headers)))
         (authorized? (authenticate token)))
    (if authorized?
        (funcall next)
        (responsify :fail "authorization failed."
                    (list (cons :auth "did not pass."))))))

#| route |#

(esr:defroute api-consumer-login
    ("/api/consumer/login" :method :post :decorators (@json)) ()
  (w-resp-body
   (let* ((email (am:->> body (assoc :email) cdr))
          (password (am:->> body (assoc :password) cdr)))
     (if (and (string= email "jacksparrow@pirate.com")
              (string= password "abc123!@#"))
         (responsify :success "matched" (list (cons :token (tokenize email))))
         (responsify :fail "not match" (list (cons :token "not generated.")))))))

#| control |#

(defvar app
  (make-instance 'easy-routes:routes-acceptor :port port-no))

(defun start ()
  (print "server started.")
  (hct:start app))

(defun stop ()
  (print "server stopped.")
  (hct:stop app))

(defun refresh ()
  (print "server resetting.")
  (hct:stop app)
  (hct:start app))
