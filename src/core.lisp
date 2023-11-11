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

#| type |#

(deftype http-result ()
  '(member :success :fail))

#| util |#

(defun now ()
  (get-universal-time))

(defun agetv (al k)
  (am:->> al (assoc k) cdr))

#| database |#

(defun connect-db ()
  (pmd:connect-toplevel
   "unadb" "darren" "" "localhost"))

(defun disconnect-db ()
  (pmd:disconnect-toplevel))

(defun get-uuid ()
  "todo: this should be replaced by cl native function."
  (am:->> "select uuid_generate_v4()"
    pmd:query car car))

#| domain |#

(defclass common ()
  ((id :col-type string
       :initarg :id
       :reader id))
  (:metaclass pmd:dao-class))

(defclass writer (common)
  ((first-name :col-type string
               :type string
               :initarg :first-name
               :initform (error "first-name not supplied")
               :reader first-name))
  (:metaclass pmd:dao-class)
  (:keys id))

(srp:comment
  (let ((w (make-instance 'writer
                          :first-name 12)))
  (pmd:insert-dao w))

  (pmd:query
   (:select '*
    :from 'writer
    :where (:= 'first-name "Oren"))
   :alist)

  (defmethod alistify ((consumer consumer))
  (let ((id (id-of consumer))
        (fname (fname-of consumer)))
    `((:id . ,id) (:fname . ,fname)))))

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
     (print body)
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

(esr:defroute
    echo ("/api/echo" :method :get) (word)
  word)

(esr:defroute
    post-writer ("/api/writer" :method :post) ()
  (w-resp-body
   (let* ((first-name (agetv body :first-name))
          (w (make-instance 'writer :first-name first-name)))
     (pmd:insert-dao w)
     (responsify :success "posted" (list (cons :data "good"))))))

(esr:defroute
    api-consumer-login
    ("/api/consumer/login" :method :post) ()
  (w-resp-body
   (let* ((email (am:->> body (assoc :email) cdr))
          (password (am:->> body (assoc :password) cdr)))
     (if (and (string= email "jacksparrow@pirate.com")
              (string= password "abc123!@#"))
         (responsify :success "matched" (list (cons :token (tokenize email))))
         (responsify :fail "not match" (list (cons :token "not generated.")))))))

#| control |#

(defvar app
  (make-instance
   'easy-routes:routes-acceptor
   :port port-no))

(defun start ()
  (connect-db)
  (print "# db connected.")
  (hct:start app)
  (print "# server started."))

(defun stop ()
  (disconnect-db)
  (print "# db disconnected.")
  (hct:stop app)
  (print "# server stopped."))

(defun refresh ()
  (print "# server restarted.")
  (hct:stop app)
  (hct:start app))
