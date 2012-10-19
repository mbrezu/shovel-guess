;;;; shovel-guess.lisp

(in-package #:shovel-guess)

(defparameter *sources* (list "
    var game = fn () {
      var secretNumber = floor(@random() * 100 + 1)
      var attempt = 0
      var iteration = fn () {
        attempt = attempt + 1
        @print('Attempt ' + string(attempt) + ' - ')
        @print('enter a number between 1 and 100: ')
        var guess = @readInt()
        if guess < secretNumber {
          @printLn('Too small!')
          iteration()
        }
        else if guess > secretNumber {
          @printLn('Too large!')
          iteration()
        }
        else {
          @printLn('You guessed it in ' + string(attempt) + ' attempts! Congratulations!')
          @print('Another game? (y/n) ')
          if @readChar() == 'y' game()
        }
      }
      iteration()
    }

    game()
    "))

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port 4242)))

(defstruct session
  id last-access-time
  vm-state vm-bytecode vm-sources
  page-content user-read)

(defvar *session* nil)

(defun web-print (content)
  (setf (session-page-content *session*)
        (with-output-to-string (str)
          (write-string (session-page-content *session*) str)
          (write-string "<span>" str)
          (write-string (hunchentoot:escape-for-html content) str)
          (write-string "</span>" str))))

(defun web-print-ln (content)
  (setf (session-page-content *session*)
        (with-output-to-string (str)
          (write-string (session-page-content *session*) str)
          (write-string "<span>" str)
          (write-string (hunchentoot:escape-for-html content) str)
          (write-string "</span><br/>" str))))

(defun web-read-char ()
  (case (session-user-read *session*)
    ((nil)
     (setf (session-user-read *session*) :char)
     (values :null :nap-and-retry-on-wake-up))
    (:char
     (setf (session-user-read *session*) nil)
     (string (elt (hunchentoot:get-parameter "input") 0)))))

(defun web-read-int ()
  (case (session-user-read *session*)
    ((nil)
     (setf (session-user-read *session*) :int)
     (values :null :nap-and-retry-on-wake-up))
    (:int
     (setf (session-user-read *session*) nil)
     (nth-value 0 (parse-integer (hunchentoot:get-parameter "input"))))))

(defun web-read-line ()
  (case (session-user-read *session*)
    ((nil)
     (setf (session-user-read *session*) :line)
     (values :null :nap-and-retry-on-wake-up))
    (:line
     (setf (session-user-read *session*) nil)
     (hunchentoot:get-parameter "input"))))

(defparameter *udps* (list (list "print" 'web-print 1)
                           (list "printLn" 'web-print-ln 1)
                           (list "readChar" 'web-read-char 0)
                           (list "readInt" 'web-read-int 0)
                           (list "readLine" 'web-read-line 0)
                           (list "random" (lambda () (random 1.0d0)) 0)))

(hunchentoot:define-easy-handler (whatever :uri "/whatever") ()
  (setf (hunchentoot:content-type*) "text/html")
  (alexandria:if-let (session-id (hunchentoot:get-parameter "sessionid"))
    (use-session session-id)
    (start-new-session)))

(defvar *sqlite-lock* (bt:make-lock "sqlite-lock"))

(defmacro within-sqlite-transaction (&body body)
  `(bt:with-lock-held (*sqlite-lock*)
     (sqlite:with-open-database (db *db-path*)
       (sqlite:with-transaction db
         ,@body))))

(defun make-sources (program)
  (list (shovel:make-source-file :name "your-program"
                                 :contents program)))

(defun start-new-session ()
  (alexandria:if-let (program (hunchentoot:post-parameter "program"))
    (multiple-value-bind (result error)
        (ignore-errors
          (shovel:get-bytecode (make-sources program)))
      (if result
          (create-and-run-session result program)
          (concatenate 'string
                       "<pre>"
                       (hunchentoot:escape-for-html
                        (with-output-to-string (str)
                          (print-object error str)))
                       "</pre>")))
    (with-output-to-string (str)
      (write-string "<form action='/whatever' method='post'>" str)
      (write-string "<textarea name='program' id='shovel-input' rows='30' cols='80'></textarea><br/>" str)
      (write-string "<input type='submit' value='Submit'/>" str)
      (write-string "</form>" str)
      (write-string "<script type='text/javascript'>
document.getElementById('shovel-input').focus()</script>"
                    str))))

(defun create-and-run-session (bytecode program)
  (let ((*session* (make-session :id nil
                                 :last-access-time nil
                                 :vm-state nil
                                 :vm-bytecode bytecode
                                 :vm-sources program
                                 :page-content ""
                                 :user-read nil)))
    (run-session)))

(defun run-session ()
  (handler-case
      (multiple-value-bind (result vm)
          (shovel:run-vm (session-vm-bytecode *session*)
                         :sources (make-sources (session-vm-sources *session*))
                         :user-primitives *udps*
                         :state (session-vm-state *session*))
        (declare (ignore result))
        (cond ((shovel:vm-execution-complete vm)
               (concatenate 'string
                            (session-page-content *session*)
                            "<p>Program completed.</p>"))
              (t
               (setf (session-vm-state *session*) (shovel:serialize-vm-state vm))
               (save-session)
               (generate-page))))
    (shovel:shovel-error (err)
      (concatenate 'string
                   "<pre>"
                   (hunchentoot:escape-for-html
                    (with-output-to-string (str)
                      (print-object err str)))
                   "</pre>"))))

(defun save-session ()
  (within-sqlite-transaction
    (let* ((id (sqlite:execute-single db "SELECT MAX(Id) + 1 FROM Sessions"))
           (bytecode (coerce (shovel:serialize-bytecode (session-vm-bytecode *session*))
                             '(simple-array (unsigned-byte 8) (*))))
           (state (coerce (session-vm-state *session*)
                          '(simple-array (unsigned-byte 8) (*))))
           (user-read-code (ecase (session-user-read *session*)
                             ((nil) 0)
                             (:int 1)
                             (:char 2)
                             (:line 3))))
      (setf (session-id *session*) id)
      (sqlite:execute-non-query db "INSERT INTO Sessions (Id, LastAccessTime,
    VmState, VmBytecode, VmSources, PageContent, UserRead) VALUES (?, ?, ?, ?, ?, ?, ?)"
                                id
                                (get-universal-time)
                                state
                                bytecode
                                (session-vm-sources *session*)
                                (session-page-content *session*)
                                user-read-code))))

(defun load-session (session-id)
  (within-sqlite-transaction
    (let ((session-items
           (first (sqlite:execute-to-list db "SELECT Id, LastAccessTime, VmState,
VmBytecode, VmSources, PageContent, UserRead FROM Sessions WHERE Id = ?" session-id))))
      (when session-items
        (make-session :id (elt session-items 0)
                      :last-access-time (elt session-items 1)
                      :vm-state (elt session-items 2)
                      :vm-bytecode (shovel:deserialize-bytecode (elt session-items 3))
                      :vm-sources (elt session-items 4)
                      :page-content (elt session-items 5)
                      :user-read (ecase (elt session-items 6)
                                   ((0) nil)
                                   ((1) :int)
                                   ((2) :char)
                                   ((3) :line)))))))

(defun use-session (session-id)
  (let ((*session* (load-session session-id)))
    (cond (*session*
           (alexandria:when-let (input (hunchentoot:get-parameter "input"))
             (setf (session-page-content *session*)
                   (concatenate 'string
                                (session-page-content *session*)
                                input
                                "<br/>")))
           (run-session))
          (t (start-new-session)))))

(defun generate-page ()
  (with-output-to-string (str)
    (write-string (session-page-content *session*) str)
    (write-string "<form action='/whatever' method='get'>" str)
    (write-string "<input type='text' name='input' id='shovel-input'/>" str)
    (format str "<input type='hidden' name='sessionid' value='~d' id='shovel-input'/>"
            (session-id *session*))
    (write-string "<input type='submit' value='Submit'/>" str)
    (write-string "</form>" str)
    (write-string "<script type='text/javascript'>
document.getElementById('shovel-input').focus()</script>"
                  str)))

(defparameter *db-path* (asdf:system-relative-pathname
                         :shovel-guess "state.db"))

(defun init-db ()
  (sqlite:with-open-database (db *db-path*)
    (sqlite:with-transaction db
      (sqlite:execute-non-query db "
      CREATE TABLE Sessions (Id INTEGER PRIMARY KEY,
                             LastAccessTime INTEGER,
                             VmState BINARY,
                             VmBytecode BINARY,
                             VmSources TEXT,
                             PageContent TEXT,
                             UserRead INT)"))))
