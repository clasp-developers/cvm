(in-package #:cvm/machine)

(defun dis-signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defun bc-unsigned (bytecode ip nbytes)
  ;; Read NBYTES of little-endian integer.
  (do* ((i 0 (1+ i))
        (s 0 (+ 8 s))
        (sum 0))
       ((= i nbytes) sum)
    (incf sum (ash (aref bytecode (+ ip i)) s))))

(defun bc-signed (bytecode ip nbytes)
  (dis-signed (bc-unsigned bytecode ip nbytes)
              (* 8 nbytes)))

;;; Return the instruction description for OPCODE.
(defun decode-instr (opcode)
  (let ((res (member opcode *full-codes* :key #'second)))
    (if res (first res) (error "unknown bytecode opcode ~d" opcode))))

;;; Return a list of all IPs that are jumped to.
(defun gather-labels (bytecode ip length)
  (let ((end (+ ip length))
        (result ())
        (longp ())
        op)
    (loop (setq op (decode-instr (aref bytecode ip)))
          ;; Go through the arguments, identifying any labels.
          (let ((opip ip)) ; IP of the start of the instruction
            (incf ip)
            (dolist (argi (if longp (fourth op) (third op)))
              (let ((nbytes (logandc2 argi +mask-arg+)))
                (if (label-arg-p argi)
                    (cl:push (+ opip (bc-signed bytecode ip nbytes)) result))
                (incf ip nbytes))))
          ;; If this is a LONG, set that for the next instruction.
          ;; (KLUDGE)
          ;; Otherwise reset longp to false.
          (setq longp (cl:eq (first op) 'long))
          (if (>= ip end) (cl:return (sort result #'<))))))

(defun disassemble-instruction (bytecode ip &key (labels () labelsp))
  (let ((desc (decode-instr (aref bytecode ip)))
        (longp ()) (opip ip))
    (when (cl:eq (first desc) 'long)
      (setf longp t desc (decode-instr (aref bytecode (incf opip)))))
    (setf ip (1+ opip))
    (loop for argi in (if longp (fourth desc) (third desc))
          for nbytes = (logandc2 argi +mask-arg+)
          collect (cond ((constant-arg-p argi)
                         (list :constant
                               (bc-unsigned bytecode ip nbytes)))
                        ((label-arg-p argi)
                         (let* ((lip (+ opip (bc-signed bytecode ip nbytes)))
                                (lpos (position lip labels)))
                           (cond (labelsp
                                  (assert lpos)
                                  (list :label lpos))
                                 (t (list :label lip)))))
                        ((keys-arg-p argi)
                         (list :keys
                               (bc-unsigned bytecode ip nbytes)))
                        (t
                         (list :operand
                               (bc-unsigned bytecode ip nbytes))))
            into args
          do (incf ip nbytes)
          finally (cl:return (values (list* (first desc) longp args) ip)))))

(defun %disassemble-bytecode (bytecode)
  (let* ((length (length bytecode))
         (labels (gather-labels bytecode 0 length))
         (ip 0))
    (loop ;; If this is a label position, mark that.
          for labelpos = (position ip labels)
          when labelpos
            collect (write-to-string labelpos)
          ;; Decode.
          collect (multiple-value-bind (inst new-ip)
                      (disassemble-instruction bytecode ip :labels labels)
                    (setf ip new-ip)
                    inst)
          until (>= ip length))))

(defun disassemble-bytecode (bytecode literals)
  (let ((dis (%disassemble-bytecode bytecode)))
    (flet ((textify-operand (thing)
             (destructuring-bind (kind value) thing
               (cond ((cl:eq kind :constant) (format () "'~s" (aref literals value)))
                     ((cl:eq kind :label) (format () "L~a" value))
                     ((cl:eq kind :operand) (format () "~d" value))
                     ;; :keys special cased below
                     (t (error "Illegal kind ~a" kind))))))
      (format t "~&---module---~%")
      (dolist (item dis)
        (cond
          ((consp item)
           ;; instruction
           (destructuring-bind (name longp . args) item
             (if (string= name "parse-key-args")
                 ;; We special case this despite the keys-arg thing because it's
                 ;; just pretty weird all around.
                 (let* ((more-start (second (first args)))
                        (kci (second (second args)))
                        (aokp (logbitp (if longp 15 7) kci))
                        (key-count (logand kci (if longp #x7fff #x7f)))
                        (keystart (second (third args)))
                        (keys cl:nil)
                        (framestart (second (fourth args))))
                   ;; Gather the keys
                   (do ((i 0 (1+ i)))
                       ((= i key-count) (setq keys (nreverse keys)))
                     (cl:push (aref literals (+ keystart i)) keys))
                   ;; Print
                   (format t "~&  ~:[~;long ~]~(~a~)~:[~;-aok~] ~d ~d '~s ~d"
                           longp name aokp more-start key-count keys framestart))
                 ;; Normal case
                 (format t "~&  ~:[~;long ~]~(~a~)~{ ~a~}~%"
                         longp name (mapcar #'textify-operand args)))))
          ((or (stringp item) (symbolp item))
           ;; label
           (format t "~&L~a:~%" item))
          (t (error "Illegal item ~a" item))))))
  (values))

(defgeneric disassemble (object))

(defmethod disassemble ((object bytecode-module))
  (disassemble-bytecode (bytecode-module-bytecode object)
                        (bytecode-module-literals object)))

;; TODO: Record function boundaries, so that among other things we can
;; disassemble only the region for the function being disassembled.
(defmethod disassemble ((object bytecode-function))
  (disassemble (bytecode-function-module object)))

(defmethod disassemble ((object bytecode-closure))
  (disassemble (bytecode-closure-template object)))