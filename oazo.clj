;clojure is not a typed language

;define datatypes
(defstruct IdC :sym )
(defstruct AppC :func :args)
(defstruct NumC :num)
(defstruct LamC :args :body)
(defstruct IfC :if :then :else)
(defstruct StrC :str)

;define values
(defstruct PrimV :binop)
(defstruct RealV :real)
(defstruct BoolV :bool)
(defstruct StrV :str)
(defstruct CloV :args :body :env)

;define env
(defstruct bind :name :val)
