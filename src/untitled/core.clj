(ns untitled.core)
(require '[clojure.core.match :refer [match]])

(defstruct NumC :numC)
(defstruct IdC :IdC)
(defstruct AppC :func :args)
(defstruct StrC :strC)
(defstruct LamC :args :body)
(defstruct IfC :test :then :else)
(def ExprC [NumC IdC AppC StrC LamC IfC])

(defstruct CloV :args :body :env)
(defstruct NumV :num)
(defstruct BoolV :bool)
(defstruct PrimV :op)
(defstruct StrV :str)
(def Value [CloV NumV BoolV PrimV StrV])

(defstruct person :name :age :height)

(defstruct Binding :name :value)
(defstruct Env :bindings)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn interp
  "interp"
  [expr]
  (match expr
         {:numC a} (struct NumV a)
         {:strC s} (struct StrV s)
         :else "couldn't match")
  )


