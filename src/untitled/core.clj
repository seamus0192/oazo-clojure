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


(defn lookup
  [name env]
  (let [binding (some #(when (= name (:name %)) %) (:bindings env))]
    (if binding
      (:value binding)
      (throw (Exception. "Name not found")))))

(defn apply-primop
  [op args]
  (case op
    '+ (reduce + (map :num args))
    '- (reduce - (map :num args))
    ; Add on here
    ))

(defn interp
  "interp"
  [expr env]
  (match [expr env]
         [{:numC a} _] (struct NumV a)
         [{:strC s} _] (struct StrV s)
         [{:idC i} e] (lookup i e)
         :else "couldn't match")
  ; add more operations
  )