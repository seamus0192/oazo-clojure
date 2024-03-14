(ns untitled.core)
(require '[clojure.core.match :refer [match]])

(defstruct NumC :numC)
(defstruct IdC :idC)
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
(struct Binding 'a 10)
(defstruct Env :bindings)

(def top-level-bindings
  (list (struct Binding '+ (struct PrimV '+))
        (struct Binding '- (struct PrimV '-))
        (struct Binding '* (struct PrimV '*))
        (struct Binding '/ (struct PrimV '/))
        (struct Binding '<= (struct PrimV '<=))
        (struct Binding 'equal? (struct PrimV 'are-equal?))
        (struct Binding 'true (struct BoolV true))
        (struct Binding 'false (struct BoolV false))
        (struct Binding 'error (struct PrimV 'user-error))))


(defn lookup [symbol bindings]
  (let [binding (first (filter #(= (:name %) symbol) bindings))]
    (if binding
      (:value binding)
      nil)))

(defn apply-primop
  [op args]
  (match op
         '+ (reduce + (map :num args))
         '- (reduce - (map :num args)))
  ; Add on here
  )

(defn appC-helper [f-val arg-vals]
  (match f-val
         {:op op} (apply-primop op arg-vals)))

(defn interp
  "interp"
  [expr env]
  (match [expr env]
         [{:numC a} _] (struct NumV a)
         [{:strC s} _] (struct StrV s)
         [{:args f :body b} e] (struct CloV f b e)
         [{:idC i} e] (lookup i e)
         [{:func func :args args} e] (appC-helper (interp func e) (map #(interp % e) args))
         :else "couldn't match")
  ; add more operations
  )
