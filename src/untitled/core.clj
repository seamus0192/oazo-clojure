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
        (struct Binding 'trueV (struct BoolV true))
        (struct Binding 'falseV (struct BoolV false))
        (struct Binding 'error (struct PrimV 'user-error))))


(defn lookup [symbol bindings]
  (let [binding (first (filter #(= (:name %) symbol) bindings))]
    (if binding
      (:value binding)
      nil)))

(defn apply-primop
  [op args]
  (match op
         '+ (NumV (reduce + 0 (map :num args)))
         '- (NumV (reduce - (:num (first args)) (map :num (rest args))))
         '* (NumV (reduce * 1 (map :num args)))
         '/ (NumV (reduce (fn [x y] (/ y x))
                          (:num (first args))
                          (map :num (rest args))))
         '<= (BoolV (<= (:num (first args)) (:num (second args))))
         'are-equal? (BoolV (= (first args) (second args)))
         ;'user-error (ErrV (user-error (first args)))
         ))

(defn appC-helper [f-val arg-vals]
  (match f-val
         ; PrimV
         {:op op} (apply-primop op arg-vals)
         ;CloV
         {:args f-args :body f-body :env f-env}
         (if (= (count f-args) (count arg-vals))
           (let [new-bindings (map (fn [arg val] [arg val]) f-args arg-vals)
                 new-env (assoc f-env new-bindings)]
             (interp f-body new-env))
           (throw (Exception. "Argument count mismatch")))))

(defn ifC-helper [test]
  (match test
         {:bool b} b))

(defn interp
  "interp"
  [expr env]
  (match [expr env]
         [{:numC a} _] (struct NumV a)
         [{:strC s} _] (struct StrV s)
         [{:args f :body b} e] (struct CloV f b e)
         [{:idC i} e] (lookup i e)
         [{:func func :args args} e] (appC-helper (interp func e) (map #(interp % e) args))
         [{:test test :then then :else else} e] (if (ifC-helper (interp test e)) (interp then e) (interp else e))
         :else "couldn't match"))

;; test examples

;; num case
(= (interp (struct NumC 2) top-level-bindings) (struct NumV 2))

;; str case
(= (interp (struct StrC "hi") top-level-bindings) (struct StrV "hi"))

;; lamC case
(= (interp (struct LamC (list (struct IdC 'a) (struct IdC 'b)) (struct AppC (struct IdC '+) (list (struct NumC 2) (struct NumC 3)))) top-level-bindings) (struct CloV (list (struct IdC 'a) (struct IdC 'b)) (struct AppC (struct IdC '+) (list (struct NumC 2) (struct NumC 3))) top-level-bindings))

;; idC/AppC case and apply-primop/lookup addition
(= (interp (struct IdC '+) top-level-bindings) (struct PrimV '+))
(= (interp (struct AppC (struct IdC '+) (list (struct NumC 3) (struct NumC 8))) top-level-bindings) 11)
(= (interp (struct AppC (struct IdC '-) (list (struct NumC 3) (struct NumC 8))) top-level-bindings) -5)

;; if case
(= (interp (struct IfC (struct IdC 'trueV) (struct StrC "hi") (struct StrC "else")) top-level-bindings) (struct StrV "hi"))
