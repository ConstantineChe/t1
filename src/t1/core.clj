(ns t1.core)

(declare evaluate)

(defn- resolve-args [vars op args]
  (let [args-count (count args)]
    (when (= 0 args-count)
      (throw (new Exception
                  (format "Parse error: 0 arguments appliued to %s function" (name op)))))
    (map (fn [x]
           (cond
             (and (= op 'abs) (not= 1 args-count))
             (throw (new Exception
                         (format "Parse error: abs takes 1 argument %d given" args-count)))

             (and (= op 'power) (not= 2 args-count))
             (throw (new Exception
                         (format "Parse error: power takes 2 arguments %d given" args-count)))

             (number? x) x

             (symbol? x) ((keyword x) vars)

             (list? x) (evaluate vars x)
             :else (throw (new Exception (format "Parse error: %d is not a number" x)))))
         args)))

(defn evaluate [vars [op & args]]
  (let [args (resolve-args vars op args)]
    (cond
      (#{'+ '- '* '/} op) (apply (resolve op) args)

      (= 'abs op) (Math/abs (first args))

      (= 'power op) (Math/pow (first args) (second args))

      :else (throw (new Exception (format "Parse error: cannot resolve operation %s" op))))))

(defn- optimize* [op arg expr]
  (cond
    (and (= op 'abs) (seq? arg) (-> arg first (= 'abs)))
    (reduced arg)

    (and (= 2 (count expr)) (= op 'power) (integer? arg) (even? arg)
         (seq? (second expr)) (-> expr second first (= 'abs)))
    (reduced (list 'power (-> expr second second) arg))

    (symbol? arg) (concat expr [arg])

    (seq? arg) (concat expr [arg])

    (#{'- '+} op)
    (if (zero? arg) expr (concat expr [arg]))

    (= op '*) (if (zero? arg)
                (reduced 0)
                (if (= 1 arg) expr (concat expr [arg])))

    (= op '/) (if (and (= 1 arg) (> (count expr) 1))
                expr
                (concat expr [arg]))

    (and (= 2 (count expr)) (= op 'power) (zero? arg)) (reduced 1)

    (and (= 2 (count expr)) (= op 'power) (= 1 arg)) (reduced (second expr))

    :else (concat expr [arg])))

(defn optimize [[op & args]]
  (let [expr (reduce (fn [expr arg]
                       (if (list? arg)
                         (let [optimized (optimize arg)]
                           (optimize* op optimized expr))
                         (optimize* op arg expr)))
                     (list op) args)]
    (cond
      (number? expr) expr
      (symbol? expr) expr

      (and (= 1 (count expr)) (#{'+ '-} op)) 0
      (and (= 1 (count expr)) (= '* op)) 1

      (and (#{'- '+ '/ '*} op) (= 2 (count expr)))
      (second expr)

      :else expr)))

(defn- get-vars [[_ & args]]
  (distinct
    (reduce (fn [vars arg]
              (if (symbol? arg)
                (conj vars arg)
                (if (list? arg)
                  (concat vars (get-vars arg))
                 vars))) [] args)))

(defn- ->javascript* [[op & args]]
  (cond
    (= op 'power) (apply format "Math.pow(%s, %s)"
                         (map (fn [arg]
                                (if (list? arg)
                                  (->javascript* arg)
                                  arg))
                              args))
    (= op 'abs) (format "Math.abs(%s)" (if (-> args first list?)
                                         (-> args first ->javascript*)
                                         (first args)))

    :else
   (str "("
        (apply str (interpose (str " " (name op) " ")
                              (map (fn [arg]
                                     (if (list? arg)
                                       (->javascript* arg)
                                       arg))
                                   args)))
        ")")))

(defn ->javascript [name expr]
  (str "function " name "(" (apply str (interpose ", " (get-vars expr)))
       ") { return " (->javascript* expr) "; }"))
