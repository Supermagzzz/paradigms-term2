(defn proto-get [obj key] (cond (contains? obj key) (obj key) (contains? obj :prototype) (proto-get (obj :prototype) key)))
(defn proto-call [this key & args] (apply (proto-get this key) (cons this args)))
(defn field [key] (fn [this] (proto-get this key)))
(defn method [key] (fn [this & args] (apply proto-call this key args)))

(defn constant [value] (constantly value))
(defn variable [x] (fn [values] (get values x)))

(defn operation [f] (fn [& operands] (fn [values] (apply f (mapv (fn [op] (op values)) operands)))))
(defn unary [f] (fn [operand] (fn [values] (f (operand values)))))

(defn _divide [a & other] (if (seq? other) (reduce (fn [a b] (/ a (double b))) a other) (/ (double a))))
(defn _exp [x] (Math/exp x))
(defn _sumexp [& args] (apply + (mapv _exp args)))
(defn _softmax [& args] (/ (_exp (first args)) (double (apply _sumexp args))))

(defn binary-bit-operation [op] (fn [a b] (Double/longBitsToDouble (op (Double/doubleToLongBits a) (Double/doubleToLongBits b)))))
(defn binary-bit-operation' [op] (fn [a' b'] (Double/longBitsToDouble (op (Double/doubleToLongBits a') (Double/doubleToLongBits b')))))
(defn bit-operation [op] (fn [& args] (reduce (binary-bit-operation op) args)))
(defn bit-operation' [op] (fn [args args'] (reduce (binary-bit-operation' op) args')))

(defn bit-impl [a b] (bit-or (bit-not a) b))
(defn bit-iff [a b] (bit-and (bit-impl a b) (bit-impl b a)))
(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(def divide (operation _divide))
(def my-xor (operation (bit-operation bit-xor)))
(def my-or (operation (bit-operation bit-or)))
(def my-and (operation (bit-operation bit-and)))
(def my-impl (operation (bit-operation bit-impl)))
(def my-iff (operation (bit-operation bit-and)))
(def negate (unary -))
(def exp (unary _exp))
(def sumexp (operation _sumexp))
(def softmax (operation _softmax))

(defn BaseOperation [evaluate toString toStringInfix diff]
  {:evaluate      evaluate
   :toString      toString
   :toStringInfix toStringInfix
   :diff          diff})

(def toString (method :toString))
(def toStringInfix (method :toStringInfix))
(def evaluate (method :evaluate))
(def diff (method :diff))

(declare zero)
(declare one)

(def Constant-proto
  (let [val (field :value)]
    (BaseOperation
      (fn [this vars] (val this))
      (fn [this] (format "%.1f" (double (val this))))
      (fn [this] (format "%.1f" (double (val this))))
      (fn [this x] zero))))

(def Variable-proto
  (let [name (field :name)]
    (BaseOperation
      (fn [this vars] (vars (name this)))
      (fn [this] (name this))
      (fn [this] (name this))
      (fn [this x] (if (= x (name this)) one zero)))))

(def Operation-proto
  (let [name (field :name)
        op (field :op)
        op' (field :op')
        args (field :args)]
    (BaseOperation
      (fn [this vars] (apply (op this) (mapv #(evaluate % vars) (args this))))
      (fn [this] (str "(" (name this) " " (clojure.string/join " " (mapv toString (args this))) ")"))
      (fn [this] (if (= (count (args this)) 1)
                   (str (name this) "(" (apply toStringInfix (args this)) ")")
                   (str "(" (clojure.string/join (str " " (name this) " ") (mapv toStringInfix (args this))) ")")))
      (fn [this x] ((op' this) (args this) (mapv #(diff % x) (args this)))))))

(defn Constant [value]
  {:prototype Constant-proto
   :value     value})

(defn Variable [name]
  {:prototype Variable-proto
   :name      (str name)})

(defn Operation [name op op']
  (let [this-proto {:prototype Operation-proto
                    :name      name
                    :op        op
                    :op'       op'}]
    (fn [& args]
      {:prototype this-proto
       :args      (vec args)})))

(def zero (Constant 0))
(def one (Constant 1))

(def Add (Operation "+" + (fn [args args'] (apply Add args'))))
(def Subtract (Operation "-" - (fn [args args'] (apply Subtract args'))))
(def Negate (Operation "negate" - (fn [args args'] (Negate (first args')))))

(declare Multiply)
(defn Multiply' [args args']
  (second (reduce
            (fn [[a a'] [b b']] [(Multiply a b) (Add (Multiply a' b) (Multiply a b'))])
            [args args'])))

(def Multiply (Operation "*" * Multiply'))

(declare Divide)
(defn Divide' [args args']
  (let [a (first args)
        a' (first args')
        b (apply Multiply (rest args))
        b' (apply Multiply (rest args'))]
    (Divide (Subtract (Multiply a' b) (Multiply a b')) (Multiply b b))))

(def Divide (Operation "/" _divide Divide'))

(def Xor (Operation "^" (bit-operation bit-xor) (bit-operation' bit-xor)))
(def And (Operation "&" (bit-operation bit-and) (bit-operation' bit-and)))
(def Or (Operation "|" (bit-operation bit-or) (bit-operation' bit-or)))
(def Impl (Operation "=>" (bit-operation bit-impl) (bit-operation' bit-impl)))
(def Iff (Operation "<=>" (bit-operation bit-iff) (bit-operation' bit-iff)))

(def Exp (Operation 'exp _exp (fn [a a'] (Multiply a' (Exp a)))))

(declare Sumexp)
(defn Sumexp' [args args'] (apply Add (map-indexed (fn [i x] (Multiply (Exp x) (nth args' i))) args)))

(def Sumexp (Operation "sumexp" _sumexp Sumexp'))

(def Softmax (Operation "softmax" _softmax
                        (fn [args args']
                          (let [sum (apply Sumexp args)
                                sum' (Sumexp' args args')
                                a (Exp (first args))
                                a' (Multiply (first args') a)]
                            (Divide' [a sum] [a' sum'])))))
(def Operations {"+"       Add
                 "-"       Subtract
                 "*"       Multiply
                 "/"       Divide
                 "^"       Xor
                 "|"       Or
                 "&"       And
                 "=>"      Impl
                 "<=>"     Iff
                 "negate"  Negate
                 "sumexp"  Sumexp
                 "softmax" Softmax})

(def operations {"+"       add
                 "-"       subtract
                 "*"       multiply
                 "/"       divide
                 "^"       my-xor
                 "|"       my-or
                 "&"       my-and
                 "=>"      my-impl
                 "<=>"     my-iff
                 "negate"  negate
                 "sumexp"  sumexp
                 "softmax" softmax})

(defn parse [operations' Const-impl Variable-impl]
  (fn [expression]
    (letfn [(parse' [token]
              (cond
                (number? token) (Const-impl token)
                (symbol? token) (Variable-impl (str token))
                (seq? token) (apply (operations' (str (first token))) (map parse' (rest token)))))]
      (parse' (read-string expression)))))
(def parseObject (parse Operations Constant Variable))
(def parseFunction (parse operations constant variable))

;Combinators starts
(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _show [result] (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result)))) "!"))
(defn tabulate [parser inputs] (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))
(defn _empty [value] (partial -return value))
(defn _char [p] (fn [[c & cs]] (if (and c (p c)) (-return c cs))))
(defn _map [f result] (if (-valid? result) (-return (f (-value result)) (-tail result))))
(defn _combine [f a b] (fn [str] (let [ar ((force a) str)] (if (-valid? ar) (_map (partial f (-value ar)) ((force b) (-tail ar)))))))
(defn _either [a b] (fn [str] (let [ar ((force a) str)] (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p] (fn [input] (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))
(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (partial _map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value] (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps] (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps] (reduce _either p ps))
(defn +opt [p] (+or p (_empty nil)))
(defn +star [p] (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))
;Combinators ends

(def *digit (+char "0123456789"))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(def *number
  (+map Constant (+map read-string (+seqf (fn [sign int dot double] (apply str sign (apply str int) dot (apply str double)))
                                          (+opt (+char "+-"))
                                          (+plus *digit)
                                          (+opt (+char "."))
                                          (+opt (+plus *digit))))))
(def *variable (+map Variable (+char "xyz")))
(defn *string [operation] (apply +seqf (constantly operation) (mapv #(+char (str %)) (str operation))))
(def *negate (*string "negate"))

(defn *get-left-operation [operations-map left & [right]]
  (second (reduce (fn [[_ cur1] [op2 cur2]] [nil ((operations-map op2) cur1 cur2)]) (cons [nil left] right))))
(defn *get-right-operation [operations-map left & [right]]
  (second (reduce (fn [[op1 cur1] [_ cur2]] [op1 ((operations-map op1) cur2 cur1)]) (reverse (cons [nil left] right)))))

(defn *get-unary [name value] ((Operations name) value))

(defn *decrease-priority [operations-map rule *previous]
  (let [parser (apply +or (mapv *string (keys operations-map)))]
    (+seqf (partial rule operations-map) (*previous) (+star (+seq *ws parser *ws (*previous))))))

(defn left  [operations-map] (partial *decrease-priority operations-map *get-left-operation))
(defn right [operations-map] (partial *decrease-priority operations-map *get-right-operation))

(declare *low)

(def *operation-symbol [(left {"*" Multiply "/" Divide})
                        (left {"+" Add "-" Subtract})
                        (left {"&" And})
                        (left {"|" Or})
                        (left {"^" Xor})
                        (right {"=>" Impl})
                        (left {"<=>" Iff})])

(def *value (reduce (fn [last cur] #(cur last)) (cons *low *operation-symbol)))

(def *brackets (delay (+seqn 0 (+ignore (+char "(")) *ws (*value) *ws (+ignore (+char ")")))))
(def *unary (+seqf *get-unary *negate *ws (delay (+or *brackets (+seqn 0 *ws *low *ws)))))
(def *low (+or *number *variable *unary *brackets))

(def parseObjectInfix (+parser (+seqn 0 *ws (*value) *ws)))