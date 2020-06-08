(defn is-tensor? [v] (or (number? v)
                         (and (apply = (mapv (fn [x] (if (vector? x) (count x) 0)) v))
                              (is-tensor? (reduce (fn [a b] (apply conj a b)) v)))))

(defn get-form [t] (if (number? t) (list) (conj (get-form (first t)) (count t))))

(defn max-form [& tensors] (reduce (fn [a b] (if (< (count a) (count b)) b a)) (list) (mapv get-form tensors)))

(defn can-broadcast [a form] (every? identity (mapv = (reverse (get-form a)) (reverse form))))

(defn broadcast [& t]
  (let [form (apply max-form t)]
    (mapv (fn [a] ((fn [form a]
                     (def size (count (get-form a)))
                     (letfn [(broadcast-impl [cur-form]
                               (if (= size (count cur-form)) a (vec (repeat (first cur-form) (broadcast-impl (rest cur-form))))))]
                       (broadcast-impl form))) form a)) t)))

(defn check-dim [x size] (and (is-tensor? x) (= size (count (get-form x)))))
(defn is-vector? [v] (check-dim v 1))
(defn is-matrix? [m] (check-dim m 2))

(defn base-f [type] (fn [f]
                      (letfn [(base-f-impl' [& v] (if (is-vector? v) (apply f v) (apply mapv base-f-impl' v)))
                              (base-f-impl [& v]
                                {:pre [(every? type v) (every? (fn [t] (can-broadcast t (apply max-form v))) v)]}
                                (apply base-f-impl' (apply broadcast v)))
                              ] base-f-impl)))

(def vector-f (base-f is-vector?))
(def matrix-f (base-f is-matrix?))
(def tensor-f (base-f is-tensor?))

(def v+ (vector-f +))
(def v- (vector-f -))
(def v* (vector-f *))

(def m+ (matrix-f +))
(def m- (matrix-f -))
(def m* (matrix-f *))

(def b+ (tensor-f +))
(def b- (tensor-f -))
(def b* (tensor-f *))
(def bd (tensor-f /))

(defn scalar-mul [type f] (fn [v & s] {:pre [(type v) (every? number? s)]} (apply f (apply broadcast v s))))
(defn matrix-mul [type f] (fn [x & other] {:pre [(every? type other)]} (mapv (fn [v] (apply f v other)) x)))

(def v*s (scalar-mul is-vector? v*))
(defn scalar [a b] (reduce + (v* a b)))

(defn vect [& v] (reduce
                   (fn [a b] {:pre [(= (list 3) (get-form a)) (= (list 3) (get-form b))]}
                     (letfn [(det [i j] (- (* (nth a i) (nth b j)) (* (nth a j) (nth b i))))]
                       [(det 1 2), (det 2 0), (det 0 1)])) v))

(defn transpose [m] {:pre [(is-matrix? m)]} (apply mapv vector m))

(def m*s (scalar-mul is-matrix? m*))
(def m*v (matrix-mul is-vector? scalar))
(defn m*m [& m] (reduce (matrix-mul is-matrix? (fn [v m] (m*v (transpose m) v))) m))

(defn is-tensor? [v] (or (number? v)
                         (and (apply = (mapv (fn [x] (if (vector? x) (count x) 0)) v))
                              (every? is-tensor? v))))

(defn get-form [t] (if (number? t) (list) (conj (get-form (first t)) (count t))))

(defn max-form [& tensors] (reduce (fn [a b] (if (< (count a) (count b)) b a)) (list) (mapv get-form tensors)))

(defn can-broadcast [a form] (every? identity (mapv = (reverse (get-form a)) (reverse form))))

(defn broadcast [& t]
  (let [form (apply max-form t)]
    (mapv (fn [a] ((fn [form a]
                     (def size (count (get-form a)))
                     (letfn [(broadcast-impl [cur-form]
                               (if (= size (count cur-form)) a (vec (repeat (first cur-form) (broadcast-impl (rest cur-form))))))]
                       (broadcast-impl form))) form a)) t)))

(defn check-dim [x size] (and (is-tensor? x) (= size (count (get-form x)))))
(defn is-vector? [v] (check-dim v 1))
(defn is-matrix? [m] (check-dim m 2))

(defn base-f [type] (fn [f]
                      (letfn [(base-f-impl' [& v] (if (is-vector? v) (apply f v) (apply mapv base-f-impl' v)))
                              (base-f-impl [& args]
                                {:pre [(every? type args) (every? (fn [t] (can-broadcast t (apply max-form args))) args)]}
                                (apply base-f-impl' (apply broadcast args)))
                              ] base-f-impl)))

(def vector-f (base-f is-vector?))
(def matrix-f (base-f is-matrix?))
(def tensor-f (base-f is-tensor?))

(def v+ (vector-f +))
(def v- (vector-f -))
(def v* (vector-f *))

(def m+ (matrix-f +))
(def m- (matrix-f -))
(def m* (matrix-f *))

(def b+ (tensor-f +))
(def b- (tensor-f -))
(def b* (tensor-f *))
(def bd (tensor-f /))

(defn scalar-mul [type f] (fn [v & s] {:pre [(type v) (every? number? s)]} (apply f (apply broadcast v s))))
(defn matrix-mul [type f] (fn [x & other] {:pre [(every? type other)]} (mapv (fn [v] (apply f v other)) x)))

(def v*s (scalar-mul is-vector? v*))
(defn scalar [a b] (reduce + (v* a b)))

(defn vect [& v] (reduce
                   (fn [a b] {:pre [(= (list 3) (get-form a)) (= (list 3) (get-form b))]}
                     (letfn [(det [i j] (- (* (nth a i) (nth b j)) (* (nth a j) (nth b i))))]
                       [(det 1 2), (det 2 0), (det 0 1)])) v))

(defn transpose [m] {:pre [(is-matrix? m)]} (apply mapv vector m))

(def m*s (scalar-mul is-matrix? m*))
(def m*v (matrix-mul is-vector? scalar))
(defn m*m [& m] (reduce (matrix-mul is-matrix? (fn [v m] (m*v (transpose m) v))) m))
