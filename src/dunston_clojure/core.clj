(ns dunston-clojure.core
  (:gen-class))

(defn parse-args [args]
  (if-not (empty? args) (clojure.string/split (first args) #",")))

(defn parse-line [line]
  (let [line-details (clojure.string/split line #" ")]
    {:line-number (first line-details) :instruction (second line-details) :args (parse-args (rest (rest line-details)))}))

(def regs (atom {:A 0 :B 0 :C 0 :D 0}))

(def program "10 START\n20 MOV A,2\n30 ADD A,4\n40 DIV A,3\n50 PRN A\n60 STOP")

(defn coerce-to-number [val]
  (if (number? val)
    val
    (Integer/parseInt val)))

(defn mov [x y]
  (swap! regs assoc (keyword x) (coerce-to-number y)))

(defn add [x y]
  (mov x (+ (@regs (keyword x)) (coerce-to-number y))))

(defn sub [x y]
  (mov x (- (@regs (keyword x)) (coerce-to-number y))))

(defn mul [x y]
  (mov x (* (@regs (keyword x)) (coerce-to-number y))))

(defn div [x y]
  (mov x (float (/ (@regs (keyword x)) (coerce-to-number y)))))

(defn prn [x]
  (if-let [val (@regs (keyword x))]
    (println val)
    (println x)))

(def instruction-map {:PRN prn, :ADD add, :MOV mov, :SUB sub, :MUL mul, :DIV div})

(def parsed-program (map parse-line (clojure.string/split program #"\n")))

(defn execute-line [func args]
  (if-not (nil? func) (apply func args)))

(defn execute-lines [lines]
  (doseq [lines lines]  (execute-line ((keyword (:instruction lines)) instruction-map) (:args lines))))