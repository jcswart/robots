(ns robots.core)

(def test-input "5 5\n1 2 N\nLMLMLMLMM\n3 3 E\nMMRMMRMRRM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parsing

(defn convert-char
  "Input files are parsed into char seqs. We need either ints or keywords."
  [c]
  (when c
    (let [s (str c)]
      (try
        (Integer/parseInt s)
        (catch Exception e
          (keyword s))))))

(defn convert-parsed
  "Convert the parse input into clojure data structures."
  [parsed]
  (for [coll parsed]
    (map convert-char coll)))

(defn remove-spaces
  "Spaces from parsed input are unnessary."
  [parsed]
  (let [space?        #(= \space %)
        remove-spaces (fn [coll] (filter (complement space?) coll))]
    (map remove-spaces parsed)))

(defn parse-input
  "Turn a file of robot commands into clojure data."
  []
  (let [strings->char-seqs #(map seq %)]
    (-> test-input
        (clojure.string/split-lines)
        (strings->char-seqs)
        (remove-spaces)
        (convert-parsed)
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Robots

(def turn-left  {:N :W, :W :S, :S :E, :E :N})
(def turn-right {:N :E, :E :S, :S :W, :W :N})


(defn make-robot
  "A robot representation.

  :x       - x coord
  :y       - y coord
  :heading - cardinal direction robot will move
  :index   - when this robot will move in comparision to others. lower is first.
  :actions - parsed actions"
  [idx [x-y-heading actions]]
  {:x       (first x-y-heading)
   :y       (second x-y-heading)
   :heading (last x-y-heading) ;; (get x-y-heading 2) is nil, last works. what is lazyseq doing?
   :index   (inc idx)
   :actions actions})

(defn name-robot
  "A name based on origin location."
  [{:keys [x y heading] :as robot}]
  (assoc robot :name (str x "-" y "-" (name heading))))

(defn move-robot
  "Given heading 'h' move the robot."
  [[x y h]]
  (case h
    :N [x       (inc y) h]
    :E [(inc x) y       h]
    :S [x       (dec y) h]
    :W [(dec x) y       h]))

(defn do-action
  "Given robot (x,y,h) perform action."
  [[x y h] action]
  (case action
    :L [x y (h turn-left)]
    :R [x y (h turn-right)]
    :M (move-robot [x y h])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment

(defn seed-robots
  [env]
  (let [robots (->> (:robot-movements env)
                    (map-indexed make-robot)
                    (map name-robot))]
    (-> env
        (assoc :robot-origins robots)
        (dissoc :robot-movements))))

(defn create-environment [[max-bounds & rest]]
  (println rest)
  {:max-plateau-bounds max-bounds
   :robot-movements (partition 2 rest)})


(defn make-series [{x :x y :y h :heading :as r-o}]
  (let [series-builder (fn [series action]
                         (let [prev   (last series)
                               update (do-action prev action)]
                           (conj series update)))
        init-series [[x y h]]]
    (assoc r-o :series (reduce series-builder init-series (:actions r-o)))))

(defn create-movement-series [{r-o :robot-origins :as env}]
  (assoc env :robot-series (map make-series r-o)))

(defn final-output [{r-s :robot-series :as env}]
  (map #(last (:series %)) r-s))

;; summarize series
;; (let [{s :series a :actions} (first (:robot-series res))] (interleave s a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main

(defn debug
  "Put between fn in a -> to debug."
  [x]
  (println x)
  x)

(defn -main [& args]
  (-> (parse-input)
      (create-environment)
      (seed-robots)
      (create-movement-series)
      (final-output)))
