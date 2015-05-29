(ns robots.core
  "AdZerk Robots Interview Problem.

  Overview:
    - Parse a robot input file.
    - Create a map to represent the environment.
    - The environment contains the plateau bounds, and robots.
    - Robots have an origin location, heading, actions and series.
    - Based on the origin and actions we create a series.
    - The series represents every location/state the robot will inhabit.

  Why?
    - The rationale for this design was to make time 'first class'.
    - Each robot's series let's you deal with the robot 'in time' in a fairly
      straight-forward way.
    - Once you have a series you can detect collisions, plateau bounds, etc.
    - It is also somewhat monadic.  Calculate series first, mess with data then
      when you are ready 'commit' the future process and carry it out.
    - This also lets you work with collections which I personally prefer.
    ")

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

(defn create-environment
  "Given parsed input construct an environment map."
  [[max-bounds & rest]]
  {:max-plateau-bounds max-bounds
   :robot-movements (partition 2 rest)})

(defn seed-robots
  "Given environment make robot representations & actions."
  [env]
  (let [robots (->> (:robot-movements env)
                    (map-indexed make-robot)
                    (map name-robot))]
    (-> env
        (assoc  :robot-origins robots)
        (dissoc :robot-movements))))

(defn make-series
  "Determine each robots series based on origin and actions."
  [{x :x y :y h :heading :as r-o}]
  (let [series-builder (fn [series action]
                         (let [prev   (last series)
                               update (do-action prev action)]
                           (conj series update)))
        init-series [[x y h]]]
    (assoc r-o :series (reduce series-builder init-series (:actions r-o)))))

(defn create-movement-series
  "Robot actions are represented as a time series. A series is a list
  of all the locations the robot will inhabit based on the actions
  given to it."
  [{r-o :robot-origins :as env}]
  (assoc env :robot-series (map make-series r-o)))

(defn final-output
  "Final output is the last entry in the robot's series."
  [{r-s :robot-series :as env}]
  (map #(last (:series %)) r-s))

;; summarize series
;; (let [{s :series a :actions} (first (:robot-series res))] (interleave s a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main

(defn debug
  "Put between fns in a -> to debug."
  [x]
  (println x)
  x)

(defn -main [& args]
  (-> (parse-input)
      (create-environment)
      (seed-robots)
      (create-movement-series)
      (final-output)))
