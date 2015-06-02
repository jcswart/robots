(set-env!
 :source-paths #{"src"})

(require '[robots.core :as r])

(defn print-output
  "Convert & print results."
  [{output :output :as robot-result}]
  (let [xform (fn [[x y h]]
                (format "%s %s %s" x y (name h)))]
    (doseq [x (map xform output)]
      (println x))))

(defn print-warnings
  "Convert and print out of bounds warning on series."
  [{world :world :as robot-result}]
  (let [warnings (fn [{:keys [valid? series]}]
                   (format "%s will stay on plateau? %s" (last series) valid?))]
    (doseq [x (map warnings world)]
      (println x))))

(deftask robots
  "Determine the final location given a robot instructions input FILE."
  [i input FILE str  "the robot instructions"
   w warn       bool "Display out of bounds warnings."]
  (let [result   (r/-main (:input *opts*))]
    (print-output result)
    (when (:warn *opts*)
      (print-warnings result))))
