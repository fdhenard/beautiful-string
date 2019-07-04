(ns beautiful-string.core
  (:require [clojure.pprint]))

;; from https://stackoverflow.com/a/26076537/59439
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(def valid-chars
  "a-z"
  (->> (range 97 123)
       (map 
        #(-> %
             char
             str))
       set))

(defn get-char-beauty-calc-maps [line]
  (let [unique-chars (->> line
                          (map clojure.string/lower-case)
                          (filter #(contains? valid-chars %))
                          set)
        ;; _ (println "unique chars" unique-chars ", count:" (count unique-chars))
        perms (clojure.math.combinatorics/permutations unique-chars)
        beauty-perms (map
                      (fn [letters]
                        (-> letters
                            (zipmap (reverse (range 1 27)))))
                      perms)]
    beauty-perms))

(defn char-to-beauty [char char-beauty-calc-map]
  (let [as-lower (clojure.string/lower-case char)
        res (get char-beauty-calc-map as-lower 0)]
    res))

(defn calc-max-beauty [line]
  (let [char-beauty-calc-maps (get-char-beauty-calc-maps line)
        ;; _ (clojure.pprint/pprint char-beauty-calc-maps)
        ;; _ (println (str "count: " (count char-beauty-calc-maps)))
        possible-sums-for-line
        (map
         (fn [char-beauty-calc-map]
           (->> line
                (map #(char-to-beauty % char-beauty-calc-map))
                (reduce +)))
         char-beauty-calc-maps)]
    (apply max possible-sums-for-line)))

(comment
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (calc-max-beauty line))))

(defn -main [& args]
  (println (calc-max-beauty (first args))))
