(ns find-word.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))
(defn count-word
  "计算每个字符在集合出现的次数
  m 为map,c为字符
  "
  [m c]
  (assoc m c (inc (m c 0))))

(defn index-word
  "标识字符在集合的位置"
  [col]
  (persistent!
   (reduce
    (fn [m e]
      (let [[k v] e]
        (assoc! m k (conj (m k #{}) v))))
    (transient {})
    (map (fn [x y] (vector x y)) col (range (count col))))))

(defn find-next-word
  "return set"
  [d e]
  (let [s (set (map inc e))
        sub (reduce
             concat {}
             (filter #(> (count %) 1)
                     (map #(set/intersection s (val %)) d)))]
    (if (empty? sub)
      sub
      (concat sub e))))

(defn find-next-word2
  [d e]
  (loop [m [] s (set (map inc e))]
    (if (and (< (count m) 100) (not (empty? s)))
      (let [nw (reduce concat []
                       (filter #(> (count %) 1)
                               (map
                                #(set/intersection s (val %))
                                d)))
            nm (set/intersection (set (map dec nw)))]
        (recur
         (concat m nm nw)
         (set (map inc nw)))
        )
      m))
  )
(defn read-txt-file [path]
  "read txt file,return character collection"
  (with-open [reader (io/reader path)]
    (reduce concat
            (doall (for [c (line-seq reader)] c)))))
(defn find-repeat-word
  "find word that repeat two or more"
  [m]
  (let [dict (index-word m)
        keyset (map
                #(if (= 1 (count (val %))) (key %)) dict) ;;only one
        ]
    (apply dissoc dict keyset)))

(defn -main
  "parse txt, find word"
  [& args]
  (let [chars (read-txt-file "/home/liuchunhua/1.txt")
        dict (find-repeat-word chars)
        nums (sort (set
                    (reduce concat
                            (map (partial find-next-word2 dict) (vals dict)))))
        split-num (filter #(not (nil? %))
                          (map
                           (fn [x] (let [[a b] x] (if (= (inc a) b) nil a)))
                           (partition 2 nums)))
        strs (map (fn [x] (let [[a b] x] (subs (str/join chars) a b)))
                  (partition 2 (conj split-num 0)))
        ]
    (println split-num)
    (println (count chars))
    (doseq [s strs]
      (println s))
    ))
