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
  "贪婪匹配"
  ([d e]
   (find-next-word2 d e 3))
  ([d e ^long i]
   (loop [m [] s (set (map inc e))]
     (if (not (empty? s))
       (let [nw (reduce concat []
                        (filter #(> (count %) i)
                                (map
                                 #(set/intersection s (val %))
                                 d)))
             nm (set/intersection (set (map dec nw)))]
         (recur
          (concat m nm nw)
          (set (map inc nw)))
         )
       m))))
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

(defn find-word-bound [col]
  (loop [m (transient (vector (first col))) c col]
    (if (> (count c) 1)
      (recur
       (let [a (first c) b (first (rest c))]
         (if (= (inc a) b) nil (reduce conj! m [a b]))
         m)
       (rest c))
      (persistent! m))))

(defn -main
  "parse txt, find word"
  [& args]
  (let [chars (read-txt-file "/home/liuchunhua/1.txt")
        dict (find-repeat-word chars)
        nums (filter seq
                     (reduce conj []
                             (map
                              (partial find-next-word2 dict)
                              (vals dict))))
        ]
    (doseq [s nums]
      (let [article (str/join chars)
            m (map #(apply subs article %)
                   (partition 2 (find-word-bound (sort (set s)))))]
        (doseq [w m]
          (println w))))))
