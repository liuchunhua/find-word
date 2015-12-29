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

(defn find-next-character
  "d为字符的位置字典，e为某个字符的位置集合，i为下一个字符出现的频率。"
  ([d e]
   (find-next-character d e 3))
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
  "col为某个词语在文章中位置的集合，返还词语的开始s, 结束e [[s,e]...]"
  (loop [m (transient (vector (first col))) c col]
    (if (> (count c) 1)
      (recur
       (let [a (first c) b (first (rest c))]
         (if (= (inc a) b) nil (reduce conj! m [a b]))
         m)
       (rest c))
      (persistent! m))))

(defn find-word-position
  "dict为字符的位置字典,返回词语中所有字符位置集合[[]...]"
  [dict]
  (filter seq
          (reduce conj []
                  (map
                   (partial find-next-character dict)
                   (vals dict)))))

(defn find-word
  "找出文章中的词语，article为文章, col为词语在文章中的字符位置"
  [article col]
  (let [m (partition 2 (find-word-bound (sort (set col))))]
    (map #(subs article (first %) (inc (last %))) m)))

(defn -main
  "parse txt, find word"
  [ f & args]
  (let [chars (read-txt-file f)
        article (str/join chars)
        dict (find-repeat-word chars)
        nums (find-word-position dict)
        ]
    (doseq [s nums]
      (doseq [w (find-word article s)] (println w)))
    ))
