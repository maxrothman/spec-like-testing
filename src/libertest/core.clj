(ns libertest.core
  (:require [clojure.test :as test]
            [clojure.zip :as zip]))

(defmulti show ::type)
(defmethod show :default
  [x]
  (prn x))

;; TODO: support tagging assertions with names
;; will require digging into clojure.test internals
;; (or doing something different entirely)

(defmacro assertion [name & forms]
  `{::type ::assertion
    ::name ~name
    ::code (fn [] ~@forms)})

(defmethod show ::assertion
  [x]
  [::assertion (::name x) (::code x)])

(defn group [name & trees]
  {::type ::group
   ::name name
   ::children (vec trees)})

(defmethod show ::group
  [x]
  [::group (::name x) (mapv show (::children x))])

(defn make-zipper [root]
  (zip/zipper (comp some? ::children)
              (comp seq ::children)
              (fn [node children] (assoc node ::children children)
                ;; I don't really understand why this is wrong?
                ;; I guess "make-node" would be better named as "set-children"?
                #_(update node ::children #(into (or % []) children)))
              root))
(def node (assertion "foo" inc))
(update node ::children into [node])

(defn spy
  ([x]
   (prn x)
   x)
  ([msg x]
   (prn msg)
   x))

(defn empty-node
  "An placeholder node that does nothing except have children.
   Used to work around splicing issues"
  [children]
  {::type ::empty
   ::children children})

(defmethod show ::empty
  [x]
  [::empty (mapv show (::children x))])

;; From https://groups.google.com/g/clojure/c/FIJ5Pe-3PFM
;; TODO: don't manually set :end, that's an impl detail
;; Not needed currently, delete if not used by the time this is finished
#_(defn next-over [loc]
  (cond
    (zip/end? loc) loc
    (some? (zip/right loc)) (zip/right loc)
    (some? (zip/up loc)) (recur (zip/up loc))
    :else (assoc loc 1 :end)))

(defn map-tree [filter-f map-f tree]
  (loop [loc (make-zipper tree)]
    (cond
      (zip/end?) (zip/root loc)
      (filter-f loc) (recur (zip/next (map-f loc)))
      :else (recur (zip/next loc)))))

(defn around-each [f & trees]
  (empty-node
   (vec
    (for [tree trees]
      (loop [loc (make-zipper tree)]
        (cond
          (zip/end? loc) (zip/root loc)
          (zip/branch? loc) (recur (zip/next loc))
          :else (recur
                 (zip/next
                  (zip/edit loc update ::code #(fn [] (f %)))))))))))

(defn around [f tree]
  nil)

(defmethod show ::hook
  [x]
  [::hook (::code x) (mapv show (::children x))])

(comment
  (def tree (show (around-each (fn [f] (prn "outer") (f))
                               (group "baz"
                                      (around-each (fn [f] (prn "inner") (f))
                                                   (group "foo"
                                                          (assertion "a1" (test/is (= 1 (inc 0))))
                                                          (group "bar"
                                                                 (assertion "a2" (test/is (not= 2 (inc 3))))))
                                                   (assertion "a3" (test/is (= 1 (dec 2)))))))))
  (assertion (test/is (= 1 1)))

  (map (comp :name zip/node)
       (take-while (complement zip/end?)
                   (iterate zip/next (make-zipper {:name "a"
                                                   :children [{:name "b"
                                                               :children [{:name "c"}]}
                                                              {:name "d"}]}))))
  ;; => ("a" "b" "c" "d")
  )
