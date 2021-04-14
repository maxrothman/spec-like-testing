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

(defmacro group [name & forms]
  `{::type ::group
    ::name ~name
    ::children ~(vec forms)})

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

;; From https://groups.google.com/g/clojure/c/FIJ5Pe-3PFM
;; TODO: don't manually set :end, that's an impl detail
(defn next-over [loc]
  (cond
    (zip/end? loc) loc
    (some? (zip/right loc)) (zip/right loc)
    (some? (zip/up loc)) (recur (zip/up loc))
    :else (assoc loc 1 :end)))

(defn around-each [f tree]
  (loop [loc (make-zipper tree)]
    (cond
      (zip/end? loc) (zip/root loc)
      (zip/branch? loc) (recur (zip/next loc))
      :else (recur
             (next-over
              (zip/edit loc update ::code #(fn [] (f %))))))))

(defmethod show ::before
  [x]
  [::before (::code x) (mapv show (::children x))])

(comment
  (def tree (show (around-each (fn [f] (prn "outer") (f))
                               (around-each (fn [f] (prn "inner") (f))
                                            (group "foo"
                                                   (assertion "a1" (test/is (= 1 (inc 0))))
                                                   (group "bar"
                                                          (assertion "a2" (test/is (not= 2 (inc 3))))))))))
  (assertion (test/is (= 1 1)))

  (map (comp :name zip/node)
       (take-while (complement zip/end?)
                   (iterate zip/next (make-zipper {:name "a"
                                                   :children [{:name "b"
                                                               :children [{:name "c"}]}
                                                              {:name "d"}]}))))
  ;; => ("a" "b" "c" "d")
  )
