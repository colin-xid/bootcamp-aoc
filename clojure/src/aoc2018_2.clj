(ns aoc2018-2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 문자열 시퀀스로 반환"
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)))

(defn- str-to-frequency-map
  "string 을 char frequency map 으로 반환하는 함수"
  [input-string]
  (reduce (fn [char-map key]
            (let [current (get char-map key 0)]
              (assoc char-map key (inc current))))
          {}
          (seq input-string)))

(defn- flags-for-2-3
  "char-frequency-map 에 값이 2 또는 3인 항목이 존재하는지 여부를 [1 1] 형태로 반환하는 함수"
  [char-frequency-map]
  (reduce (fn [[two three] [_ value]]
            (case value
              2 [1 three]
              3 [two 1]
              [two three]))
          [0 0]
          char-frequency-map))

(defn solution
  [resource-path]
  (let [[once twice]
        (->> (read-lines resource-path)
             (map str-to-frequency-map)
             (map flags-for-2-3)
             (apply mapv +))]
    (* once twice)))

(comment
  (solution "2018_2.txt")) ;3952

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
