(ns aoc2018-1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; 공통 함수
;; trim 과 함께 문제열을 정수로 변환
(defn parse-int [string]
  (Integer/parseInt (str/trim string)))

;; resource-path 에 있는 파일을 읽어 정수 시퀀스로 반환
(defn- read-lines [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)
       (map parse-int)))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(comment
  (println (reduce + (read-lines "2018_1.txt")))) ; 595

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

;; set 에 누적 합을 기록하고, 새로운 합이 set 에 포함되어 있다면 처음으로 두번 나오는 숫자로 판단하여 리턴함
(defn first-duplicated-sum [resource-path]
  (let [nums (cycle (read-lines resource-path))]
    (loop [result-set #{}
           [num & more] (reductions + 0 nums)]
      (if (result-set num)
        num
        (recur (conj result-set num) more)))))

(comment
  (println (first-duplicated-sum "2018_1.txt"))) ; 80598