(ns aoc2018-2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 문자열 시퀀스로 반환"
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)))

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

(defn- str-to-frequency-map
  "string 을 char frequency map 으로 반환하는 함수"
  [input-string]
  (reduce (fn [char-map key]
            (let [current (get char-map key 0)]
              (assoc char-map key (inc current))))
          {}
          input-string))

(defn- flags-for-2-3
  "char-frequency-map 에 값이 2 또는 3인 항목이 존재하는지 여부를 [1 1] 형태로 반환하는 함수"
  [char-frequency-map]
  ; 전체를 순회할수밖에 없는 구조인데 중간에 멈추는 것이 좋다
  ; reduced 혹은 loop
  ; 로직 검토도 필요
  (reduce (fn [[two three] [_ value]]
            (case value
              2 [1 three]
              3 [two 1]
              [two three]))
          {:a1 0 :a2 0} ; vector 보다는 map 사용
          char-frequency-map))

(defn solution
  [resource-path]
  (->> (read-lines resource-path)
       (map str-to-frequency-map)
       (map flags-for-2-3)
       (apply mapv +)
       (apply *)))

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

; let 바인딩이나 ->> 사용
; 가독성이 떨어짐
(defn- only-one-char-diff?
  [first second]
  (->> (map vector first second)
       (filter (fn [[char1 char2]] (not= char1 char2)))
       count
       (= 1)))

; 원래 데이터를 pair 로 묶었다면,
; 재귀로도 충분히 풀 수 있음 시도해보면 좋을듯.
(defn- create-i-j
  [n]
  (for [i (range n)
        j (range n)
        :when (not= i j)]
    [i j]))

; 함수형으로 생각하기
; 데이터를 어떻게 변형하는지 < 중요
; nth 는 피해야되는 함수 (자료구조가 linked list)
; ide 경고 확인하기
(defn- get-only-one-diff-words
  [lines]
  (some (fn [[i j]]
          (let [first (nth lines i)
                second (nth lines j)]
            (if (only-one-char-diff? first second)
              [first second])))
        (create-i-j (count lines))))

(defn- get-result
  [[s1 s2]]
  (->> (filter  (fn [[c1 c2]] (= c1 c2))
                (map vector s1 s2))
       (map first)
       (apply str)))

; let binding 은 중간 결과가 필요할때
(defn part2-solution
  [resource-path]
  (->> (read-lines resource-path)
       (get-only-one-diff-words)
       (get-result)))

(part2-solution "2018_2.txt") ; vtnikorkulbfejvyznqgdxpaw

; 문제 키워드 네이밍

;; #################################
;; ###        Refactoring        ###
;; #################################
