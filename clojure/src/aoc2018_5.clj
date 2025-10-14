(ns aoc2018-5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(defn- read-polymer-line
  "리소스 파일에서 폴리머 한 줄을 읽어 문자열로 반환한다."
  [resource-path]
  (slurp (io/resource resource-path)))

(defn- char-range
  "캐릭터 start 에서 end 까지 포함하는 캐릭터 시퀀스를 생성한다"
  [start end]
  (->> (range (int start) (inc (int end)))
       (map char)))

(defn- make-pair-pattern
  "두 문자 시퀀스의 각각을 이어 붙인 문자열들의 OR 조각을 만든다.
   생성되는 결과는 aA|...|zZ 형태이다."
  [range1 range2]
  (->> (mapv str range1 range2)
       (str/join "|")))

(defn- a-Z-pattern
  "대소문자만 다른 동일 문자 쌍(예: aA, Aa, bB, Bb, …)에 매치하는 정규식을 생성해 반환한다.
   생성되는 결과는 aA|bB|…|Aa|Bb|… 형태이다."
  []
  (let [a-z (char-range \a \z)
        A-Z (char-range \A \Z)
        a-A-pair (make-pair-pattern a-z A-Z)
        A-a-pair (make-pair-pattern A-Z a-z)]
    (-> (str a-A-pair "|" A-a-pair)
        re-pattern)))

(defn find-resulting-polymer-length
  [polymer]
  (let [pattern (a-Z-pattern)]
    (-> (loop [polymer polymer]
          (if (re-find pattern polymer)
            (recur (str/replace polymer pattern ""))
            polymer))
        (count))))

(find-resulting-polymer-length (read-polymer-line "2018_5.txt"))


;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn- remove-char
  "주어진 polymer 문자열에서 특정 문자를 대소문자구분 없이 제거하여 새로운 문자열을 반환한다."
  [polymer character]
  (let [pattern (re-pattern (str "(?i)" character))]
    (str/replace polymer pattern "")))

(defn find-shortest-polymer-length
  [polymer]
  (->> (char-range \a \z)
       (map str)
       (reduce (fn [before character]
                 (-> (remove-char polymer character)
                     find-resulting-polymer-length
                     (min before)))
               Integer/MAX_VALUE)))

(find-shortest-polymer-length (read-polymer-line "2018_5.txt"))