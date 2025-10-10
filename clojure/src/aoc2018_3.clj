(ns aoc2018_3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 문자열 시퀀스로 반환"
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)))

(defn- parse-claim
  "#1 @ 393,863: 11x29 를 {:id :x :y :width :height} 로 변환하는 함수"
  [line]
  (let [[_ id x y width height] (re-matches
                                 #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
                                 line)]
    {:id (parse-long id)
     :x (parse-long x)
     :y (parse-long y)
     :width (parse-long width)
     :height (parse-long height)}))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(defn- expand-points
  "width 와 height 를 이용하여 모든 천의 좌표를 {[x, y] 1} 로 변환하는 함수"
  [{:keys [_ x y width height]}]
  (for [point-x (range x (+ x width))
        point-y (range y (+ y height))]
    {[point-x point-y] 1}))

(defn count-overlapping-fabric
  "중복된 천의 넓이를 계산하는 함수"
  []
  (->> (read-lines "2018_3.txt")
       (map parse-claim)
       (mapcat expand-points)
       (apply merge-with +)
       (filter (fn [[_ v]] (> v 1)))
       (count)))

(comment (println (count-overlapping-fabric)))


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
