(ns aoc2018-6
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 라인 단위의 문자열 시퀀스로 반환합니다."
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)))

(defn- parse-coordinate
  "x, y 형식의 좌표 문자열을 파싱하여, x y 키를 갖는 맵으로 변환합니다."
  [index coordinates-string]
  (let [[x y] (-> (str/split coordinates-string #", "))]
    {:x (parse-long x)
     :y (parse-long y)
     :id index}))

(defn- calculate-bounding-box
  "좌표 맵의 컬렉션을 입력받아, 모든 좌표를 감싸는 bounding box 정보를 반환합니다."
  [full-coordinates]
  (let [xs (map :x full-coordinates)
        ys (map :y full-coordinates)]
    {:min-x (apply min xs)
     :min-y (apply min ys)
     :max-x (apply max xs)
     :max-y (apply max ys)}))

(defn- create-grid
  "두 좌표 사이의 맨해튼 거리(Manhattan distance)를 계산합니다."
  [{:keys [min-x min-y max-x max-y]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    {:x x :y y}))

(defn- manhattan-distance-between
  "좌표 2개를 받아서 멘하튼 거리를 반환합니다."
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

;; 작성중
(defn- calculate-area [full-coordinates grid]
  (reduce (fn [coordinates current-grid]
            (println (reduce (fn [min-distance coordinate]
                               (min min-distance (manhattan-distance-between current-grid coordinate)))
                             Integer/MAX_VALUE
                             coordinates)))
          full-coordinates
          grid))


(let [full-coordinates (->> (read-lines "2018_6.txt")
                            (map-indexed parse-coordinate))
      bounding-box (calculate-bounding-box full-coordinates)]
  (->> (create-grid bounding-box)
       (calculate-area full-coordinates)))


;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.
