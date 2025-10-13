(ns aoc2018-4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 정렬된 문자열 시퀀스로 반환"
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)
       (sort)))

(defn- get-guard-id
  "라인에서 가드의 id 를 추출하는 함수"
  [line]
  (->> (str/replace (re-find #"#\d+" line) "#" "")
       (parse-long)))

(defn- get-minute
  "라인에서 분을 추출하는 함수"
  [line]
  (->> (str/replace (re-find #":\d+" line) ":" "")
       (parse-long)))

(defn- create-guard-map
  "문자열 시퀀스를 [guard-id [{:sleep sleep-time :wake-up wake-up-time}]] 로 변환하는 함수"
  [lines]
  (loop
   [result []
    guard-id -1
    sleep-time -1
    [line & more] lines]
    (cond
      (nil? line)
      result

      (str/includes? line "Guard")
      (recur result (get-guard-id line) sleep-time more)

      (str/includes? line "falls asleep")
      (recur result guard-id (get-minute line) more)

      (str/includes? line "wakes up")
      (recur (conj result {guard-id [{:sleep sleep-time :wake-up (get-minute line)}]}) guard-id sleep-time more))))

(defn- calculate-sleep-duration
  "총 잠들어있는 시간을 map 에 추가하는 함수"
  [[guard-id intervals]]
  {:id guard-id
   :sleep-duration (reduce + (map (fn [{:keys [sleep wake-up]}]
                                    (- wake-up sleep)) intervals))
   :value intervals})

(defn- find-spent-minute
  "가장 많이 잠들어있는 분과 횟수을 찾는 함수"
  [input]
  (->> (mapcat (fn [{:keys [sleep wake-up]}]
                 (range sleep wake-up)) input)
       (frequencies)
       (apply max-key val)))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(defn find-sleepiest-guard
  []
  (let [sleepiest-guard (->> (read-lines "2018_4.txt")
                             (create-guard-map)
                             (apply merge-with into)
                             (map calculate-sleep-duration)
                             (apply max-key :sleep-duration))
        sleepiest-id (:id sleepiest-guard)]
    (->> (:value sleepiest-guard)
         (find-spent-minute)
         (key)
         (* sleepiest-id))))

(comment (find-sleepiest-guard))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(defn- multi-id-minute [guard]
  (* (:id guard) (first (:minute guard))))

(defn find-minute-guard
  []
  (->> (read-lines "2018_4.txt")
       (create-guard-map)
       (apply merge-with into)
       (map calculate-sleep-duration)
       (map (fn [guard]
              {:id (:id guard) :minute (find-spent-minute (:value guard))}))
       (apply max-key (comp second :minute))
       (multi-id-minute)))

(comment (find-minute-guard))