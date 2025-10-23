(ns aoc2018-7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 문자열 시퀀스로 반환합니다."
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)))

;; 파트 1

;; 스케줄이 주어질 때, 일이 처리되는 순서를 반환하시오.
;; 알파벳 캐릭터 하나로 대표되는 일(work)이 주어지고, 각 일을 처리하기 위해서 선행되어야 하는 일들이 스케줄 형식으로 주어짐.

;; ```
;; Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.
;; ```

;; 위와 같은 입력의 경우 아래 형태의 그래프가 만들어짐.

;; ```
;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----
;; ```

;; 순서는 아래와 같음.
;; - 처음엔 C만 가능함. C에서 시작. 만약 다른 시작점이 존재한다면 알파벳 순서로 진행.
;; - C 다음으로 A와 F가 가능한데, A가 알파벳 우선순위가 높으므로 A로 감.
;; - A 를 완료하면 B, D, F가 가능한데, 역시 알파벳 우선순위가 높은 B가 수행됨.
;; - 남은 D와 F중에서 D가 수행됨
;; - F가 수행됨
;; - E가 수행됨 (E는 B, D, F 모두가 선행되어야 수행될 수 있음)

;; 결과: `CABDFE`

(defn- parse-step
  "지침 문자열에서 시작 step 과 종료 step 을 추출하여 반환합니다."
  [instruction]
  (let [[_ start end] (re-matches
                       #"Step (.) must be finished before step (.) can begin."
                       instruction)]
    {:start start :end end}))

(defn- add-step-edge
  "단일 지침을 기반으로 그래프에 edge 를 추가합니다."
  [acc {:keys [start end]}]
  (-> acc
      (update-in [start :child] (fnil conj (sorted-set)) end)
      (update-in [end :parent] (fnil conj (sorted-set)) start)))

(defn- has-no-parent?
  "주어진 step 에 :parent 가 없는지 확인합니다."
  [[_ step]]
  (-> (:parent step)
      empty?))

(defn- next-available-step
  "가장 먼저 수행할 수 있는 step 을 반환합니다."
  [steps]
  (->> (filter has-no-parent? steps)
       (map key)
       (first)))

(defn- complete-step
  "완료된 step 을 제거하고, 다른 step 의 :parent 목록에서 제거합니다.
   
   input
   graph : {A {:parrent B C D}, B {:parrent }}
   complete-step : B

   output
   {A {:parrent C D}}
   "
  [graph complete-step]
  (->> (remove (fn [[step-key _]] (= step-key complete-step)) graph)
       (map
        (fn [[step-key {parents :parent}]]
          {step-key {:parent (disj parents complete-step)}}))
       (into (sorted-map))))

(defn process-step
  "현재 그래프에서 다음 실행 가능한 step 을 선택하고, 해당 step 을 완료 처리한 그래프를 반환합니다."
  [{graph :graph}]
  (let [next-step (next-available-step graph)]
    {:next-step next-step
     :graph (complete-step graph next-step)}))

(defn- determine-step-order
  "주어진 그래프를 수행 가능한 순서대로 반환합니다."
  [step-graph]
  (->> {:next-step ""
        :graph step-graph}
       (iterate process-step)
       (take-while #(:next-step %))
       (map :next-step)
       (apply str)))

(defn- build-step-graph
  "지침 문자열 컬렉션을 step 그래프로 변환합니다."
  [instructions]
  (->> (map parse-step instructions)
       (reduce add-step-edge {})
       (into (sorted-map))))

(->> (read-lines "2018_7.txt")
     (build-step-graph)
     (determine-step-order))

;; 파트 2

;; 파트 1에서는 일을 워커(worker)가 하나였지만, 파트 2는 5명. 즉, 동시에 5개의 일을 처리할 수 있게 됨.
;; 그리고 각각의 일 (A\~Z)은 처리하는데 (60+1\~60+26)의 시간이 걸림. B는 62초, D는 64초, etc.

;; 이 때, 주어진 모든 일을 처리하는데 걸리는 시간을 구하시오.

;; 예)

;; 아래는 파트 1의 예시에서 워커가 2명이 된 케이스이다.
;; ```
;; Second   Worker 1   Worker 2   Done
;;    0        C          .        
;;    1        C          .        
;;    2        C          .        
;;    3        A          F       C
;;    4        B          F       CA
;;    5        B          F       CA
;;    6        D          F       CAB
;;    7        D          F       CAB
;;    8        D          F       CAB
;;    9        D          .       CABF
;;   10        E          .       CABFD
;;   11        E          .       CABFD
;;   12        E          .       CABFD
;;   13        E          .       CABFD
;;   14        E          .       CABFD
;;   15        .          .       CABFDE
;; ```
;; 15초가 걸리므로 답은 15

(defn- find-next-available-step
  "작업중인 스탭을 제외하고 parent 가 없는 스탭을 반환합니다."
  [steps working-steps]
  (->> (remove (fn [[step-key _]]
                 (contains? (set working-steps) step-key)) steps)
       (next-available-step)))

(defn- step-duration
  "문자열 A-Z 를 받아 작업에 걸리는 시간인 61-86 을 리턴합니다."
  [step]
  (- (int (first step)) 4))

(defn- assignable?
  "작업자에게 할당 가능 여부를 반환합니다."
  [next-step active-workers]
  (and next-step (< (count active-workers) 5)))

(defn- assign-workers
  "작업 목록에 할당 가능한 step 과 소요시간을 할당합니다."
  [workers steps]
  (->> (loop [active-workers (->> workers
                                  (filter #(not (= 0 (val %))))
                                  (into {}))]
         (let [next-step (find-next-available-step steps (keys active-workers))]
           (if (assignable? next-step active-workers)
             (recur (assoc active-workers next-step (step-duration next-step)))
             active-workers)))))

(defn- process-work
  "모든 작업자에게 min duration 을 동일하게 소비해 남은 시간을 갱신합니다.
   
   input
   filled-workers : {E 65, F 66, M 73, T 80}
   min-duration : 65

   output
   {E 0, F 1, M 8, T 15}
   "
  [min-duration filled-workers]
  (->> (map (fn [[step-key step-duration]]
              {step-key (- step-duration min-duration)})
            filled-workers)
       (into {})))

(defn- remove-complete-steps
  "완료된 step 들을 그래프에서 제거합니다.

   input
   graph : {A {:parrent B C D}, B {:parrent }, C {:parrent A D}}
   to-remove : A B

   output
   {C {:parrent A D}}
   "
  [graph to-remove]
  (reduce
   (fn [acc step]
     (complete-step acc step))
   graph
   to-remove))

(defn- do-work
  "worker 에 작업을 할당하고 하나가 끝날때까지 작업을 수행합니다."
  [{:keys [duration workers graph]}]
  (let [filled-workers (assign-workers workers graph)
        min-duration (apply min (vals filled-workers))
        remain-workers (process-work min-duration filled-workers)]
    {:duration (+ min-duration duration)
     :workers remain-workers
     :graph (->> (filter #(= 0 (val %)) remain-workers)
                 (keys)
                 (remove-complete-steps graph))}))

(defn- calculate-working-duration
  "작업을 모두 완료하는데 걸리는 시간을 계산합니다."
  [workers step-graph]
  (->> {:duration 0
        :workers workers
        :graph step-graph}
       (iterate do-work)
       (drop-while #(seq (:graph %)))
       first
       :duration))

(->> (read-lines "2018_7.txt")
     (build-step-graph)
     (calculate-working-duration {}))