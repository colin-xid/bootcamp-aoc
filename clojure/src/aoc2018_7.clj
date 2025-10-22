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
  "완료된 step 을 제거하고, 다른 step 의 :parent 목록에서 제거합니다."
  [graph complite-step]
  (reduce
   (fn [acc [key _]]
     (if (= key complite-step)
       (dissoc acc key)
       (update-in acc [key :parent] disj complite-step)))
   graph
   graph))

(defn- determine-step-order
  "주어진 그래프를 수행 가능한 순서대로 반환합니다."
  [step-graph]
  (loop [ordered-step nil
         steps step-graph]
    (if (= (count ordered-step) (count step-graph))
      ordered-step
      (let [next-step (next-available-step steps)
            updated-order (str ordered-step next-step)
            remain-steps (complete-step steps next-step)]
        (recur updated-order remain-steps)))))

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

(defn- fill-workers
  "작업 목록에 step key 와 소요시간을 할당합니다."
  [workers steps]
  (->> (loop [workers (into {} (filter #(not (= 0 (val %))) workers))]
         (let [next-step (find-next-available-step steps (keys workers))]
           (if (and next-step (< (count workers) 5))
             (recur (assoc workers next-step (step-duration next-step)))
             workers)))))

;; reduce 를 단순 순회로 사용한 점이 아쉬움
;; 용도에 맞는 방법을 찾아서 리펙터링 해보기
(defn- process-work
  "작업을 min duration 만큼 실행합니다."
  [min-duration filled-workers]
  (reduce (fn [acc [step-key step-duration]]
            (assoc acc step-key (- step-duration min-duration)))
          filled-workers
          filled-workers))

;; docstring 만 봤을 때 어떤 값이 들어와서 어떻게 나가는지 헷갈림
;; 입출력 예시를 넣으면 이해하기 좋을듯
(defn- remove-complete-steps
  "여러 완료된 step 들을 순서대로 제거합니다."
  [graph to-remove]
  (reduce
   (fn [acc step]
     (complete-step acc step))
   graph
   (keys to-remove)))

;; 전체적으로 복잡해서 눈으로 따라가기 힘듦
;; loop 대신 iterate , take-while 을 사용하는 방법이 있음
;; 다른 사람들이 작성한 코드 참고해보기
;; loop 가 많다는건 절차지향적이라는 신호
(defn- calculate-working-duration
  [workers step-graph]
  (loop [duration 0
         workers workers
         graph step-graph]
    (let [filled-workers (fill-workers workers graph)]
      (if (empty? filled-workers)
        duration
        (let [min-duration (apply min (vals filled-workers))
              remain-work (process-work min-duration filled-workers)]
          (->> (filter #(= 0 (val %)) remain-work)
               (remove-complete-steps graph)
               (recur (+ duration min-duration) remain-work)))))))

(->> (read-lines "2018_7.txt")
     (build-step-graph)
     (calculate-working-duration {}))