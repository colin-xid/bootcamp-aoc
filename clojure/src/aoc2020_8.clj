(ns aoc2020-8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; 파트 1

;; 휴대용 게임 콘솔의 부트 코드가 무한 루프에 빠져 실행이 멈추지 않는다.
;; 각 명령은 다음 세 가지 중 하나로 구성된다:
;; - acc (accumulate): 누산기(accumulator)의 값을 인자로 증가/감소시키고 다음 줄로 이동
;; - jmp (jump): 현재 위치로부터 상대적 오프셋만큼 점프
;; - nop (no operation): 아무 일도 하지 않고 다음 줄로 이동

;; 프로그램은 첫 줄부터 시작하며, 동일한 명령을 두 번째로 실행하려는 순간
;; 무한 루프에 빠졌음을 의미한다.

;; 문제: 동일한 명령이 두 번째로 실행되기 직전, 누산기(accumulator)의 값은 얼마인가?

;; 예시 입력:
;;   nop +0
;;   acc +1
;;   jmp +4
;;   acc +3
;;   jmp -3
;;   acc -99
;;   acc +1
;;   jmp -4
;;   acc +6

;; 위 예시의 경우, 무한 루프 직전 누산기 값은 5이다.

(defn- read-lines
  "resource-path 에 있는 파일을 읽어 문자열 시퀀스로 반환합니다."
  [resource-path]
  (->> (slurp (io/resource resource-path))
       (str/split-lines)
       (remove str/blank?)))

(defn- parse-boot-code
  "string 명령어를 operation, argument, id 로 구조화하여 반환합니다."
  [index command-line]
  (let [[operation argument] (str/split command-line #" ")]
    {:operation operation
     :argument (parse-long argument)
     :id index}))

(defn- run-operation
  "입력으로 들어온 명령을 실행합니다."
  [{:keys [operation argument]} accumulator index]
  (case operation
    "acc" {:next-acc (+ accumulator argument) :next-index (inc index)}
    "nop" {:next-acc accumulator :next-index (inc index)}
    "jmp" {:next-acc accumulator :next-index (+ index argument)}))

(defn find-loop-accumulator
  "동일한 명령이 두 번째로 실행되기 직전의 accumulator 를 반환합니다."
  [boot-codes]
  (let [instructions (->> boot-codes
                          (map-indexed parse-boot-code)
                          (vec))]
    (loop [index 0
           current-acc 0
           visited-ids #{}]
      (let [{:keys [next-index next-acc]} (-> (get instructions index)
                                              (run-operation current-acc index))]
        (if (visited-ids index)
          current-acc
          (recur next-index next-acc (conj visited-ids index)))))))

(comment
  (->> (read-lines "2020_8.txt")
       (find-loop-accumulator)))

;; 파트 2

;; 프로그램의 무한 루프 원인을 분석한 결과, 단 하나의 명령어가 잘못되어 있음이 밝혀졌다.
;; 즉, 어딘가에서 `jmp` 명령은 `nop`이 되어야 하고, 혹은 `nop` 명령이 `jmp`로 잘못되어 있다.

;; 프로그램은 정상적으로 종료될 경우, 마지막 명령 바로 다음 줄(= 파일 끝)을 실행하려고 시도하며 종료된다.
;; 따라서 정확히 하나의 `jmp` 또는 `nop`을 수정해 프로그램이 정상 종료되도록 해야 한다.

;; 예시 입력:
;;   nop +0
;;   acc +1
;;   jmp +4
;;   acc +3
;;   jmp -3
;;   acc -99
;;   acc +1
;;   jmp -4
;;   acc +6

;; 위 프로그램에서 `jmp -4`를 `nop -4`로 바꾸면 프로그램이 정상 종료된다.
;; 이 경우 실행 순서는 다음과 같다:

;;   nop +0  | 1
;;   acc +1  | 2
;;   jmp +4  | 3
;;   acc +3  |
;;   jmp -3  |
;;   acc -99 |
;;   acc +1  | 4
;;   nop -4  | 5
;;   acc +6  | 6

;; 프로그램은 마지막 줄을 실행한 후 정상적으로 종료되며,종료 시 누산기(accumulator)의 최종 값은 8이다.
;; 하나의 `jmp` `nop`을 바꿔 프로그램이 정상 종료되도록 만들었을 때, 종료 시 누산기(accumulator)의 값은 얼마인가?

(defn- change-operation
  "id 가 target-id 인 명령을 jmp <-> nop 으로 변경합니다."
  [{:keys [id operation] :as instruction} target-id]
  (if (= id target-id)
    (case operation
      "jmp" (assoc instruction :operation "nop")
      "nop" (assoc instruction :operation "jmp")
      "acc" instruction)
    instruction))

(defn find-terminating-acc
  "`jmp` `nop` 중 하나를 바꿔서 프로그램이 정상 종료될 때의 accumulator 값을 반환합니다."
  [boot-codes]
  (let [instructions (->> boot-codes
                          (map-indexed parse-boot-code)
                          (vec))
        nop-jmp-ids (->> instructions
                         (filter #(#{"nop" "jmp"} (:operation %)))
                         (map :id))]
    (loop [index 0
           current-acc 0
           visited-ids #{}
           [flipped-id & rest-ids] nop-jmp-ids]
      (let [{:keys [next-index next-acc]} (-> (get instructions index)
                                              (change-operation flipped-id)
                                              (run-operation current-acc index))]
        (cond
          (= next-index (count instructions))
          current-acc

          (visited-ids index)
          (recur 0 0 #{} rest-ids)

          :else
          (recur next-index next-acc (conj visited-ids index) (cons flipped-id rest-ids)))))))

(comment
  (->> (read-lines "2020_8.txt")
       (find-terminating-acc)))