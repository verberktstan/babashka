(ns babashka.impl.clojure.main-test
  (:require [babashka.test-utils :as tu]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing]]))

(def bb
  (comp edn/read-string tu/bb))

(deftest with-read-known-test
  (testing ":unknown gets set to true"
    (is (true? (bb nil (pr-str '(binding [*read-eval* :unknown]
                                  (clojure.main/with-read-known *read-eval*)))))))
  (testing "other values don't change"
    (t/are [read-eval-value]
      (= read-eval-value
        (bb nil (str "(binding [*read-eval* " read-eval-value "]"
                  " (clojure.main/with-read-known *read-eval*))")))
      false true 5)))

(def not-starts-with? (complement str/starts-with?))

(def boom
  (bb nil (pr-str '(let [triage (clojure.main/ex-triage (Throwable->map (ex-info "boom!" {})))]
                     {:triage triage
                      :str    (clojure.main/ex-str triage)}))))

(deftest ex-triage-test
  (let [result (:triage boom)]
    (testing "ex-triage reports map with phase, cause and class (at minimum)"
      (is (= {:clojure.error/phase :execution
              :clojure.error/cause "boom!"
              :clojure.error/class 'clojure.lang.ExceptionInfo}
             (select-keys
              result
              [:clojure.error/phase :clojure.error/cause :clojure.error/class]))))

    (testing "sci frames are filtered symbol should not contain a sci.* namespace"
      ;; core-class? is extended to skip sci.* frames so that SCI interpreter
      ;; internals don't appear as the reported call site.
      (when-let [sym (:clojure.error/symbol result)]
        (is (not (str/starts-with? (namespace sym) "sci"))))))

  (testing "explicit phase in datafied throwable is honoured"
    (is (= :read-source
           (bb nil (pr-str '(:clojure.error/phase
                             (clojure.main/ex-triage
                              {:via   [{:type    'clojure.lang.ExceptionInfo
                                        :message "oops"
                                        :data    {}}]
                               :trace []
                               :phase :read-source})))))))

  (testing "works with plain RuntimeException"
    (is (= "plain error"
           (bb nil (pr-str '(:clojure.error/cause
                             (clojure.main/ex-triage
                              (Throwable->map
                               (try (throw (RuntimeException. "plain error"))
                                    (catch Throwable e e)))))))))))

(deftest ex-str-test
  (let [result (:str boom)]
    (testing "returns a string"
      (is (string? result)))

    (testing "output starts with Execution error for :execution phase"
      (is (str/starts-with? result "Execution error")))

    (testing "output contains the cause message"
      (is (str/includes? result "boom!"))))

  (testing "accepts pre-built triage map directly"
    (is
     (string?
      (bb nil (pr-str '(clojure.main/ex-str
                        {:clojure.error/phase :execution
                         :clojure.error/cause "something went wrong"
                         :clojure.error/class 'java.lang.Exception})))))))

(deftest err->msg-test
  (testing "returns a string from a Throwable"
    (let [result (bb nil (pr-str '(clojure.main/err->msg
                                   (try (throw (ex-info "kaboom" {}))
                                        (catch Throwable e e)))))]
      (is (string? result))
      (is (str/includes? result "kaboom"))
      (is (str/starts-with? result "Execution error")))))
