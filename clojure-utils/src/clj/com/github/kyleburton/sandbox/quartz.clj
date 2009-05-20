;; goals with my evaluation of Quartz are:
;;
;; * create a simple example of creating and scheduling 
;;   an in-process job (print the current time?)
;;   * make a 'clojure' job?
;; * examples of all of the scheduling approaches
;;   * 'in 10 minutes'
;;   * repeat / recurrance
;;   * 'at 8am every monday' (Cron scheduler)
;; * exampls of using persistence for registered jobs
;; * demonstration of what happens if the scheudler is
;;   down during the schedule time - how does it deal 
;;   with this, does it re-execute pending jobs that
;;   missed their invocation points?  Or does that need
;;   to be implemented at an application level?
;; * create a job that sends an AMQP message to a broker
;;   allowing the scheduler to be an abstracted service
;;   invoker
;; * make a registry for these JMS OR AMQP jobs supporting
;;   the above behaviors for robustness
;; * how do you do HA clustering for quartz?
;;

(ns com.github.kyleburton.sandbox.quartz
  (:import (org.quartz SchedulerFactory Scheduler TriggerUtils JobDetail)
           (org.quartz.impl StdSchedulerFactory)
           (com.github.kyleburton.sandbox.quartz ClojureJob)))

(def *schedule-factory* (StdSchedulerFactory.))

(def *scheduler* (atom nil))

(defn ensure-scheduler-started []
  (if (or (not @*scheduler*)
          (.isShutdown @*scheduler*)
          (not (.isStarted @*scheduler*)))
    (do
      (reset! *scheduler* (.getScheduler *schedule-factory*))
      (.start @*scheduler*)
      true)
    nil))

;; (.isShutdown @*scheduler*)

;; (ensure-scheduler-started)

(defn stop-scheduler []
  (if (and @*scheduler*
           (.isStarted @*scheduler*))
    (.shutdown @*scheduler*)))

(defn schedule-job [job-detail trigger]
  (ensure-scheduler-started)
  (.scheduleJob @*scheduler* job-detail trigger))

(defn delete-job [job-detail]
  (.deleteJob @*scheduler*
              (.getName job-detail)
              (.getGroup job-detail)))

(defn job-exists? [job-detail]
  (not (nil? (.getJobDetail @*scheduler*
                            (.getName job-detail)
                            (.getGroup job-detail)))))

;; NB: the function has to be named for ClojureJob to be able to call
;; it.
(defn testfn [context]
  (prn (format "testfn: context=%s time=%s" 
               context
               (java.util.Date.))))

;;(parents (class testfn))

(defn quartz-test []
  (let [job-detail (JobDetail. "myJob" nil ClojureJob)
        trigger (doto (TriggerUtils/makeSecondlyTrigger 10)
                  (.setStartTime (TriggerUtils/getEvenSecondDate (java.util.Date.)))
                  (.setName "My Second Trigger"))]
    (.put (.getJobDataMap job-detail) ClojureJob/NAMESPACE_PARAMETER "com.github.kyleburton.sandbox.quartz")
    (.put (.getJobDataMap job-detail) ClojureJob/FUNCTION_NAME_PARAMETER "testfn")
    (schedule-job job-detail trigger)))

(defn quartz-test-fn [fn]
  (let [job-detail (JobDetail. "myJob" nil ClojureJob)
        trigger (doto (TriggerUtils/makeSecondlyTrigger 10)
                  (.setStartTime (TriggerUtils/getEvenSecondDate (java.util.Date.)))
                  (.setName "My Second Trigger"))]
    (.put (.getJobDataMap job-detail) ClojureJob/NAMESPACE_PARAMETER "com.github.kyleburton.sandbox.quartz")
    (.put (.getJobDataMap job-detail) ClojureJob/FUNCTION_PARAMETER fn)
    (schedule-job job-detail trigger)))

;; (job-exists? (JobDetail. "myJob" nil ClojureJob))
;; (delete-job (JobDetail. "myJob" nil ClojureJob))

;; (quartz-test)

(def *count* (atom 0))

(quartz-test-fn (fn [context] 
                  (reset! *count* (inc @*count*))
                  (prn (format "anon scheduled function! context=%s callled %d times!" context @*count*))))

;; (stop-scheduler)