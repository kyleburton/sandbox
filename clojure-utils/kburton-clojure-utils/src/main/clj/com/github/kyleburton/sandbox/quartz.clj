(ns com.github.kyleburton.sandbox.quartz
  (:import (org.quartz SchedulerFactory Scheduler TriggerUtils JobDetail SimpleTrigger)
           (org.quartz.impl StdSchedulerFactory)
           (com.github.kyleburton.sandbox.quartz ClojureJob)))


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


(defonce *schedule-factory* (StdSchedulerFactory.))

(defonce *scheduler* (atom nil))

(defn ensure-scheduler-started []
  (if (or (not @*scheduler*)
          (.isShutdown @*scheduler*)
          (not (.isStarted @*scheduler*)))
    (do
      (reset! *scheduler* (.getScheduler *schedule-factory*))
      (.start @*scheduler*)
      true)
    nil))

(defn stop-scheduler []
  (if (and @*scheduler*
           (.isStarted @*scheduler*))
    (.shutdown @*scheduler*)))

(defn schedule-job [job-detail trigger]
  (ensure-scheduler-started)
  (.scheduleJob @*scheduler* job-detail trigger))

(def *defualt-group-name* "DEFAULT")

(defn unschedule-by-name [job-name & [group-name]]
  (.unscheduleJob @*scheduler* job-name (or group-name *defualt-group-name*))
  (.deleteJob @*scheduler* job-name (or group-name *defualt-group-name*)))

(defn delete-job [job-detail]
  (.deleteJob @*scheduler*
              (.getName job-detail)
              (.getGroup job-detail)))

(defn job-exists? [job-detail]
  (not (nil? (.getJobDetail @*scheduler*
                            (.getName job-detail)
                            (.getGroup job-detail)))))

(defn testfn [context]
  (prn (format "testfn: context=%s time=%s" 
               context
               (java.util.Date.))))

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

(defmulti trigger-in (fn [tag val] tag))

(defmethod trigger-in :seconds [tag val]
  (SimpleTrigger. (format "Trigger in %s seconds." val) 
                  *defualt-group-name*
                  (TriggerUtils/getEvenSecondDate (java.util.Date.))))

(defmethod trigger-in :minutes [tag val]
  (SimpleTrigger. (format "Trigger in %s minutes." val) 
                  *defualt-group-name*
                  (TriggerUtils/getEvenMinuteDate (java.util.Date.))))

(defmethod trigger-in :hours [tag val]
  (SimpleTrigger. (format "Trigger in %s hours." val) 
                  *defualt-group-name*
                  (TriggerUtils/getEvenHourDate (java.util.Date.))))


(comment

  (seq (.getTriggerGroupNames @*scheduler*))
  (seq (.getJobGroupNames @*scheduler*))
  (seq (.getCurrentlyExecutingJobs @*scheduler*))
  (seq (.getCalendarNames @*scheduler*))

  (first (seq (.getCurrentlyExecutingJobs @*scheduler*)))

  (seq (.getJobNames @*scheduler* "DEFAULT")) ;; => ("every 10s")
  (seq (.getTriggersOfJob @*scheduler* "every 10s" "DEFAULT"))
  (.getFullName (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (.getName (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (.getGroup (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (.getKey (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (.isDurable (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (.isStateful (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (.isVolatile (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
  (seq (.getJobListenerNames (.getJobDetail @*scheduler* "every 10s" "DEFAULT")))
  (.unscheduleJob @*scheduler* 
                    (.getFullName (.getJobDetail @*scheduler* "every 10s" "DEFAULT"))
                    (.getGroup (.getJobDetail @*scheduler* "every 10s" "DEFAULT")))
  (.deleteJob @*scheduler*  "every 10s" "DEFAULT")

  (seq (.getTriggerNames @*scheduler* "DEFAULT"))

  (.unscheduleJob @*scheduler* "Trigger every 10 seconds." "DEFAULT")


  (trigger-in :seconds 10)

  (trigger-in :minutes 10)

  (trigger-in :hours 10)

)

(defmulti  now+ (fn [tag val] tag))
(defmethod now+ :seconds  [tag val] (java.util.Date. (+ (*       1000 val) (.getTime (java.util.Date.)))))
(defmethod now+ :minutes  [tag val] (java.util.Date. (+ (*    60 1000 val) (.getTime (java.util.Date.)))))
(defmethod now+ :hours    [tag val] (java.util.Date. (+ (*  3600 1000 val) (.getTime (java.util.Date.)))))
(defmethod now+ :days     [tag val] (java.util.Date. (+ (* 86400 1000 val) (.getTime (java.util.Date.)))))

(defmulti trigger-every (fn [tag start] tag))

(defmethod trigger-every :seconds [tag interval]
  (doto (TriggerUtils/makeSecondlyTrigger interval)
    (.setStartTime (TriggerUtils/getEvenMinuteDate (now+ :seconds interval)))
    (.setName (format "Trigger every %s seconds." interval))))

(defmethod trigger-every :minute [tag start]
  (doto (TriggerUtils/makeMinutelyTrigger)
    (.setStartTime (TriggerUtils/getEvenMinuteDate start))
    (.setName (format "Trigger every %s minutes." val))))

(defmethod trigger-every :hour [tag start]
  (doto (TriggerUtils/makeHourlyTrigger)
    (.setStartTime (TriggerUtils/getEvenHourDate start))
    (.setName (format "Trigger every %s hours." val))))

(comment

  (trigger-every :minute (now+ :minutes 3))

  (trigger-every :hour (now+ :seconds 0))

)

;; TODO: there is no TriggerUtils/getEvenDayDate, implement a `trigger every day'
;; (defmethod trigger-every :day [tag val]
;;   (doto (TriggerUtils/makeDailyTrigger)
;;     (.setStartTime (TriggerUtils/getEvenMinuteDate start))
;;     (.setName (format "Trigger in %s minutes." val))))


;; TODO: TriggerUtils supports various `getDateOf' functions to get a
;; specific date, create a `trigger-at' which is a 1time trigger for
;; the given date/time





;; (quartz-test)

;; (def *count* (atom 0))

;; (quartz-test-fn (fn [context] 
;;                   (reset! *count* (inc @*count*))
;;                   (prn (format "anon scheduled function! context=%s called %d times!" context @*count*))))

;; (stop-scheduler)