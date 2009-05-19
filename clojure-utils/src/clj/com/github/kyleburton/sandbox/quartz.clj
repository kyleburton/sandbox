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

(def *scheduler* (.getScheduler *schedule-factory*))

(.start *scheduler*)

(def *min-trigger* (doto (TriggerUtils/makeMinutelyTrigger)
                     (.setStartTime (TriggerUtils/getEvenMinuteDate (java.util.Date.)))
                     (.setName "My Minute Trigger")))

;; myJob, no-group (nil)
(def job-detail (JobDetail. "myJob" nil ClojureJob))

(defn testfn []
  (prn (str "within testfn, the time is: " (java.util.Date.))))

(.put (.getJobDataMap job-detail) "job.clojure.namespace" "com.github.kyleburton.sandbox.quartz")
(.put (.getJobDataMap job-detail) "job.clojure.function" "testfn")


;; this works:
;; (.scheduleJob *scheduler* job-detail *min-trigger*)

;; TODO: determine how to pass arguments into the function!


