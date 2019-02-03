(ns scratchpad.aws
  (:require
   [clojure.java.io          :as io]
   [clojure.data.json        :as json]
   [clojure.tools.logging    :as log]
   [cognitect.aws.client.api :as aws]))

(comment

  (def s3 (aws/client {:api :s3}))
  (aws/validate-requests s3 true)
  

  (aws/ops s3)
  (aws/doc s3 :ListBuckets)

  (->
   s3
   (aws/invoke {:op :ListBuckets})
   :Buckets)


  (def ec2 (aws/client {:api :ec2
                        :region "us-west-2"}))

  (aws/validate-requests ec2 true)

  (aws/ops ec2)
  (-> ec2
      aws/ops
      :DescribeInstances
      :documentation)
  (aws/doc ec2 :DescribeInstances)

  (->>
   (aws/invoke ec2 {:op :DescribeInstances})
   :Reservations
   (mapcat :Instances))


  )
