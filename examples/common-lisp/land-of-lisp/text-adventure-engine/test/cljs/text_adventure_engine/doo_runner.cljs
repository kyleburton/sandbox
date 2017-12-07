(ns text-adventure-engine.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [text-adventure-engine.core-test]))

(doo-tests 'text-adventure-engine.core-test)

