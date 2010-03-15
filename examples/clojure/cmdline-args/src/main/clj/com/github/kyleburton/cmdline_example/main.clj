(ns com.github.kyleburton.cmdline-example.main
  (:use
     clojure.contrib.command-line)
  (:gen-class))

(defn -main [& args]
  (with-command-line
    args
    "Usage: program [options]..."
    [[file     "Input file"       nil]
     [output   "Output file"      nil]
     [invert?  "Invert the logic" nil]]
    (prn (format "file=%s output=%s invert?=%s" file output invert?))))
