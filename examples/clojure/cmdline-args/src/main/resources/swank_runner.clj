
(require 'swank.swank)
(swank.swank/ignore-protocol-version "2009-08-19")
(swank.swank/start-server "/tmp/algoconnect-services.port" :encoding "iso-latin-1-unix" :port 4005)
