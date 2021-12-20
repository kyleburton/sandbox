PROJECTS="clj-etl-utils clorine impresario jrclj krbemacs dev-utils cassandra-sandbox teporingo clj-xpath perCEPtor system-utils clj-bloom credit_card_validator impresario lein-marginalia lein-margauto twilio-in-ten-minutes tellmewhen newflow abstract-tables large-data-and-clojure base-app jbit clj-lfsr presenting-chef-solo fuzzy-string typrtail cucumber-example adeventures-in-etl krb-bash-utils kyles-secret-interviewing-techniques intro-to-genetic-algorithms ruby-data-fu introduction-to-git"

for p in $PROJECTS; do
  cd $HOME/personal/projects
  if [ -d $p ]; then
    cd $p; git pob
  else
    git clone git@github.com:kyleburton/$p.git
  fi
done
