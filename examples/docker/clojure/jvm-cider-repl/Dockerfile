FROM java:8-jdk

EXPOSE 4014
# EXPOSE 1099
# EXPOSE 80
# EXPOSE 443
# EXPOSE 8000
# EXPOSE 8080


RUN apt-get update && apt-get -y install lsof curl \
    && curl -o ./lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein \
    && chmod 755 ./lein

COPY project.clj project.clj
COPY src/ src/

RUN ./lein install

COPY run.sh run.sh
ENTRYPOINT ["/bin/bash", "run.sh"]
