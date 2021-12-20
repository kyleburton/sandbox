# FROM phusion/baseimage:0.9.15
FROM ubuntu:16.04
#FROM ubuntu:xenial-20160125

RUN apt-get update -y                                 \
 && apt-get install -y curl                          

RUN echo "LAYER: DOWNLOADS"                           \
 && mkdir -p /opt/perforce                            \
 && curl -o /opt/perforce/helix-versioning-engine.tgz \
         "http://cdist2.perforce.com/perforce/r15.2/bin.linux26x86_64/helix-versioning-engine.tgz" \
 && curl -o /opt/perforce/sdp.Unix.2015.2.16638.tgz   \
         "https://swarm.workshop.perforce.com/projects/perforce-software-sdp/download/downloads/sdp.Unix.2015.2.16638.tgz"


RUN echo "LAYER: file system"                          \
 && mkdir -p /metadata                                 \
 && mkdir -p /logs                                     \
 && mkdir -p /p4                                       \
 && mkdir -p /depodata/sdp                             \
 && mkdir -p /p4/1/root                                \
 && mkdir -p /p4/1/logs                                \
 && touch    /p4/1/logs/journal                       

RUN echo "LAYER: unarchive perforce"                   \
 && tar -xzf /opt/perforce/helix-versioning-engine.tgz \
        -C /opt/perforce                               \
 && tar -xzf /opt/perforce/sdp.Unix.2015.2.16638.tgz   \
        -C /opt/perforce


# TODO: set up sdp?
# RUN  bash sdp-install.sh 1

EXPOSE 1666
EXPOSE 1667

CMD ["/opt/perforce/p4d", "-r", "/p4/1/root", "-J", "/p4/1/logs/journal" ]

