FROM ubuntu

MAINTAINER daewon

RUN apt-get update

RUN apt-get install -y memcached

EXPOSE 11211

CMD ["-m", "64"]

USER daemon

ENTRYPOINT memcached

