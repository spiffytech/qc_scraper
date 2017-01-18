FROM mono:3.8

RUN mkdir /data
ADD blah.db /data/blah.db

RUN apt-get update && apt-get install libmono-sqlite4.0-cil

ADD . /src
WORKDIR /src
RUN ./fakebuild.sh

CMD mono --debug build/qc_scraper.exe /data/feed.xml
