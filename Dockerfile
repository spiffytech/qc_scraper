FROM thaiphan/mono:3.6.0.39

RUN mkdir /data
ADD blah.db /data/blah.db

ADD build /build

CMD cd /data && mono --debug /build/qc_scraper.exe /data/feed.xml
