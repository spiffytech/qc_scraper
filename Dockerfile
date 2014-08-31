FROM thaiphan/mono:3.6.0.39

ADD build /build
ADD blah.db /build/blah.db

CMD cd /build && mono --debug qc_scraper.exe
