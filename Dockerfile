FROM haskell:8.0

RUN cabal update && mkdir /opt/app

WORKDIR /opt/app

ADD LICENSE LICENSE
ADD haskell-hashtag-viewer.cabal .

RUN cabal update && cabal install --only-dependencies -j$(nproc)

ADD src src
RUN cabal build -j$(nproc)
ADD public public

