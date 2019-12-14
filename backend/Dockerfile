FROM haskell:8.2.2

COPY podcast-service.cabal podcast-service.cabal

RUN cabal update 
RUN cabal install --only-dependencies -j4

COPY src src
COPY app app
COPY test test
COPY LICENSE LICENSE

RUN cabal install
