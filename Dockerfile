FROM haskell:8

COPY . .

RUN stack build
