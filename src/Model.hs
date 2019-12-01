{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Model where

import Control.Exception
import Data.Text (Text, unpack)
import Network.HTTP.Conduit
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Aeson

podcastDir :: String
podcastDir = "podcasts/"

data PodcastError
  = PodcastNotFound Name
  | MalformedXml
  | MissingTag String
  | MissingAttr String
  | IOWrapper IOException
  deriving (Show)

instance Exception PodcastError

data PodcastState = PodcastState
  { name :: Text
  , epNum :: Int
  } deriving (Generic, ToJSON, FromJSON)

data Podcast = Podcast
  { title :: String
  , description :: String
  , url :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

type Name = Text
type Podcasts = Map.Map Name String
type PodcastResult = Either PodcastError

liftEither :: Either PodcastError a -> IO a
liftEither (Right a) = pure a
liftEither (Left a) = throw a

podcasts :: Podcasts
podcasts = Map.fromList
  [ ("eng", "http://historyofenglishpodcast.com/feed/podcast/" )
  , ("byz", "https://rss.acast.com/thehistoryofbyzantium")
  ]

requestPodcast :: String -> IO String
requestPodcast url = fmap (Char8.unpack) (simpleHttp url)

getPodcastUrl :: Text -> PodcastResult String
getPodcastUrl podcastName = case (Map.lookup podcastName podcasts) of
  Nothing -> Left $ PodcastNotFound podcastName
  Just v -> pure v

xmldoc = do
  str <- readFile "eng.xml"
  case parseXMLDoc str of
    Nothing -> error "bad"
    Just x -> pure x

getMp3Links :: Element -> PodcastResult [Podcast]
getMp3Links doc = getItems doc >>= mapM getPodcastInfo

xmlName :: String -> QName
xmlName tagName = QName tagName Nothing Nothing

byTag :: String -> Element -> PodcastResult Element
byTag tagName doc = case filterElementName ((==) (xmlName tagName)) doc of
  Just x -> pure x
  Nothing -> Left $ MissingTag tagName

attr :: String -> Element -> PodcastResult String
attr attrName doc = case findAttr (xmlName attrName) doc of
  Just x -> pure x
  Nothing -> Left $ MissingAttr attrName

getPodcastInfo :: Element -> PodcastResult Podcast
getPodcastInfo doc = do
  title <- fmap strContent $ byTag "title" doc
  description <- fmap strContent $ byTag "description" doc
  url <- byTag "enclosure" doc >>= attr "url"
  pure $ Podcast title description url

getItems :: Element -> PodcastResult [Element]
getItems doc = case filterElementsName isItem doc of
  [] -> Left MalformedXml
  is -> pure is
  where
    isItem = (==) (xmlName "item")

parsePodcastDoc :: String -> PodcastResult Element
parsePodcastDoc doc = case (parseXMLDoc doc) of
  Nothing -> Left MalformedXml
  Just x -> pure x

getMp3sForPodcast :: Text -> IO [Podcast]
getMp3sForPodcast podcastName = do
  url <- liftEither $ getPodcastUrl podcastName
  body <- requestPodcast url
  doc <- liftEither $ parsePodcastDoc body
  liftEither $ getMp3Links doc

getEpNumber :: Text -> IO Int
getEpNumber = fmap read . readFile . (++) podcastDir . unpack

listEpNums :: IO [PodcastState]
listEpNums = mapM readPair (Map.toList podcasts)
  where
    readPair (k, _) = do
      n <- (getEpNumber k)
      pure (PodcastState k n)

incEpNum :: Text -> IO ()
incEpNum podcastName = do
  !num <- getEpNumber podcastName
  writeFile (podcastDir ++ unpack podcastName) (show $ num + 1)

podcast :: Text -> IO Podcast
podcast podcastName = do
  links <- getMp3sForPodcast podcastName
  num <- getEpNumber podcastName
  pure $ links !! ((length links) - num)

