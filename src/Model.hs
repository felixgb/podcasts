{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

module Model where

import Control.Exception
import Network.HTTP.Conduit
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import qualified Data.ByteString.Lazy.Char8 as Char8
import GHC.Generics
import Data.Aeson
import Control.Parallel.Strategies

podcastDir :: String
podcastDir = "podcasts/"

data PodcastError
  = PodcastNotFound String
  | MalformedXml
  | MissingTag String
  | MissingAttr String
  | IOWrapper IOException
  deriving (Show)

instance Exception PodcastError

data Podcast = Podcast
  { title :: String
  , description :: String
  , url :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data PodcastsState = PodcastsState
  { eps :: [Podcast]
  , name :: String
  , epNum :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

type PodcastResult = Either PodcastError

liftEither :: Either PodcastError a -> IO a
liftEither (Right a) = pure a
liftEither (Left a) = throw a

podcasts :: [(String, String)]
podcasts =
  [ ("eng", "http://historyofenglishpodcast.com/feed/podcast/" )
  , ("byz", "https://rss.acast.com/thehistoryofbyzantium")
  -- , ("iot", "https://podcasts.files.bbci.co.uk/b006qykl.rss")
  , ("enghist", "https://historyofenglishpodcast.com/feed/podcast/")
  ]

requestPodcast :: String -> IO String
-- requestPodcast url = fmap (Char8.unpack) (simpleHttp url)
requestPodcast url = readFile (name ++ ".xml")
  where
    Just name = lookup url [(v, k) | (k, v) <- podcasts]

getPodcastUrl :: String -> PodcastResult String
getPodcastUrl podcastName = case (lookup podcastName podcasts) of
  Nothing -> Left $ PodcastNotFound podcastName
  Just v -> pure v

getMp3Links :: Element -> PodcastResult [Podcast]
getMp3Links doc = getItems doc >>= mapM getPodcastInfo >>= pure . reverse

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

getMp3sForPodcast :: String -> IO [Podcast]
getMp3sForPodcast podcastName = do
  url <- liftEither $ getPodcastUrl podcastName
  body <- requestPodcast url
  doc <- liftEither $ parsePodcastDoc body
  liftEither $ getMp3Links doc

getEpNumber :: String -> IO Int
getEpNumber = fmap read . readFile . (++) podcastDir

setEpNum :: String -> Int -> IO ()
setEpNum podcastName num = writeFile (podcastDir ++ podcastName) (show num)

numberedEps :: IO [PodcastsState]
numberedEps = sequence $ parMap rseq namedGet podcasts
  where
    namedGet (name, _) = do
      eps <- getMp3sForPodcast name
      epNum <- getEpNumber name
      pure $ PodcastsState eps name epNum
