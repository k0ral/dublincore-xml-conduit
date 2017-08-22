{-# LANGUAGE OverloadedStrings #-}
-- | XML streaming renderers for the __Dublin Core Metadata Element Set__.
--
-- This module is meant to be imported qualified, like this:
--
-- > import qualified Text.XML.DublinCore.Conduit.Render as DC
module Text.XML.DublinCore.Conduit.Render
  ( renderElementContributor
  , renderElementCoverage
  , renderElementCreator
  , renderElementDate
  , renderElementDescription
  , renderElementFormat
  , renderElementIdentifier
  , renderElementLanguage
  , renderElementPublisher
  , renderElementRelation
  , renderElementRights
  , renderElementSource
  , renderElementSubject
  , renderElementTitle
  , renderElementType
  ) where

-- {{{ Imports
import           Text.XML.DublinCore

import           Conduit
import           Data.Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types
import           Text.XML.Stream.Render
-- }}}


-- {{{ Utils
dcName :: Text -> Name
dcName string = Name string (Just "http://purl.org/dc/elements/1.1/") (Just namespacePrefix)
-- }}}

-- | Render a @\<dc:contributor\>@ element.
renderElementContributor :: Monad m => Text -> Source m Event
renderElementContributor = tag (dcName "contributor") mempty . content

-- | Render a @\<dc:coverage\>@ element.
renderElementCoverage :: Monad m => Text -> Source m Event
renderElementCoverage = tag (dcName "coverage") mempty . content

-- | Render a @\<dc:creator\>@ element.
renderElementCreator :: Monad m => Text -> Source m Event
renderElementCreator = tag (dcName "creator") mempty . content

-- | Render a @\<dc:date\>@ element.
renderElementDate :: Monad m => UTCTime -> Source m Event
renderElementDate = tag (dcName "date") mempty . content . formatTimeRFC3339 . utcToZonedTime utc

-- | Render a @\<dc:description\>@ element.
renderElementDescription :: Monad m => Text -> Source m Event
renderElementDescription = tag (dcName "description") mempty . content

-- | Render a @\<dc:format\>@ element.
renderElementFormat :: Monad m => Text -> Source m Event
renderElementFormat = tag (dcName "format") mempty . content

-- | Render a @\<dc:identifier\>@ element.
renderElementIdentifier :: Monad m => Text -> Source m Event
renderElementIdentifier = tag (dcName "identifier") mempty . content

-- | Render a @\<dc:language\>@ element.
renderElementLanguage :: Monad m => Text -> Source m Event
renderElementLanguage = tag (dcName "language") mempty . content

-- | Render a @\<dc:publisher\>@ element.
renderElementPublisher :: Monad m => Text -> Source m Event
renderElementPublisher = tag (dcName "publisher") mempty . content

-- | Render a @\<dc:relation\>@ element.
renderElementRelation :: Monad m => Text -> Source m Event
renderElementRelation = tag (dcName "relation") mempty . content

-- | Render a @\<dc:rights\>@ element.
renderElementRights :: Monad m => Text -> Source m Event
renderElementRights = tag (dcName "rights") mempty . content

-- | Render a @\<dc:source\>@ element.
renderElementSource :: Monad m => Text -> Source m Event
renderElementSource = tag (dcName "source") mempty . content

-- | Render a @\<dc:subject\>@ element.
renderElementSubject :: Monad m => Text -> Source m Event
renderElementSubject = tag (dcName "subject") mempty . content

-- | Render a @\<dc:title\>@ element.
renderElementTitle :: Monad m => Text -> Source m Event
renderElementTitle = tag (dcName "title") mempty . content

-- | Render a @\<dc:type\>@ element.
renderElementType :: Monad m => Text -> Source m Event
renderElementType = tag (dcName "type") mempty . content
