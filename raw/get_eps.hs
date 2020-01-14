{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isAscii, isDigit, isPrint)
import           Data.List             (intercalate)
import           Data.Maybe            (mapMaybe)
import qualified Data.Time.Calendar    as Cal
import qualified Data.Time.Clock       as Clock
import qualified Data.Time.Format      as DF
import           System.Environment    (getEnv, lookupEnv)

main = do feed <- getEnv    "FEED"
          keep <- lookupEnv "KEEP_ALL"
          now  <- Clock.getCurrentTime
          BS.interact $ convert (sanitise feed)
                                (case keep of
                                  Nothing -> Just (Clock.utctDay now)
                                  Just _  -> Nothing)
  where sanitise = escape . BS.pack

-- | XML escape a ByteString
escape = replace '<' "&lt;" .
         replace '<' "&lt;" .
         replace '&' "&amp;"  -- Do this first, to avoid double-escaping
  where replace c rep s = BS.intercalate rep (BS.split c s)

convert feed today stdin = mkFeed feed items
  where items  = mapMaybe (lineToItem feed today) (getLines stdin)

-- | Takes HTML filled with CSV and returns only the CSV data rows
getLines :: BS.ByteString -> [BS.ByteString]
getLines = map (BS.map onlyAscii) . filter hasLeadingDigit . BS.lines
  where hasLeadingDigit l = not (BS.null l) && isDigit (BS.head l)

        onlyAscii c = if isAscii c && isPrint c then c else '_'

{- | Turn the fields in a given line into an item for the feed. Return Nothing
     if the item's date is in the future or before a year ago, based on 'today'.

     Format of input line:
       NUM,SERIES,EPISODE,DATE,"NAME","URL"

     Format of output item:
       <item>
         <title>NUM (sSERIESeEPISODE) "NAME"</title>
         <link>URL</link>
         <pubDate>2019-11-24</pubDate>
         <description>Episode NUM, sSERIESeEPISODE - "NAME"</description>
         <guid>FEED-NUM</guid>
       </item>
-}
lineToItem feed today line = if inRange today day then Just item else Nothing
  where -- Extract fields from the given line

        -- pre  = NUM,SERIES,EPISODE,DATE,
        -- post = "NAME","URL"
        (pre , post ) = BS.span noQ line

        [num, series, episode, rawDate, _] = BS.split ',' pre

        -- post' = ","url"
        (name, post') = BS.span noQ (BS.tail post)

        url           = BS.takeWhile noQ
                          (BS.tail (BS.dropWhile noQ (BS.tail post')))

        day           = fixDate rawDate

        noQ           = (/= '"')

        -- Construct the output fields

        title = BS.concat [num, " (", ep, ") ", name]
        desc  = BS.concat ["Episode ", num, ", ", ep, " - ", name]
        ep    = BS.concat ["s", series, "e", episode]
        guid  = BS.concat [feed , "-", num]
        date  = BS.pack (DF.formatTime DF.defaultTimeLocale "%Y-%m-%d" day)

        -- Splice the output fields into the XML template
        item = BS.unlines [
            "<item>"
          ,   "<title>"       +++ title +++ "</title>"
          ,   "<pubDate>"     +++ date  +++ "</pubDate>"
          ,   "<description>" +++ desc  +++ "</description>"
          ,   "<guid>"        +++ guid  +++ "</guid>"
          , "</item>"
          ]

mkFeed feed items = BS.unlines ([
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
  , "<rss xmlns:atom=\"http://www.w3.org/2005/Atom\" version=\"2.0\">"
  ,   "<channel>"
  ,     "<title>"       +++ feed +++ "</title>"
  ,     "<description>" +++ feed +++ "</description>"
  ,   "</channel>"
  ] ++ items ++ ["</rss>"])

-- | Turn dates from a naive '20 Nov 10' format into a more sensible 2010-11-20
fixDate :: BS.ByteString -> Cal.Day
fixDate d = case DF.parseTimeM False  -- Don't strip leading/trailing whitespace
                               DF.defaultTimeLocale  -- US locale; it'll do
                               "%d %b %Y"  -- Format to parse
                               (BS.unpack (BS.unwords [day, month, year'])) of
                   Nothing -> error ("Failed to decode date: " ++ BS.unpack d)
                   Just d' -> d'

  where [day, month, year] = BS.split ' ' d

        {- Stick '19' before the year if it's before the Unix epoch (epguides
           has a Y2K problem...). Without this, GNU date will assume dates like
           '64' should be '2064' (and, on 32bit, will get an overflow error!) -}
        year' = let y :: Int
                    y = read (BS.unpack year)

                    century = if y < 70 && y > 30 then "19" else "20"
                 in century +++ year

-- | Whether a given date is within a year of the given 'today' date, and not in
--   the future. If 'today' is Nothing we always return True (useful for tests).
inRange today = case today of
  Nothing     -> const True
  Just today' -> let lastYear = Cal.addDays (-365) today'
                  in \day -> Cal.diffDays lastYear day <= 0 &&
                             Cal.diffDays today'   day >= 0

-- | Shorthand
(+++) = BS.append
