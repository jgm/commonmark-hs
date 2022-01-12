{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.RebaseRelativePaths
  ( rebaseRelativePathsSpec )
where
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Inlines
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Parsec (getPosition)
import Text.Parsec.Pos (sourceName)
import System.FilePath
import qualified System.FilePath.Windows as Windows
import qualified System.FilePath.Posix as Posix
import Network.URI (URI (uriScheme), parseURI)
import qualified Data.Set as Set

rebaseRelativePathsSpec
  :: forall m bl il . (Monad m , IsInline il , IsBlock il bl)
  => SyntaxSpec m il bl
rebaseRelativePathsSpec =
  defaultSyntaxSpec {
     syntaxBracketedSpecs = [rebasedImageSpec, rebasedLinkSpec] }

 where

  rebasedImageSpec :: BracketedSpec il
  rebasedImageSpec =BracketedSpec
            { bracketedName = "Image"
            , bracketedNests = True
            , bracketedPrefix = Just '!'
            , bracketedSuffixEnd = Just ')'
            , bracketedSuffix = newImageSuffix
            }

  rebasedLinkSpec :: BracketedSpec il
  rebasedLinkSpec = BracketedSpec
           { bracketedName = "Link"
           , bracketedNests = False  -- links don't nest inside links
           , bracketedPrefix = Nothing
           , bracketedSuffixEnd = Just ')'
           , bracketedSuffix = newLinkSuffix
           }

  newImageSuffix rm key = do
    pos <- getPosition
    LinkInfo target title attrs mbpos <- pLink rm key
    let pos' = fromMaybe pos mbpos
    return $! addAttributes attrs . image (rebasePath pos' target) title

  newLinkSuffix rm key = do
    pos <- getPosition
    LinkInfo target title attrs mbpos <- pLink rm key
    let pos' = fromMaybe pos mbpos
    return $! addAttributes attrs . link (rebasePath pos' target) title

-- | Rebase a relative path, by adding the (relative) directory
-- of the containing source position.  Absolute links and URLs
-- are untouched.
rebasePath :: SourcePos -> Text -> Text
rebasePath pos path = do
  let fp = sourceName pos
      isFragment = T.take 1 path == "#"
      path' = T.unpack path
      isAbsolutePath = Posix.isAbsolute path' || Windows.isAbsolute path'
   in if T.null path || isFragment || isAbsolutePath || isURI path
         then path
         else
           case takeDirectory fp of
             ""  -> path
             "." -> path
             d   -> T.pack d <> "/" <> path

-- | Schemes from http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes doi, javascript, isbn, pmid.
schemes :: Set.Set T.Text
schemes = Set.fromList
  -- Official IANA schemes
  [ "aaa", "aaas", "about", "acap", "acct", "acr", "adiumxtra", "afp", "afs"
  , "aim", "appdata", "apt", "attachment", "aw", "barion", "beshare", "bitcoin"
  , "blob", "bolo", "browserext", "callto", "cap", "chrome", "chrome-extension"
  , "cid", "coap", "coaps", "com-eventbrite-attendee", "content", "crid", "cvs"
  , "data", "dav", "dict", "dis", "dlna-playcontainer", "dlna-playsingle"
  , "dns", "dntp", "dtn", "dvb", "ed2k", "example", "facetime", "fax", "feed"
  , "feedready", "file", "filesystem", "finger", "fish", "ftp", "geo", "gg"
  , "git", "gizmoproject", "go", "gopher", "graph", "gtalk", "h323", "ham"
  , "hcp", "http", "https", "hxxp", "hxxps", "hydrazone", "iax", "icap", "icon"
  , "im", "imap", "info", "iotdisco", "ipn", "ipp", "ipps", "irc", "irc6"
  , "ircs", "iris", "iris.beep", "iris.lwz", "iris.xpc", "iris.xpcs"
  , "isostore", "itms", "jabber", "jar", "jms", "keyparc", "lastfm", "ldap"
  , "ldaps", "lvlt", "magnet", "mailserver", "mailto", "maps", "market"
  , "message", "mid", "mms", "modem", "mongodb", "moz", "ms-access"
  , "ms-browser-extension", "ms-drive-to", "ms-enrollment", "ms-excel"
  , "ms-gamebarservices", "ms-getoffice", "ms-help", "ms-infopath"
  , "ms-media-stream-id", "ms-officeapp", "ms-project", "ms-powerpoint"
  , "ms-publisher", "ms-search-repair", "ms-secondary-screen-controller"
  , "ms-secondary-screen-setup", "ms-settings", "ms-settings-airplanemode"
  , "ms-settings-bluetooth", "ms-settings-camera", "ms-settings-cellular"
  , "ms-settings-cloudstorage", "ms-settings-connectabledevices"
  , "ms-settings-displays-topology", "ms-settings-emailandaccounts"
  , "ms-settings-language", "ms-settings-location", "ms-settings-lock"
  , "ms-settings-nfctransactions", "ms-settings-notifications"
  , "ms-settings-power", "ms-settings-privacy", "ms-settings-proximity"
  , "ms-settings-screenrotation", "ms-settings-wifi", "ms-settings-workplace"
  , "ms-spd", "ms-sttoverlay", "ms-transit-to", "ms-virtualtouchpad"
  , "ms-visio", "ms-walk-to", "ms-whiteboard", "ms-whiteboard-cmd", "ms-word"
  , "msnim", "msrp", "msrps", "mtqp", "mumble", "mupdate", "mvn", "news", "nfs"
  , "ni", "nih", "nntp", "notes", "ocf", "oid", "onenote", "onenote-cmd"
  , "opaquelocktoken", "pack", "palm", "paparazzi", "pkcs11", "platform", "pop"
  , "pres", "prospero", "proxy", "pwid", "psyc", "qb", "query", "redis"
  , "rediss", "reload", "res", "resource", "rmi", "rsync", "rtmfp", "rtmp"
  , "rtsp", "rtsps", "rtspu", "secondlife", "service", "session", "sftp", "sgn"
  , "shttp", "sieve", "sip", "sips", "skype", "smb", "sms", "smtp", "snews"
  , "snmp", "soap.beep", "soap.beeps", "soldat", "spotify", "ssh", "steam"
  , "stun", "stuns", "submit", "svn", "tag", "teamspeak", "tel", "teliaeid"
  , "telnet", "tftp", "things", "thismessage", "tip", "tn3270", "tool", "turn"
  , "turns", "tv", "udp", "unreal", "urn", "ut2004", "v-event", "vemmi"
  , "ventrilo", "videotex", "vnc", "view-source", "wais", "webcal", "wpid"
  , "ws", "wss", "wtai", "wyciwyg", "xcon", "xcon-userid", "xfire"
  , "xmlrpc.beep", "xmlrpc.beeps", "xmpp", "xri", "ymsgr", "z39.50", "z39.50r"
  , "z39.50s"
  -- Unofficial schemes
  , "doi", "isbn", "javascript", "pmid"
  ]

-- | Check if the string is a valid URL with a IANA or frequently used but
-- unofficial scheme (see @schemes@).
isURI :: T.Text -> Bool
isURI = maybe False hasKnownScheme . parseURI . T.unpack
  where
    hasKnownScheme = (`Set.member` schemes) . T.toLower .
                     T.filter (/= ':') . T.pack . uriScheme
