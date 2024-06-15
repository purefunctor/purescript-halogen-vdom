module Halogen.VDom.Hydrate where

import Prelude

import Data.Foldable (surround)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception as EEx
import Partial.Unsafe (unsafePartial)
import Web.DOM.CharacterData as DOM.CharacterData
import Web.DOM.Node (Node) as DOM
import Web.DOM.Node as DOM.Node
import Web.DOM.NodeType as DOM.NodeType
import Web.DOM.Text (Text) as DOM
import Web.DOM.Text as DOM.Text

quoteText :: String -> String
quoteText t = surround "\"" [ t ]

checkIsTextNode :: DOM.Node -> Effect DOM.Text 
checkIsTextNode node =
  case DOM.Text.fromNode node of
    Just text -> pure text
    Nothing -> do
      EEx.throwException $ EEx.error $ "Expected node to be a " <> show DOM.NodeType.TextNode <> ", but got " <> show (unsafePartial (DOM.Node.nodeType node))

checkTextContentIsEqTo :: String -> DOM.Text -> Effect Unit
checkTextContentIsEqTo expectedText actualText = do
  textContent <- DOM.CharacterData.data_ (DOM.Text.toCharacterData actualText)
  when (textContent /= expectedText) do
    EEx.throwException $ EEx.error $ "Expected element text to equal to " <> quoteText expectedText <> ", but got " <> quoteText textContent
