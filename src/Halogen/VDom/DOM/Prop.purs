module Halogen.VDom.DOM.Prop where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (null, toNullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign (typeOf)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Halogen.VDom as V
import Halogen.VDom.Machine (Step'(..), Step, mkStep)
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (EventListener, eventListener) as DOM

-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.
data Prop a
  = Attribute (Maybe Namespace) String String
  | Property String PropValue
  | Handler DOM.EventType (DOM.Event → Maybe a)
  | Ref (ElemRef DOM.Element → Maybe a)

instance functorProp ∷ Functor Prop where
  map f (Handler ty g) = Handler ty (map f <$> g)
  map f (Ref g) = Ref (map f <$> g)
  map _ p = unsafeCoerce p

data ElemRef a
  = Created a
  | Removed a

instance functorElemRef ∷ Functor ElemRef where
  map f (Created a) = Created (f a)
  map f (Removed a) = Removed (f a)

foreign import data PropValue ∷ Type

propFromString ∷ String → PropValue
propFromString = unsafeCoerce

propFromBoolean ∷ Boolean → PropValue
propFromBoolean = unsafeCoerce

propFromInt ∷ Int → PropValue
propFromInt = unsafeCoerce

propFromNumber ∷ Number → PropValue
propFromNumber = unsafeCoerce

-- Implementation

type EmitterInputBuilder a = DOM.Event → Maybe a

type EventListenerAndCurrentEmitterInputBuilder a = Tuple DOM.EventListener (Ref.Ref (EmitterInputBuilder a))

type PropState a = 
  { events :: Object (EventListenerAndCurrentEmitterInputBuilder a)
  , props :: Object (Prop a)
  , emit :: a -> Effect Unit
  , el :: DOM.Element
  }

-- | A `Machine` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.
buildProp
  ∷ ∀ a
  . (a → Effect Unit)
  → DOM.Element
  → V.Machine (Array (Prop a)) Unit
buildProp emit el = renderProp
  where
  renderProp ∷ EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    events ← Util.newMutMap
    props ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (Fn.runFn3 applyProp el emit events)
    let
      state :: PropState a
      state =
        { events: Util.unsafeFreeze events
        , props
        , el
        , emit
        }
    pure $ mkStep $ Step unit state patchProp haltProp

patchProp ::
  ∀ a
  . EFn.EffectFn2
    (PropState a)
    (Array (Prop a))
    (Step (Array (Prop a)) Unit)
patchProp = EFn.mkEffectFn2 \state ps2 → do
  events ← Util.newMutMap
  let
    { events: prevEvents, props: ps1, emit, el } = state
    onThese = Fn.runFn3 diffProp el prevEvents events
    onThis = Fn.runFn2 removeProp el prevEvents
    onThat = Fn.runFn3 applyProp el emit events
  props ← EFn.runEffectFn6 Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
  let
    nextState :: PropState a
    nextState =
      { events: Util.unsafeFreeze events
      , props
      , el
      , emit
      }
  pure $ mkStep $ Step unit nextState patchProp haltProp

haltProp :: forall a. EFn.EffectFn1 (PropState a) Unit
haltProp = EFn.mkEffectFn1 \state -> do
  case Object.lookup "ref" state.props of
    Just (Ref f) ->
      EFn.runEffectFn2 mbEmit state.emit (f (Removed state.el))
    _ ->
      pure unit

mbEmit :: forall a. EFn.EffectFn2 (a -> Effect Unit) (Maybe a) Unit
mbEmit = EFn.mkEffectFn2 \emit -> case _ of
  Just a -> emit a
  _ -> pure unit

applyProp
  ∷ ∀ r a
  . Fn.Fn3
    DOM.Element
    (a → Effect Unit)
    (STObject r (EventListenerAndCurrentEmitterInputBuilder a))
    (EFn.EffectFn3 String Int (Prop a) (Prop a))
applyProp = Fn.mkFn3 \el emit events -> EFn.mkEffectFn3 \_ _ v →
  case v of
    Attribute ns attr val → do
      EFn.runEffectFn4 Util.setAttribute (toNullable ns) attr val el
      pure v
    Property prop val → do
      EFn.runEffectFn3 setProperty prop val el
      pure v
    Handler ty f → do
      EFn.runEffectFn5 setHandler el emit events ty f
      pure v
    Ref f → do
      EFn.runEffectFn2 mbEmit emit (f (Created el))
      pure v

setHandler
  ∷ ∀ r a
  . EFn.EffectFn5
    DOM.Element
    (a -> Effect Unit)
    (STObject r (EventListenerAndCurrentEmitterInputBuilder a))
    DOM.EventType
    (DOM.Event -> Maybe a)
    Unit
setHandler = EFn.mkEffectFn5 \el emit events (DOM.EventType ty) f →
  case Fn.runFn2 Util.unsafeGetAny ty events of
    handler | Fn.runFn2 Util.unsafeHasAny ty events → do
      Ref.write f (snd handler)
    _ → do
      ref ← Ref.new f
      listener ← DOM.eventListener \ev → do
        f' ← Ref.read ref
        EFn.runEffectFn2 mbEmit emit (f' ev)
      EFn.runEffectFn3 Util.pokeMutMap ty (Tuple listener ref) events
      EFn.runEffectFn3 Util.addEventListener ty listener el

diffProp
  ∷ ∀ r a
  . Fn.Fn3
    DOM.Element
    (Object.Object (EventListenerAndCurrentEmitterInputBuilder a))
    (STObject r (EventListenerAndCurrentEmitterInputBuilder a))
    (EFn.EffectFn4 String Int (Prop a) (Prop a) (Prop a))
diffProp = Fn.mkFn3 \el prevEvents events → EFn.mkEffectFn4 \_ _ v1 v2 →
  case v1, v2 of
    Attribute _ _ val1, Attribute ns2 attr2 val2 →
      if val1 == val2
        then pure v2
        else do
          EFn.runEffectFn4 Util.setAttribute (toNullable ns2) attr2 val2 el
          pure v2
    Property _ val1, Property prop2 val2 →
      case Fn.runFn2 Util.refEq val1 val2, prop2 of
        true, _ →
          pure v2
        _, "value" → do
          let elVal = Fn.runFn2 unsafeGetProperty "value" el
          if Fn.runFn2 Util.refEq elVal val2
            then pure v2
            else do
              EFn.runEffectFn3 setProperty prop2 val2 el
              pure v2
        _, _ → do
          EFn.runEffectFn3 setProperty prop2 val2 el
          pure v2
    Handler _ _, Handler (DOM.EventType ty) f → do
      let
        handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
      Ref.write f (snd handler)
      EFn.runEffectFn3 Util.pokeMutMap ty handler events
      pure v2
    _, _ →
      pure v2

removeProp
  ∷ ∀ a
  . Fn.Fn2
    DOM.Element
    (Object (EventListenerAndCurrentEmitterInputBuilder a))
    (EFn.EffectFn2 String (Prop a) Unit)
removeProp = Fn.mkFn2 \el prevEvents -> EFn.mkEffectFn2 \_ v →
  case v of
    Attribute ns attr _ →
      EFn.runEffectFn3 Util.removeAttribute (toNullable ns) attr el
    Property prop _ →
      EFn.runEffectFn2 removeProperty prop el
    Handler (DOM.EventType ty) _ → do
      let
        handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
      EFn.runEffectFn3 Util.removeEventListener ty (fst handler) el
    Ref _ →
      pure unit

-- Utilities

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"

setProperty ∷ EFn.EffectFn3 String PropValue DOM.Element Unit
setProperty = Util.unsafeSetAny

unsafeGetProperty ∷ Fn.Fn2 String DOM.Element PropValue
unsafeGetProperty = Util.unsafeGetAny

removeProperty ∷ EFn.EffectFn2 String DOM.Element Unit
removeProperty = EFn.mkEffectFn2 \key el →
  EFn.runEffectFn3 Util.hasAttribute null key el >>= if _
    then EFn.runEffectFn3 Util.removeAttribute null key el
    else case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of
      "string" → EFn.runEffectFn3 Util.unsafeSetAny key "" el
      _        → case key of
        "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        _ → EFn.runEffectFn3 Util.unsafeSetAny key Util.jsUndefined el
