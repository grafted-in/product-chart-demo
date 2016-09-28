{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Function
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Control.Monad
import           Data.Proxy
import qualified Data.Text          as T
import           GHCJS.DOM
import           GHCJS.DOM.Element
import           GHCJS.DOM.Document
import           GHCJS.DOM.Node
import           Miso
import           Miso.Html

addCss :: String -> IO ()
addCss urlLocal = do
  Just doc <- currentDocument
  Just header <- getHead doc
  Just linkEle <- createElement doc (Just ("link" :: String))
  setAttribute linkEle ("href" :: String) urlLocal
  setAttribute linkEle ("rel" :: String) ("stylesheet" :: String)
  void $ appendChild header (Just linkEle)

embedCss :: String -> IO ()
embedCss content = do
  Just doc <- currentDocument
  Just header <- getHead doc
  Just styleEle <- createElement doc (Just ("style" :: String))
  Just txt <- createTextNode doc content
  void $ appendChild styleEle (Just txt)
  void $ appendChild header (Just styleEle)

addAllCss = do
  addCss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
  addCss "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  addCss "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css"
  addCss "static/css/flat-ui.min.css"
  addCss "static/css/style.css"


clearBody :: IO ()
clearBody = do
  Just doc <- currentDocument
  Just body <- getBody doc
  first <- getFirstChild body
  go body first
    where
      go _ Nothing = pure ()
      go body (Just first) = do
        void $ removeChild body (Just first)
        clearBody

type ColumnName = T.Text


data Chart = Chart
  { chartData :: [Map T.Text T.Text]
  , chartOrder :: [T.Text]
  } deriving (Eq, Show)


data DragType = DragColumn | DragRow deriving (Eq, Show)

data DragInfo = DragInfo {
    dragType      :: DragType
  , sourceIndex   :: Int
  , destIndex     :: Int
  , prevDestIndex :: Maybe Int
  } deriving (Eq, Show)

data Model = Model {
    chart       :: Chart
  , deletedRows :: [Map T.Text T.Text]
  , dragInfo :: Maybe DragInfo
  } deriving (Eq, Show)

chart' :: Chart
chart' = Chart
  { chartData=
    [ Map.fromList
      [ ("Image", "https://images-na.ssl-images-amazon.com/images/I/417oTqn1QBL._AC_US320_QL65_.jpg")
      , ("Title", "SE KHK6320 Outdoor Tanto Knife with Fire Starter")
      , ("Rating", "4")
      , ("Price", "8.03")
      ]
    , Map.fromList
      [ ("Image", "https://images-na.ssl-images-amazon.com/images/I/313F4y4NgJL._AC_US320_QL65_.jpg")
      , ("Title", "StatGear 99416 Surviv-All Outdoor Knife with Firestarter, Sharpener & Cord Cutter")
      , ("Rating", "4.5")
      , ("Price", "38.98")
      ]
    ]
  , chartOrder=["Image", "Title", "Rating", "Price"]
  }

deletedRows' :: [Map T.Text T.Text]
deletedRows' =
  [ Map.fromList
    [ ("Image", "https://images-na.ssl-images-amazon.com/images/I/61FempoQTML._AC_US320_QL65_.jpg")
    , ("Title", "9\" Navy SEALs Tactical Combat Bowie Knife w/SHEATH Military Fixed Blade Survival")
    , ("Rating", "4.5")
    , ("Price", "8.23")
    ]
  , Map.fromList
    [ ("Image", "https://images-na.ssl-images-amazon.com/images/I/51UF+5YSXGL._AC_US320_QL65_.jpg")
    , ("Title", "Amazon Jungle Survival Knife with Sheath")
    , ("Rating", "4")
    , ("Price", "19.95")
    ]
  , Map.fromList
    [ ("Image", "https://images-na.ssl-images-amazon.com/images/I/31blMat+JDL._AC_US320_QL65_.jpg")
    , ("Title", "Jungle Master JM-001L Fixed Blade Hunting Knife, Straight Edge Blade, Rubberized Handle")
    , ("Rating", "4")
    , ("Price", "12.63")
    ]
  , Map.fromList
    [ ("Image", "https://images-na.ssl-images-amazon.com/images/I/21AV2hom0aL._AC_US320_QL65_.jpg")
    , ("Title", "Condor Tool and Knife Jungle Bowie, 11-Inch UltraBlaC2, Black Handle, Leather Sheath")
    , ("Rating", "4.5")
    , ("Price", "40.78")
    ]
  ]

data DragTransition = Start DragType Int
                    | Enter DragType Int
                    | End
                    deriving (Eq, Show)

data ChartAction
  = Drag DragTransition
  | RemoveRow Int
  | AddRow
  | NoOp

main :: IO ()
main = do
  addAllCss
  clearBody
  startApp model' view update defaultSettings
    where
      model' :: Model
      model' = Model chart' deletedRows' Nothing


update :: ChartAction -> Model -> Effect ChartAction Model
update action model = noEff $ case action of
  Drag transition -> updateDragging transition model
  RemoveRow rowIndex ->
    let Just (deleted, newChartData) = removeAt rowIndex (model & chart & chartData)
        newChart = (model & chart){chartData = newChartData}
    in model{chart = newChart, deletedRows = deleted : (model & deletedRows)}
  AddRow ->
    if null (model & deletedRows)
      then model
      else let
        newChartData = (model & chart & chartData) ++ [last $ model & deletedRows]
        newChart = (model & chart){chartData = newChartData}
        in model { chart = newChart, deletedRows = init (deletedRows model) }
  NoOp -> model


updateDragging :: DragTransition -> Model -> Model
updateDragging action model = case action of
  Start type_ index -> model{ dragInfo = Just $ DragInfo type_ index index Nothing }

  End -> whileDragging $ \(DragInfo type_ src dest _) ->
    model{ chart = chartModifier type_ src dest (model & chart), dragInfo = Nothing }

  Enter type_ index -> whileDragging $ \info@(DragInfo type_ src dest _) ->
    model{ dragInfo = Just info{ destIndex = index, prevDestIndex = Just $ destIndex info } }

  where
    whileDragging updater = case dragInfo model of
      Nothing   -> model
      Just info -> updater info

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt n xs = Just (last left, init left ++ right)
  where (left, right) = splitAt n xs


chartModifier :: DragType -> Int -> Int -> Chart -> Chart
chartModifier DragColumn from to chart = chart{chartOrder=shift from to (chart & chartOrder)}
chartModifier DragRow    from to chart = chart{chartData=shift from to (chart & chartData)}

view :: Model -> View ChartAction
view model = master (tableView model)

tableView :: Model -> [View ChartAction]
tableView (Model modelChart _ dragInfo) =
  [ div_ [classes ["row"]]
    [ div_ [classes ["col-xs-12"]]
      [ table_
        [ classes ["table", "chart-builder"]
        , onDragEnd $ Drag End
        ]
        [ toHead dragInfo (chart & chartOrder)
        , toBody dragInfo chart
        ]
      ]
    ]
  , div_ [class_ "row"]
    [ div_ [class_ "col-xs-12 text-center"]
      [ a_ [type_ "button", class_ "add-row", onClick AddRow]
          [ span_ [class_ "fui-plus-circle"] [] ]
      ]
    ]
  , div_ [class_ "row"]
    [ div_ [class_ "col-xs-12"]
      [ div_ [classes ["alert", "alert-info", "help-box", "animated", "pulse"]]
        [ p_ []
          [ span_ [class_ "fui-bubble"] []
          , text_ " Try the following:"
          ]
        , ul_ []
          [ li_ [] [text_ "Reorder columns (doesn't work in Firefox yet)"]
          , li_ [] [text_ "Reorder rows (doesn't work in Firefox yet)"]
          , li_ [] [text_ "Delete rows"]
          , li_ [] [text_ "Add rows"]
          ]
        ]
      ]
    ]
  ]

  where
    chart = case dragInfo of
      Nothing   -> modelChart
      Just info -> chartModifier (dragType info) (sourceIndex info) (destIndex info) modelChart


toHead :: Maybe DragInfo -> [T.Text] -> View ChartAction
toHead dragInfo columnNames =
  thead_ []
    [ tr_ []
      (chartHeadings ++ [th_ [] [], th_ [] []])
    ]
  where
    chartHeadings = for (zip [1..] columnNames) $ \(colIndex, columnName) ->
      let isColDragging = (dragType <$> dragInfo) == Just DragColumn
      in th_
        ([ onDragStart $ Drag $ Start DragColumn colIndex
         , prop "draggable" True
         , onDrop (PreventDefault True) $ Drag End
         ] ++
         [ onDragEnter $ Drag $ Enter DragColumn colIndex
         | isColDragging && join (prevDestIndex <$> dragInfo) /= Just colIndex
         ])
        [ text_ columnName ]

toBody :: Maybe DragInfo -> Chart -> View ChartAction
toBody dragInfo Chart{chartData=rows, chartOrder=columnNames} =
  tbody_ [] $
    for (zip [1..] rows) $ \(rowIndex, row) ->
      let isRowDragging = (dragType <$> dragInfo) == Just DragRow
      in tr_
        [ onDragEnter $ Drag $ Enter DragRow rowIndex
        | isRowDragging -- && join (prevDestIndex <$> dragInfo) /= Just rowIndex
        ] $
        ( for columnNames $ \columnName ->
            td_ [] [showColData columnName (Map.findWithDefault "" columnName row)]
        ) ++
        [ td_ []
          [ a_ [class_ "row-delete", onClick $ RemoveRow rowIndex, alt_ "Remove"]
              [ span_ [class_ "fui-cross-circle"] [] ]
          ]
        , td_ []
          [ a_
            [ prop "draggable" True
            , onDragStart $ Drag $ Start DragRow rowIndex
            , class_ "row-drag"
            ]
            [ fa "bars" ]
          ]
        ]

showColData :: ColumnName -> T.Text -> View a
showColData col content
  | col == "Image"  = span_ [] [img_ [class_ "img-thumbnail", src_ content, alt_ "Amazon Product Image" ]]
  | col == "Price"  = text_ ("$" <> content)
  | col == "Rating" = toStars (read $ T.unpack content)
  | otherwise       = text_ content


toStars :: Float -> View a
toStars amount =
  span_ []
    [ star (amount - 0)
    , star (amount - 1)
    , star (amount - 2)
    , star (amount - 3)
    , star (amount - 4)
    ]
  where
    star amount = span_ [classes ["star-icon", starClass]] [text_ starChar]
      where
        (starClass, starChar) =
          if amount < 0.5
            then ("", "☆")
            else ("full", "★")


for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap


shift :: Int -> Int -> [a] -> [a]
shift fromIndex toIndex xs = left ++ (item : right)
  where
    Just (item, xs') = removeAt fromIndex xs
    (left, right) = splitAt (toIndex - 1) xs'


fa :: T.Text -> View a
fa icon = span_ [class_ $ "fa fa-" <> icon] []


mkNodeTxt :: T.Text -> [Attribute a] -> [View a] -> View a
mkNodeTxt = mkNode HTML

nav_ = mkNodeTxt "nav"
i_ = mkNodeTxt "i"
button_ = mkNodeTxt "button"
img_ attrs = mkNodeTxt "img" attrs []

src_, role_, alt_ :: T.Text -> Attribute a
src_ url = prop "src" url
role_ = prop "role"
alt_ = prop "alt"

br_ attrs = mkNodeTxt "br" attrs []
h5_ = mkNodeTxt "h5"
h2_ = mkNodeTxt "h2"


classes :: [T.Text] -> Attribute a
classes = class_ . T.unwords

master :: [View a] -> View a
master content = div_ []
  [ topnav
  , div_ [class_ "container"] content
  ]


topnav :: View a
topnav =
  div_ [classes ["navbar", "navbar-inverse", "navbar-static-top"], role_ "navigation"]
    [ div_ [classes ["container"]]
      [ div_ [classes ["navbar-header"]]
        [ button_ [type_ "button", classes ["navbar-toggle", "collapsed"], attr "data-toggle" "collapse", attr "data-target" "#navbar", attr "aria-expanded" "false", attr "aria-controls" "navbar"]
          [ span_ [classes ["sr-only"]] [text_ "Toggle navigation"]
          , span_ [classes ["icon-bar"]] []
          , span_ [classes ["icon-bar"]] []
          , span_ [classes ["icon-bar"]] []
          ]
        , a_ [classes ["navbar-brand"], href_ "#"] [text_ "Product Charts"]
        ]
      , div_ [id_ "navbar", classes ["navbar-collapse", "collapse"]]
        [ ul_ [classes ["nav", "navbar-nav"]]
          [ li_ [] [a_ [href_ "#"] [text_ "Dashboard"]]
          , li_ [class_ "active"] [ a_ [href_ "#chart"] [text_ "Chart Editor"] ]
          , li_ [classes ["dropdown"]]
            [ a_ [href_ "#", classes ["dropdown-toggle"], attr "data-toggle" "dropdown", role_ "button", attr "aria-haspopup" "true",  attr "aria-expanded" "false"]
              [ text_ "Dropdown"
              , span_ [classes ["caret"]] []
              ]
            , ul_ [classes ["dropdown-menu"]]
              [ li_ [] [ a_ [href_ "#"] [text_ "Action"] ]
              , li_ [] [ a_ [href_ "#"] [text_ "Another action"] ]
              , li_ [] [ a_ [href_ "#"] [text_ "Something else here"] ]
              , li_ [role_ "separator", classes ["divider"]] []
              , li_ [classes ["dropdown-header"]] [text_ "Nav header"]
              , li_ [] [ a_ [href_ "#"] [text_ "Separated link"] ]
              , li_ [] [ a_ [href_ "#"] [text_ "One more separated link"] ]
              ]
            ]
          ]
        , ul_ [classes ["nav", "navbar-nav", "navbar-right"]]
          [ li_ []
            [ a_ [] [ span_ [class_ "fui-user"] [], text_ " Philip Wadler" ]
            ]
          ]
        ]
      ]
    ]
