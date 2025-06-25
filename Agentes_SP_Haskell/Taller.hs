import Graphics.UI.Gtk
import Control.Monad.IO.Class      -- for liftIO
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo as RendCairo
import Control.Monad                --  for  ForM_
import Data.Text (pack)

import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Gdk.Pixbuf

import Graphics.UI.Gtk.General.CssProvider (cssProviderLoadFromPath, cssProviderNew)
import Graphics.UI.Gtk.Gdk.Screen (screenGetDefault, screenGetRootWindow)
import Graphics.UI.Gtk.Abstract.Widget (widgetGetStyleContext)
import Graphics.UI.Gtk.Gdk.Events (eventButton)
import Graphics.UI.Gtk.Gdk.GLContext
import Graphics.UI.Gtk.Types

import GI.Gtk.Objects.StyleContext 
import GI.Gtk.Constants
import Data.Word (Word32)

import Data.IORef
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)


import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Graphic
import Histogram
import AgentWindow
import AStar
import Agent


type Position = (Double, Double)
type Obstacle = (Double, Double, Double, Double, String) -- (x, y, width, height)


drawBattery :: Adjustment -> DrawingArea -> Render ()
drawBattery _ drawingArea = do
  setSourceRGB 0 1 0 
  RendCairo.rectangle 0 0 40 60
  fill

  setSourceRGB 0 0 0 
  RendCairo.rectangle 0 0 40 60
  stroke

drawPerformanceBar :: Adjustment -> DrawingArea -> Render ()
drawPerformanceBar _ drawingArea = do
  setSourceRGB 0 1 0 
  RendCairo.rectangle 0 0 80 10
  fill

  setSourceRGB 0 0 0 
  RendCairo.rectangle 0 0 80 10
  stroke

drawEnergyChart :: Adjustment -> DrawingArea -> Render ()
drawEnergyChart _ drawingArea = do
  setSourceRGB 0 0 0 
  RendCairo.rectangle 0 0 80 20
  stroke


main :: IO ()
main = do
  initGUI
  window <- windowNew
  adjustment <- adjustmentNew 50.0 0.0 101.0 1.0 1.0 1.0
  paintArea <- drawingAreaNew
  
  mainBox <- hBoxNew False 0

  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyAlways PolicyAlways
  --scrolledWindowAddWithViewport scrolledWindow mainBox
  
  set window [windowDefaultWidth := 1320,
              windowDefaultHeight := 520,
              windowWindowPosition := WinPosCenter,
              containerChild := mainBox,
              windowTitle := "Taller"]
  windowSetPosition window WinPosCenter  
  on window objectDestroy mainQuit




  -- ######### GRAFICO DE AGENTES #########
  agentBox <- vBoxNew False 0  

  pos1 <- newIORef (210, 80)
  pos2 <- newIORef (240, 80)
  pos3 <- newIORef (240, 80)
  pos4 <- newIORef (240, 80)

  let agents = [ 
              Agente 1 True 10 pos1
            , Agente 2 False 10 pos2
            , Agente 3 False 5 pos2
            , Agente 4 False 10 pos2
            ]

      obstacles = [(230, 20, 50, 30, "M1"), (150, 20, 50, 30, "M2"), (150, 100, 50, 30, "M3")] -- x y w h Definición de obstáculos


  widgetModifyBg paintArea StateNormal (Color 65535 65535 65535)
  on paintArea draw $ drawAgentAreaHandler adjustment paintArea agents obstacles  
  onValueChanged adjustment (valueChangedHandler paintArea)

  widgetSetSizeRequest paintArea 425 300
  paintAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd paintAreaAlignment paintArea
  boxPackStart agentBox paintAreaAlignment PackGrow 0




  -- ######### APARTADO DE DATOS #########
  dataBox <- vBoxNew False 0
  
  beginBox <- hBoxNew False 0
  columnBeginBox1 <- vBoxNew False 0
  columnBeginBox2 <- vBoxNew False 0
  columnBeginBox3 <- vBoxNew False 0
  columnBeginBox4 <- vBoxNew False 0


  startButton <- buttonNewWithLabel "Start"
  
  let targetPos = (230, 150) -- Posición objetivo

  on startButton buttonActivated $ do
    forM_ agents $ \agent -> do
      forkIO $ do
        moverAgente adjustment paintArea agent targetPos obstacles
        

  stopButton <- buttonNewWithLabel "Stop"
  
  timeLabel <- labelNew (Just "TIME")
  timeBar <- statusbarNew
  statusbarPush timeBar 1 "0"

  timeLimitLabel <- labelNew (Just "TIME LIMIT")
  timeLimitEntry <- spinButtonNewWithRange 0 1000 10

  p2sLabel <- labelNew (Just "P2S")
  p2sBar <- statusbarNew
  statusbarPush p2sBar 1 "0"

  goalLabel <- labelNew (Just "GOAL")
  goalEntry <- spinButtonNewWithRange 0 1000 10

  startButtonAlignment <- alignmentNew 0 0.5 0 0
  containerAdd startButtonAlignment startButton
  boxPackStart beginBox startButtonAlignment PackGrow 0

  stopButtonAlignment <- alignmentNew 0 0.5 0 0
  containerAdd stopButtonAlignment stopButton
  boxPackStart beginBox stopButtonAlignment PackGrow 0

  timeLabelAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd timeLabelAlignment timeLabel
  boxPackStart columnBeginBox1 timeLabelAlignment PackGrow 0
  
  timeBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd timeBarAlignment timeBar
  boxPackStart columnBeginBox1 timeBarAlignment PackGrow 0

  timeLimitLabelAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd timeLimitLabelAlignment timeLimitLabel
  boxPackStart columnBeginBox2 timeLimitLabelAlignment PackGrow 0

  timeLimitEntryAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd timeLimitEntryAlignment timeLimitEntry
  boxPackStart columnBeginBox2 timeLimitEntryAlignment PackGrow 0

  p2sLabelAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd p2sLabelAlignment p2sLabel
  boxPackStart columnBeginBox3 p2sLabelAlignment PackGrow 0
  
  p2sBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd p2sBarAlignment p2sBar
  boxPackStart columnBeginBox3 p2sBarAlignment PackGrow 0
  
  goalLabelAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd goalLabelAlignment goalLabel
  boxPackStart columnBeginBox4 goalLabelAlignment PackGrow 0
  
  goalEntryAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd goalEntryAlignment goalEntry
  boxPackStart columnBeginBox4 goalEntryAlignment PackGrow 0
  

  columnBeginBox1Alignment <- alignmentNew 0.5 0 1 1
  containerAdd columnBeginBox1Alignment columnBeginBox1
  boxPackStart beginBox columnBeginBox1Alignment PackGrow 0

  columnBeginBox2Alignment <- alignmentNew 0.5 0 1 1
  containerAdd columnBeginBox2Alignment columnBeginBox2
  boxPackStart beginBox columnBeginBox2Alignment PackGrow 0

  columnBeginBox3Alignment <- alignmentNew 0.5 0 0 0
  containerAdd columnBeginBox3Alignment columnBeginBox3
  boxPackStart beginBox columnBeginBox3Alignment PackGrow 0

  columnBeginBox4Alignment <- alignmentNew 0.5 0 0 0
  containerAdd columnBeginBox4Alignment columnBeginBox4
  boxPackStart beginBox columnBeginBox4Alignment PackGrow 0


  beginBoxAlignment <- alignmentNew 0 0 0.5 0.5
  containerAdd beginBoxAlignment beginBox
  boxPackStart dataBox beginBoxAlignment PackNatural 0
  

  --CONFIGBOX
  configBox <- hBoxNew False 0
  
  levelBox <- vBoxNew False 0

  
    --Labels Columns
  labelColumnsBox <- hBoxNew False 0


  profileLabelBox <- vBoxNew False 0
  profileLabel1 <- labelNew (Just "PROFILE INTEREST")
  profileLabel2 <- labelNew (Just "LEVELS")

  profileLabel1Alignment <- alignmentNew 1 0 0 0
  containerAdd profileLabel1Alignment profileLabel1
  boxPackStart profileLabelBox profileLabel1Alignment PackGrow 0
  
  profileLabel2Alignment <- alignmentNew 0.5 0 0 0
  containerAdd profileLabel2Alignment profileLabel2
  boxPackStart profileLabelBox profileLabel2Alignment PackGrow 0

  motivationLabel <- labelNew (Just "MOTIVATION")

  
  profileLabelBoxAlignment <- alignmentNew 1 0.5 0 0
  containerAdd profileLabelBoxAlignment profileLabelBox
  boxPackStart labelColumnsBox profileLabelBoxAlignment PackGrow 0
  
  motivationLabelAlignment <- alignmentNew 0.6 0.5 0 0
  containerAdd motivationLabelAlignment motivationLabel
  boxPackStart labelColumnsBox motivationLabelAlignment PackGrow 0

  labelColumnsBoxAlignment <- alignmentNew 1 0 0.5 0.5
  containerAdd labelColumnsBoxAlignment labelColumnsBox
  boxPackStart levelBox labelColumnsBoxAlignment PackGrow 0


    --PARENT SECTION
  parentBox <- hBoxNew False 0


  parentLabelBox <- vBoxNew False 0

  parentLabel1 <- labelNew (Just "P")
  parentLabel2 <- labelNew (Just "A")
  parentLabel3 <- labelNew (Just "R")
  parentLabel4 <- labelNew (Just "E")
  parentLabel5 <- labelNew (Just "N")
  parentLabel6 <- labelNew (Just "T")

  boxPackStart parentLabelBox parentLabel1 PackNatural 0
  boxPackStart parentLabelBox parentLabel2 PackNatural 0
  boxPackStart parentLabelBox parentLabel3 PackNatural 0
  boxPackStart parentLabelBox parentLabel4 PackNatural 0
  boxPackStart parentLabelBox parentLabel5 PackNatural 0
  boxPackStart parentLabelBox parentLabel6 PackNatural 0


  actionLabelBox1 <- vBoxNew False 0

  eatLabel1 <- labelNew (Just "EAT")
  workLabel1 <- labelNew (Just "WORK")
  restLabel1 <- labelNew (Just "REST")

  eatLabel1Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd eatLabel1Alignment eatLabel1
  boxPackStart actionLabelBox1 eatLabel1Alignment PackGrow 0 

  workLabel1Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd workLabel1Alignment workLabel1
  boxPackStart actionLabelBox1 workLabel1Alignment PackGrow 0

  restLabel1Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd restLabel1Alignment restLabel1
  boxPackStart actionLabelBox1 restLabel1Alignment PackGrow 0


  comboBoxes1 <- vBoxNew False 0

  parentEatComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText parentEatComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive parentEatComboBox 2

  parentWorkComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText parentWorkComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive parentWorkComboBox 2

  parentRestComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText parentRestComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive parentRestComboBox 2  

  parentEatComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd parentEatComboBoxAlignment parentEatComboBox
  boxPackStart comboBoxes1 parentEatComboBoxAlignment PackGrow 0
  
  parentWorkComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd parentWorkComboBoxAlignment parentWorkComboBox
  boxPackStart comboBoxes1 parentWorkComboBoxAlignment PackGrow 0

  parentRestComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd parentRestComboBoxAlignment parentRestComboBox
  boxPackStart comboBoxes1 parentRestComboBoxAlignment PackGrow 0


  parentBarBox <- vBoxNew False 0

  parentEatBar <- statusbarNew
  statusbarPush parentEatBar 1 "0"

  parentWorkBar <- statusbarNew
  statusbarPush parentWorkBar 1 "0"

  parentRestBar <- statusbarNew
  statusbarPush parentRestBar 1 "0"

  parentEatBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd parentEatBarAlignment parentEatBar
  boxPackStart parentBarBox parentEatBarAlignment PackGrow 0

  parentWorkBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd parentWorkBarAlignment parentWorkBar
  boxPackStart parentBarBox parentWorkBarAlignment PackGrow 0

  parentRestBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd parentRestBarAlignment parentRestBar
  boxPackStart parentBarBox parentRestBarAlignment PackGrow 0



  parentLabelBoxAlignment <- alignmentNew 0 0.5 0 0
  containerAdd parentLabelBoxAlignment parentLabelBox
  boxPackStart parentBox parentLabelBoxAlignment PackGrow 0

  actionLabelBox1Alignment <- alignmentNew 0 0.5 0 1
  containerAdd actionLabelBox1Alignment actionLabelBox1
  boxPackStart parentBox actionLabelBox1Alignment PackGrow 0

  comboBoxes1Alignment <- alignmentNew 0 0.5 0 1
  containerAdd comboBoxes1Alignment comboBoxes1
  boxPackStart parentBox comboBoxes1Alignment PackGrow 0

  parentBarBoxAlignment <- alignmentNew 1 0.5 1 1
  containerAdd parentBarBoxAlignment parentBarBox
  boxPackStart parentBox parentBarBoxAlignment PackGrow 0


  parentBoxAlignment <- alignmentNew 0 0 0.8 0.3
  containerAdd parentBoxAlignment parentBox
  boxPackStart levelBox parentBoxAlignment PackGrow 0



    --ADULT SECTION
  adultBox <- hBoxNew False 0


  adultLabelBox <- vBoxNew False 0

  adultLabel1 <- labelNew (Just "A")
  adultLabel2 <- labelNew (Just "D")
  adultLabel3 <- labelNew (Just "U")
  adultLabel4 <- labelNew (Just "L")
  adultLabel5 <- labelNew (Just "T")

  boxPackStart adultLabelBox adultLabel1 PackNatural 0
  boxPackStart adultLabelBox adultLabel2 PackNatural 0
  boxPackStart adultLabelBox adultLabel3 PackNatural 0
  boxPackStart adultLabelBox adultLabel4 PackNatural 0
  boxPackStart adultLabelBox adultLabel5 PackNatural 0


  actionLabelBox2 <- vBoxNew False 0

  eatLabel2 <- labelNew (Just "EAT")
  workLabel2 <- labelNew (Just "WORK")
  restLabel2 <- labelNew (Just "REST")

  eatLabel2Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd eatLabel2Alignment eatLabel2
  boxPackStart actionLabelBox2 eatLabel2Alignment PackGrow 0

  workLabel2Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd workLabel2Alignment workLabel2
  boxPackStart actionLabelBox2 workLabel2Alignment PackGrow 0
  
  restLabel2Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd restLabel2Alignment restLabel2
  boxPackStart actionLabelBox2 restLabel2Alignment PackGrow 0


  comboBoxes2 <- vBoxNew False 0

  adultEatComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText adultEatComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive adultEatComboBox 2

  adultWorkComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText adultWorkComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive adultWorkComboBox 2

  adultRestComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText adultRestComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive adultRestComboBox 2  

  adultEatComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd adultEatComboBoxAlignment adultEatComboBox
  boxPackStart comboBoxes2 adultEatComboBoxAlignment PackGrow 0

  adultWorkComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd adultWorkComboBoxAlignment adultWorkComboBox
  boxPackStart comboBoxes2 adultWorkComboBoxAlignment PackGrow 0

  adultRestComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd adultRestComboBoxAlignment adultRestComboBox
  boxPackStart comboBoxes2 adultRestComboBoxAlignment PackGrow 0


  adultBarBox <- vBoxNew False 0

  adultEatBar <- statusbarNew
  statusbarPush adultEatBar 1 "0"

  adultWorkBar <- statusbarNew
  statusbarPush adultWorkBar 1 "0"

  adultRestBar <- statusbarNew
  statusbarPush adultRestBar 1 "0"

  adultEatBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd adultEatBarAlignment adultEatBar
  boxPackStart adultBarBox adultEatBarAlignment PackGrow 0

  adultWorkBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd adultWorkBarAlignment adultWorkBar
  boxPackStart adultBarBox adultWorkBarAlignment PackGrow 0

  adultRestBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd adultRestBarAlignment adultRestBar
  boxPackStart adultBarBox adultRestBarAlignment PackGrow 0



  adultLabelBoxAlignment <- alignmentNew 0 0.5 0 0
  containerAdd adultLabelBoxAlignment adultLabelBox
  boxPackStart adultBox adultLabelBoxAlignment PackGrow 0

  actionLabelBox2Alignment <- alignmentNew 0 0.5 0 1
  containerAdd actionLabelBox2Alignment actionLabelBox2
  boxPackStart adultBox actionLabelBox2Alignment PackGrow 0

  comboBoxes2Alignment <- alignmentNew 0 0.5 0 1
  containerAdd comboBoxes2Alignment comboBoxes2
  boxPackStart adultBox comboBoxes2Alignment PackGrow 0

  adultBarBoxAlignment <- alignmentNew 1 0.5 1 1
  containerAdd adultBarBoxAlignment adultBarBox  
  boxPackStart adultBox adultBarBoxAlignment PackGrow 0

  
  adultBoxAlignment <- alignmentNew 0 0 0.8 0.3
  containerAdd adultBoxAlignment adultBox
  boxPackStart levelBox adultBoxAlignment PackGrow 0


    --CHILD SECTION
  childBox <- hBoxNew False 0


  childLabelBox <- vBoxNew False 0

  childLabel1 <- labelNew (Just "C")
  childLabel2 <- labelNew (Just "H")
  childLabel3 <- labelNew (Just "I")
  childLabel4 <- labelNew (Just "L")
  childLabel5 <- labelNew (Just "D")

  boxPackStart childLabelBox childLabel1 PackNatural 0
  boxPackStart childLabelBox childLabel2 PackNatural 0
  boxPackStart childLabelBox childLabel3 PackNatural 0
  boxPackStart childLabelBox childLabel4 PackNatural 0
  boxPackStart childLabelBox childLabel5 PackNatural 0


  actionLabelBox3 <- vBoxNew False 0

  eatLabel3 <- labelNew (Just "EAT")
  workLabel3 <- labelNew (Just "WORK")
  restLabel3 <- labelNew (Just "REST")

  eatLabel3Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd eatLabel3Alignment eatLabel3
  boxPackStart actionLabelBox3 eatLabel3Alignment PackGrow 0
  
  workLabel3Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd workLabel3Alignment workLabel3
  boxPackStart actionLabelBox3 workLabel3Alignment PackGrow 0

  restLabel3Alignment <- alignmentNew 0 0.5 0 0.5
  containerAdd restLabel3Alignment restLabel3
  boxPackStart actionLabelBox3 restLabel3Alignment PackGrow 0


  comboBoxes3 <- vBoxNew False 0

  childEatComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText childEatComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive childEatComboBox 2

  childWorkComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText childWorkComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive childWorkComboBox 2

  childRestComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText childRestComboBox . pack) ["very low", "low", "medium", "high", "very high"]
  comboBoxSetActive childRestComboBox 2   

  childEatComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd childEatComboBoxAlignment childEatComboBox
  boxPackStart comboBoxes3 childEatComboBoxAlignment PackGrow 0

  childWorkComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd childWorkComboBoxAlignment childWorkComboBox
  boxPackStart comboBoxes3 childWorkComboBoxAlignment PackGrow 0

  childRestComboBoxAlignment <- alignmentNew 0 0.5 1 0
  containerAdd childRestComboBoxAlignment childRestComboBox
  boxPackStart comboBoxes3 childRestComboBoxAlignment PackGrow 0


  childBarBox <- vBoxNew False 0

  childEatBar <- statusbarNew
  statusbarPush childEatBar 1 "0"

  childWorkBar <- statusbarNew
  statusbarPush childWorkBar 1 "0"

  childRestBar <- statusbarNew
  statusbarPush childRestBar 1 "0"

  childEatBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd childEatBarAlignment childEatBar
  boxPackStart childBarBox childEatBarAlignment PackGrow 0

  childWorkBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd childWorkBarAlignment childWorkBar
  boxPackStart childBarBox childWorkBarAlignment PackGrow 0

  childRestBarAlignment <- alignmentNew 0.5 0.5 0 0
  containerAdd childRestBarAlignment childRestBar
  boxPackStart childBarBox childRestBarAlignment PackGrow 0



  childLabelBoxAlignment <- alignmentNew 0 0.5 0 0
  containerAdd childLabelBoxAlignment childLabelBox
  boxPackStart childBox childLabelBoxAlignment PackGrow 0

  actionLabelBox3Alignment <- alignmentNew 0 0.5 0 1
  containerAdd actionLabelBox3Alignment actionLabelBox3
  boxPackStart childBox actionLabelBox3Alignment PackGrow 0

  comboBoxes3Alignment <- alignmentNew 0 0.5 0 1
  containerAdd comboBoxes3Alignment comboBoxes3
  boxPackStart childBox comboBoxes3Alignment PackGrow 0

  childBarBoxAlignment <- alignmentNew 1 0.5 1 1
  containerAdd childBarBoxAlignment childBarBox
  boxPackStart childBox childBarBoxAlignment PackGrow 0
  

  childBoxAlignment <- alignmentNew 0 0 0.8 0.3
  containerAdd childBoxAlignment childBox
  boxPackStart levelBox childBoxAlignment PackGrow 0


  levelBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd levelBoxAlignment levelBox
  boxPackStart configBox levelBoxAlignment PackGrow 0


    --SCALE BOXES
  scaleBoxes <- vBoxNew False 2
  
  scaleBoxesSections <- vBoxNew False 2
  
  parentScaleBox <- hBoxNew False 2
  adultScaleBox <- hBoxNew False 2
  childScaleBox <- hBoxNew False 2
  
  rowScaleBox1 <- vBoxNew False 0
  rowScaleBox2 <- vBoxNew False 0
  rowScaleBox3 <- vBoxNew False 0

  --SCALE SECTIONS
  scaleBoxesSections1 <- hBoxNew False 2
  scaleBoxesSections2 <- hBoxNew False 2

  interestLabel <- labelNew (Just "INTEREST LEVELS")

  interestLabelAlignment <- alignmentNew 0.5 0 0 0
  containerAdd interestLabelAlignment interestLabel
  boxPackStart scaleBoxesSections1 interestLabelAlignment PackGrow 0
  
  eatingLabel <- labelNew (Just "EATING")
  workingLabel <- labelNew (Just " WORKING ")
  restingLabel <- labelNew (Just "RESTING")

  eatingLabelAlignment <- alignmentNew 0.5 0 0 0
  containerAdd eatingLabelAlignment eatingLabel
  boxPackStart scaleBoxesSections2 eatingLabelAlignment PackGrow 0

  workingLabelAlignment <- alignmentNew 0.5 0 0 0
  containerAdd workingLabelAlignment workingLabel
  boxPackStart scaleBoxesSections2 workingLabelAlignment PackGrow 0

  restingLabelAlignment <- alignmentNew 0.5 0 0 0
  containerAdd restingLabelAlignment restingLabel
  boxPackStart scaleBoxesSections2 restingLabelAlignment PackGrow 0


  scaleBoxesSections1Alignment <- alignmentNew 0.5 0 0 0
  containerAdd scaleBoxesSections1Alignment scaleBoxesSections1
  boxPackStart scaleBoxesSections scaleBoxesSections1Alignment PackGrow 0

  scaleBoxesSections2Alignment <- alignmentNew 0.5 0 0 0
  containerAdd scaleBoxesSections2Alignment scaleBoxesSections2
  boxPackStart scaleBoxesSections scaleBoxesSections2Alignment PackGrow 0
  
  
  scaleBoxesSectionsAlignment <- alignmentNew 0 0 0 0
  containerAdd scaleBoxesSectionsAlignment scaleBoxesSections
  boxPackStart scaleBoxes scaleBoxesSectionsAlignment PackNatural 0
  

  --PARENT LEVELS
  parentEatScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted parentEatScale True

  parentWorkScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted parentWorkScale True

  parentRestScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted parentRestScale True

  parentEatScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd parentEatScaleAlignment parentEatScale
  boxPackStart parentScaleBox parentEatScaleAlignment PackGrow 0

  parentWorkScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd parentWorkScaleAlignment parentWorkScale
  boxPackStart parentScaleBox parentWorkScaleAlignment PackGrow 0

  parentRestScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd parentRestScaleAlignment parentRestScale
  boxPackStart parentScaleBox parentRestScaleAlignment PackGrow 0

  
  --ADULT LEVELS
  adultEatScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted adultEatScale True

  adultWorkScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted adultWorkScale True

  adultRestScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted adultRestScale True

  adultEatScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd adultEatScaleAlignment adultEatScale
  boxPackStart adultScaleBox adultEatScaleAlignment PackGrow 0

  adultWorkScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd adultWorkScaleAlignment adultWorkScale
  boxPackStart adultScaleBox adultWorkScaleAlignment PackGrow 0

  adultRestScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd adultRestScaleAlignment adultRestScale
  boxPackStart adultScaleBox adultRestScaleAlignment PackGrow 0


  --CHILD LEVELS
  childEatScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted childEatScale True

  childWorkScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted childWorkScale True

  childRestScale <- vScaleNewWithRange 0 100 1
  rangeSetInverted childRestScale True

  childEatScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd childEatScaleAlignment childEatScale
  boxPackStart childScaleBox childEatScaleAlignment PackGrow 0

  childWorkScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd childWorkScaleAlignment childWorkScale
  boxPackStart childScaleBox childWorkScaleAlignment PackGrow 0

  childRestScaleAlignment <- alignmentNew 0.5 0 1 0.7
  containerAdd childRestScaleAlignment childRestScale
  boxPackStart childScaleBox childRestScaleAlignment PackGrow 0



  parentScaleBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd parentScaleBoxAlignment parentScaleBox
  boxPackStart rowScaleBox1 parentScaleBoxAlignment PackGrow 0

  adultScaleBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd adultScaleBoxAlignment adultScaleBox
  boxPackStart rowScaleBox2 adultScaleBoxAlignment PackGrow 0

  childScaleBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd childScaleBoxAlignment childScaleBox
  boxPackStart rowScaleBox3 childScaleBoxAlignment PackGrow 0
  

  rowScaleBox1Alignment <- alignmentNew 0 0 1 1
  containerAdd rowScaleBox1Alignment rowScaleBox1
  boxPackStart scaleBoxes rowScaleBox1Alignment PackGrow 0

  rowScaleBox2Alignment <- alignmentNew 0 0 1 1
  containerAdd rowScaleBox2Alignment rowScaleBox2
  boxPackStart scaleBoxes rowScaleBox2Alignment PackGrow 0

  rowScaleBox3Alignment <- alignmentNew 0 0 1 1
  containerAdd rowScaleBox3Alignment rowScaleBox3
  boxPackStart scaleBoxes rowScaleBox3Alignment PackGrow 0
  

  scaleBoxesAlignment <- alignmentNew 0 0 0 1
  containerAdd scaleBoxesAlignment scaleBoxes
  boxPackStart configBox scaleBoxesAlignment PackGrow 0
  

  configBoxAlignment <- alignmentNew 0 0 1 0.2
  containerAdd configBoxAlignment configBox
  boxPackStart dataBox configBoxAlignment PackGrow 0



  
  -- ######### APARTADO DE GRAFICA DE ESTADOS #########
  mainGraphBox <- vBoxNew False 0

  -- Apartado superior
  upGraphBox <- hBoxNew False 0

  subUpGraphBox1 <- hBoxNew False 0
  subUpGraphBox2 <- hBoxNew False 0
  
  -- STATE-ACTION GRAPH
  upGraphLabelBox <- vBoxNew False 0
  
  upGraphLabel1 <- labelNew (Just "State-Action")
  upGraphLabel2 <- labelNew (Just "Graph")

  upGraphLabel1Alignment <- alignmentNew 0.5 0 0 0
  containerAdd upGraphLabel1Alignment upGraphLabel1
  boxPackStart upGraphLabelBox upGraphLabel1Alignment PackGrow 0

  upGraphLabel2Alignment <- alignmentNew 0.5 0 0 0
  containerAdd upGraphLabel2Alignment upGraphLabel2
  boxPackStart upGraphLabelBox upGraphLabel2Alignment PackGrow 0

  upGraphLabelBoxAlignment <- alignmentNew 0 1 0.3 0
  containerAdd upGraphLabelBoxAlignment upGraphLabelBox
  boxPackStart subUpGraphBox1 upGraphLabelBoxAlignment PackGrow 10


    -- Battery
  batteryBox <- vBoxNew False 0
  
  batteryLabel <- labelNew (Just "Battery")
  
  batteryArea <- drawingAreaNew
  on batteryArea draw $ drawBattery adjustment batteryArea


  batteryLabelAlignment <- alignmentNew 0 0 1 0
  containerAdd batteryLabelAlignment batteryLabel
  boxPackStart batteryBox batteryLabelAlignment PackNatural 0

  widgetSetSizeRequest batteryArea 40 70
  batteryAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd batteryAreaAlignment batteryArea
  boxPackStart batteryBox batteryAreaAlignment PackGrow 0

  batteryBoxAlignment <- alignmentNew 0 0 1 0.7
  containerAdd batteryBoxAlignment batteryBox
  boxPackStart subUpGraphBox1 batteryBoxAlignment PackGrow 0

  
    --Performance
  performanceBox <- vBoxNew False 0

  subPerformanceBox <- vBoxNew False 0

  performanceLabel <- labelNew (Just "Performance")

  performanceArea <- drawingAreaNew
  on performanceArea draw $ drawPerformanceBar adjustment performanceArea


  performanceLabelAlignment <- alignmentNew 0 0 0 0
  containerAdd performanceLabelAlignment performanceLabel
  boxPackStart subPerformanceBox performanceLabelAlignment PackNatural 0

  widgetSetSizeRequest performanceArea 80 10
  performanceAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd performanceAreaAlignment performanceArea
  boxPackStart subPerformanceBox performanceAreaAlignment PackGrow 0

  subPerformanceBoxAlignment <- alignmentNew 0 0 0.2 1
  containerAdd subPerformanceBoxAlignment subPerformanceBox
  boxPackStart performanceBox subPerformanceBoxAlignment PackGrow 0


      --Rate data
  rateDataBox <- hBoxNew False 0

  normalRateDataBox <- vBoxNew False 0
  
  normalRateDataLabel <- labelNew (Just "Rate")
  normalRateDataEntry <- spinButtonNewWithRange 0 1000 10


  normalRateDataLabelAlignment <- alignmentNew 0.1 0 0 0
  containerAdd normalRateDataLabelAlignment normalRateDataLabel
  boxPackStart normalRateDataBox normalRateDataLabelAlignment PackGrow 0

  normalRateDataEntryAlignment <- alignmentNew 0 0 0 0
  containerAdd normalRateDataEntryAlignment normalRateDataEntry
  boxPackStart normalRateDataBox normalRateDataEntryAlignment PackGrow 0


  realRateDataBox <- vBoxNew False 0
  
  realRateDataLabel <- labelNew (Just "Real Rate")
 
  realRateDataStatus <- statusbarNew
  statusbarPush realRateDataStatus 1 "0"

  
  realRateDataLabelAlignment <- alignmentNew 0.5 0 0 0
  containerAdd realRateDataLabelAlignment realRateDataLabel
  boxPackStart realRateDataBox realRateDataLabelAlignment PackGrow 0

  realRateDataStatusAlignment <- alignmentNew 0.5 0 0 0
  containerAdd realRateDataStatusAlignment realRateDataStatus
  boxPackStart realRateDataBox realRateDataStatusAlignment PackGrow 0

  
  normalRateDataBoxAlignment <- alignmentNew 0 0 0 0
  containerAdd normalRateDataBoxAlignment normalRateDataBox
  boxPackStart rateDataBox normalRateDataBoxAlignment PackGrow 0

  realRateDataBoxAlignment <- alignmentNew 1 0 0 0
  containerAdd realRateDataBoxAlignment realRateDataBox
  boxPackStart rateDataBox realRateDataBoxAlignment PackGrow 10


  rateDataBoxAlignment <- alignmentNew 0 0 0 0
  containerAdd rateDataBoxAlignment rateDataBox
  boxPackStart performanceBox rateDataBoxAlignment PackNatural 0

  performanceBoxAlignment <- alignmentNew 0 0 0 0.3
  containerAdd performanceBoxAlignment performanceBox
  boxPackStart subUpGraphBox1 performanceBoxAlignment PackNatural 10


    -- Token
  tokenBox <- vBoxNew False 0

  tokenLabel <- labelNew (Just "Token")
  
  tokenStatus <- statusbarNew
  statusbarPush tokenStatus 1 "0"


  tokenLabelAlignment <- alignmentNew 0 0 0 0
  containerAdd tokenLabelAlignment tokenLabel
  boxPackStart tokenBox tokenLabelAlignment PackGrow 0

  tokenStatusAlignment <- alignmentNew 0 0 0 0
  containerAdd tokenStatusAlignment tokenStatus
  boxPackStart tokenBox tokenStatusAlignment PackGrow 0

  tokenBoxAlignment <- alignmentNew 0 0.43 0 0
  containerAdd tokenBoxAlignment tokenBox
  boxPackStart subUpGraphBox1 tokenBoxAlignment PackNatural 0

  
  subUpGraphBox1Alignment <- alignmentNew 0 0 1 1
  containerAdd subUpGraphBox1Alignment subUpGraphBox1
  boxPackStart upGraphBox subUpGraphBox1Alignment PackNatural 0


  
  --SUBUPGRAPHBOX2
  -- Comportamiento de energias
  labelEnergyBox <- vBoxNew False 0

  hungryLabel <- labelNew (Just "Hungry")
  respLabel <- labelNew (Just "Resp")
  tiredLabel <- labelNew (Just "Tired")
  maxinumLabel <- labelNew (Just "Maxinum")
  

  hungryLabelAlignment <- alignmentNew 1 0.5 0 0
  containerAdd hungryLabelAlignment hungryLabel
  boxPackStart labelEnergyBox hungryLabelAlignment PackGrow 0

  respLabelAlignment <- alignmentNew 1 0.5 0 0
  containerAdd respLabelAlignment respLabel
  boxPackStart labelEnergyBox respLabelAlignment PackGrow 0

  tiredLabelAlignment <- alignmentNew 1 0.5 0 0
  containerAdd tiredLabelAlignment tiredLabel
  boxPackStart labelEnergyBox tiredLabelAlignment PackGrow 0

  maxinumLabelAlignment <- alignmentNew 1 0.5 0 0
  containerAdd maxinumLabelAlignment maxinumLabel
  boxPackStart labelEnergyBox maxinumLabelAlignment PackGrow 0


  labelEnergyBoxAlignment <- alignmentNew 0 0 0 1
  containerAdd labelEnergyBoxAlignment labelEnergyBox
  boxPackStart subUpGraphBox2 labelEnergyBoxAlignment PackNatural 5


  -- GRAFICAS DE ENERGIAS
  graphicsEnergyBox <- vBoxNew False 0


  hungryArea <- drawingAreaNew
  on hungryArea draw $ drawEnergyChart adjustment hungryArea

  respArea <- drawingAreaNew
  on respArea draw $ drawEnergyChart adjustment respArea

  tiredArea <- drawingAreaNew
  on tiredArea draw $ drawEnergyChart adjustment tiredArea

  maxinumArea <- drawingAreaNew
  on maxinumArea draw $ drawEnergyChart adjustment maxinumArea


  widgetSetSizeRequest hungryArea 80 20  
  hungryAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd hungryAreaAlignment hungryArea
  boxPackStart graphicsEnergyBox hungryAreaAlignment PackGrow 0

  widgetSetSizeRequest respArea 80 20
  respAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd respAreaAlignment respArea
  boxPackStart graphicsEnergyBox respAreaAlignment PackGrow 0

  widgetSetSizeRequest tiredArea 80 20
  tiredAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd tiredAreaAlignment tiredArea
  boxPackStart graphicsEnergyBox tiredAreaAlignment PackGrow 0

  widgetSetSizeRequest maxinumArea 80 20
  maxinumAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd maxinumAreaAlignment maxinumArea
  boxPackStart graphicsEnergyBox maxinumAreaAlignment PackGrow 0


  graphicsEnergyBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd graphicsEnergyBoxAlignment graphicsEnergyBox
  boxPackStart subUpGraphBox2 graphicsEnergyBoxAlignment PackGrow 0


  subUpGraphBox2Alignment <- alignmentNew 0 0 0.65 1
  containerAdd subUpGraphBox2Alignment subUpGraphBox2
  boxPackStart upGraphBox subUpGraphBox2Alignment PackGrow 0


  upGraphBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd upGraphBoxAlignment upGraphBox
  boxPackStart mainGraphBox upGraphBoxAlignment PackGrow 0



  -- GRAFICOS GENERALES
  graphsBoxes <- hBoxNew False 0
  
    --GRAFICA DE BARRAS
  mainHistogramBox <- hBoxNew False 0
  
  histogramValuesRef <- newIORef [(100, 50, 50, 100), (100, 50, 50, 100), (100, 50, 50, 100)]

  histogramArea <- drawingAreaNew
  on histogramArea draw $ drawHistogramHandler adjustment histogramArea histogramValuesRef
  timeoutAdd (updateHistogramChart histogramArea histogramValuesRef) 1000

  
  actionBox <- vBoxNew False 0

  actionLabel <- labelNew (Just "ACTION")
  
  actionStatus <- statusbarNew
  statusbarPush actionStatus 1 "WORKING"

  widgetSetSizeRequest histogramArea 150 300
  histogramAreaAlignment <- alignmentNew 0 0 1 1
  containerAdd histogramAreaAlignment histogramArea
  boxPackStart mainHistogramBox histogramAreaAlignment PackGrow 0


  actionLabelAlignment <- alignmentNew 0.5 0 0 0
  containerAdd actionLabelAlignment actionLabel
  boxPackStart actionBox actionLabelAlignment PackGrow 0

  actionStatusAlignment <- alignmentNew 0.5 0 0 0
  containerAdd actionStatusAlignment actionStatus
  boxPackStart actionBox actionStatusAlignment PackGrow 0
  

  actionBoxAlignment <- alignmentNew 0 0.2 0 0
  containerAdd actionBoxAlignment actionBox
  boxPackStart mainHistogramBox actionBoxAlignment PackNatural 0

  
  mainHistogramBoxAlignment <- alignmentNew 0 0 0.8 0
  containerAdd mainHistogramBoxAlignment mainHistogramBox
  boxPackStart agentBox mainHistogramBoxAlignment PackGrow 0
  

  -- DEBAJO DE LAS GRAFICAS DE ESTADOS
  belowGraphBox <- hBoxNew False 0

  belowGraphLabel <- labelNew (Just "PARENT ACTION")

  belowGraphicStatus <- statusbarNew
  statusbarPush belowGraphicStatus 1 "To Work"
  
  belowGraphLabelAlignment <- alignmentNew 0 0.5 0 0
  containerAdd belowGraphLabelAlignment belowGraphLabel
  boxPackStart belowGraphBox belowGraphLabelAlignment PackNatural 0
  
  belowGraphicStatusAlignment <- alignmentNew 0 0 0 0
  containerAdd belowGraphicStatusAlignment belowGraphicStatus
  boxPackStart belowGraphBox belowGraphicStatusAlignment PackNatural 0

  belowGraphBoxAlignment <- alignmentNew 0 0 0 0
  containerAdd belowGraphBoxAlignment belowGraphBox
  boxPackStart agentBox belowGraphBoxAlignment PackNatural 0


  agentBoxAlignment <- alignmentNew 0 0 1 1
  containerAdd agentBoxAlignment agentBox
  boxPackStart graphsBoxes agentBoxAlignment PackGrow 0 



    -- GRAFICAS DE ESTADOS
  graphicsBoxes <- vBoxNew False 0

  graphicsBox1 <- hBoxNew False 0
  graphicsBox2 <- hBoxNew False 0
  graphicsBox3 <- hBoxNew False 0

  -- Crear referencia IO para almacenar los valores
  valuesRef <- newIORef [(0, 10), (1, 30), (2, 20), (3, 100), (4, 0)] -- Valores iniciales

    --PARENT
  graphParentLabelBox <- vBoxNew False 0

  graphParentLabel1 <- labelNew (Just "P")
  graphParentLabel2 <- labelNew (Just "A")
  graphParentLabel3 <- labelNew (Just "R")
  graphParentLabel4 <- labelNew (Just "E")
  graphParentLabel5 <- labelNew (Just "N")
  graphParentLabel6 <- labelNew (Just "T")

  boxPackStart graphParentLabelBox graphParentLabel1 PackNatural 0
  boxPackStart graphParentLabelBox graphParentLabel2 PackNatural 0
  boxPackStart graphParentLabelBox graphParentLabel3 PackNatural 0
  boxPackStart graphParentLabelBox graphParentLabel4 PackNatural 0
  boxPackStart graphParentLabelBox graphParentLabel5 PackNatural 0
  boxPackStart graphParentLabelBox graphParentLabel6 PackNatural 0

  graphicsArea1 <- drawingAreaNew
  on graphicsArea1 draw $ drawGraphic adjustment graphicsArea1 valuesRef
  void $ timeoutAdd (updateChart graphicsArea1 valuesRef) 1000


  graphParentLabelBoxAlignment <- alignmentNew 0 0.5 0 0
  containerAdd graphParentLabelBoxAlignment graphParentLabelBox
  boxPackStart graphicsBox1 graphParentLabelBoxAlignment PackNatural 0

  widgetSetSizeRequest graphicsArea1 520 150
  graphArea1Alignment <- alignmentNew 0 0 1 1
  containerAdd graphArea1Alignment graphicsArea1
  boxPackStart graphicsBox1 graphArea1Alignment PackGrow 0

  graphicsBox1Alignment <- alignmentNew 0 0 1 1
  containerAdd graphicsBox1Alignment graphicsBox1
  boxPackStart graphicsBoxes graphicsBox1Alignment PackGrow 0


    --ADULT
  graphAdultLabelBox <- vBoxNew False 0

  graphAdultLabel1 <- labelNew (Just "A")
  graphAdultLabel2 <- labelNew (Just "D")
  graphAdultLabel3 <- labelNew (Just "U")
  graphAdultLabel4 <- labelNew (Just "L")
  graphAdultLabel5 <- labelNew (Just "T")

  boxPackStart graphAdultLabelBox graphAdultLabel1 PackNatural 0
  boxPackStart graphAdultLabelBox graphAdultLabel2 PackNatural 0
  boxPackStart graphAdultLabelBox graphAdultLabel3 PackNatural 0
  boxPackStart graphAdultLabelBox graphAdultLabel4 PackNatural 0
  boxPackStart graphAdultLabelBox graphAdultLabel5 PackNatural 0

  graphicsArea2 <- drawingAreaNew
  on graphicsArea2 draw $ drawGraphic adjustment graphicsArea2 valuesRef
  void $ timeoutAdd (updateChart graphicsArea2 valuesRef) 1000


  graphAdultLabelBoxAlignment <- alignmentNew 0 0.5 0 0
  containerAdd graphAdultLabelBoxAlignment graphAdultLabelBox
  boxPackStart graphicsBox2 graphAdultLabelBoxAlignment PackNatural 0

  widgetSetSizeRequest graphicsArea2 520 150
  graphArea2Alignment <- alignmentNew 0 0 1 1
  containerAdd graphArea2Alignment graphicsArea2
  boxPackStart graphicsBox2 graphArea2Alignment PackGrow 0

  graphicsBox2Alignment <- alignmentNew 0 0 1 1
  containerAdd graphicsBox2Alignment graphicsBox2
  boxPackStart graphicsBoxes graphicsBox2Alignment PackGrow 0


    --CHILD
  graphChildLabelBox <- vBoxNew False 0

  graphChildLabel1 <- labelNew (Just "C")
  graphChildLabel2 <- labelNew (Just "H")
  graphChildLabel3 <- labelNew (Just "I")
  graphChildLabel4 <- labelNew (Just "L")
  graphChildLabel5 <- labelNew (Just "D")

  boxPackStart graphChildLabelBox graphChildLabel1 PackNatural 0
  boxPackStart graphChildLabelBox graphChildLabel2 PackNatural 0
  boxPackStart graphChildLabelBox graphChildLabel3 PackNatural 0
  boxPackStart graphChildLabelBox graphChildLabel4 PackNatural 0
  boxPackStart graphChildLabelBox graphChildLabel5 PackNatural 0

  graphicsArea3 <- drawingAreaNew
  on graphicsArea3 draw $ drawGraphic adjustment graphicsArea3 valuesRef
  void $ timeoutAdd (updateChart graphicsArea3 valuesRef) 1000

  
  graphChildLabelBoxAlignment <- alignmentNew 0 0.5 0 0
  containerAdd graphChildLabelBoxAlignment graphChildLabelBox
  boxPackStart graphicsBox3 graphChildLabelBoxAlignment PackNatural 0

  widgetSetSizeRequest graphicsArea3 520 150
  graphArea3Alignment <- alignmentNew 0 0 1 1
  containerAdd graphArea3Alignment graphicsArea3
  boxPackStart graphicsBox3 graphArea3Alignment PackGrow 0

  graphicsBox3Alignment <- alignmentNew 0 0 1 1
  containerAdd graphicsBox3Alignment graphicsBox3
  boxPackStart graphicsBoxes graphicsBox3Alignment PackGrow 0


  graphicsBoxesAlignment <- alignmentNew 0 0.5 0.3 1
  containerAdd graphicsBoxesAlignment graphicsBoxes
  boxPackStart graphsBoxes graphicsBoxesAlignment PackGrow 0
  

  graphsBoxesAlignment <- alignmentNew 0 0 1 1
  containerAdd graphsBoxesAlignment graphsBoxes
  boxPackStart mainGraphBox graphsBoxesAlignment PackGrow 0




  -- ###### CONEXION DE LOS WIDGETS AL MAINBOX ######
  boxPackStart mainBox dataBox PackNatural 0

  mainGraphBoxAlignment <- alignmentNew 0 0 1 0.37
  containerAdd mainGraphBoxAlignment mainGraphBox
  boxPackStart mainBox mainGraphBoxAlignment PackGrow 0
  --boxPackStart mainBox mainGraphBox PackNatural 0

  -- containerAdd mainBox scrolledWindow

  -- Cargar y aplicar el archivo CSS
  cssProvider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath cssProvider (pack "C:\\Users\\ASUS\\Documents\\UABC\\Servicio Profesional\\Agentes_SP\\src\\style.css")
  screen <- Gdk.screenGetDefault
  case screen of
      Just scr -> Gtk.styleContextAddProviderForScreen scr cssProvider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
      Nothing  -> putStrLn "No se pudo obtener la pantalla"

  widgetShowAll window
  mainGUI
