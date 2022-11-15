
extensions [ matrix ]

breed [hives hive]

breed [bees bee]

breed [patchStatistics patchStatistic ]

bees-own
[
  destinationList
  flightPhase
  furthestDistance_m
  furthestLocationList
  knownPatchesList
  lastDestinationList
  lastLocationList
  nowDetectedPatchesList
  nTrips
  time_s
]


patches-own
[
  flowerPatchID
  firstPatchOfFlowerpatch
  mapDisplay
  originalColor
  patchColor
  satelliteColor
  visits
]

patchStatistics-own
[
  areaPx
  areaSqm
  calculatedDetectionProb_per_trip
  closestDistance_m
  concentration
  detects1000List
  firstDetection_s
  maxDetectProb_per_s
  meanDistance_m
  modelledDetectionProb_per_s
  modelledDetectionProb_per_trip
  nectarGathering_s
  nNectarVisits
  nPollenVisits
  patchType
  pollenGathering_s
  quantityNectar_l
  quantityPollen_g
  startDay
  stopDay
  timeMaxDetectProb_s
  totalDetections
  visitColor
]


globals
[
  BackgroundColor
  BorderColor
  BottomBorder
  ForagingFileList
  Gap_s
  LakeColor
  LeftBorder
  MaxHiveDisplAllBees_m
  MixedStratCol
  MixedStratInd
  NewTimestepForaging
  Npatches
  OutputFileList
  OutputWordResult
  PatchColoursList
  Repetitions
  RightBorder
  Scaling
  TopBorder
  TotalTrips
  WriteToFile
]

;; ================================================================================================================================================================================

to Setup   ; Setup Procedure  ; RUN A PROCEDURE

  __clear-all-and-reset-ticks
 ;if RandSeed > 0 [ random-seed RandSeed ]

  if RandSeed != 0 [ random-seed RandSeed ]
  set OutputFileList []  ;; list, in which the results for the output file will be written
  set Gap_s 3 ; 3s: gap between 2 recordings of a bee in radar data Rothamsted - and hence also duration of a "tick" (timestep)
  set MixedStratInd false
  set MixedStratCol false

  set LakeColor cyan
  set BorderColor 125.12345678987654 ; a unique color most likely not occuring on the map (~magenta)
  set OutputWordResult ""
  create-hives 1
    [
      set color 33
      set shape "square"
      set size 4
      setxy Col_X Col_Y
    ]
  ask patches [ set pcolor BorderColor ] ; pcolor set to a unique color, to determine the borders of the map
  set PatchColoursList []
  if RedPatches = true [ set PatchColoursList fput red PatchColoursList ]
  if GreenPatches = true [ set PatchColoursList fput green PatchColoursList ]
  if YellowPatches = true [ set PatchColoursList fput yellow PatchColoursList ]
  if BluePatches = true [ set PatchColoursList fput blue PatchColoursList ]
  set Scaling ScaleDistance_m / (Scale_X2 - Scale_X1) ;; real distance [m] divided by distance of grid points

  ifelse InputFile != "No input file"
    [
      ImportMapProc                   ;;loads the crop map    ; CALL A PROCEDURE
      if count patches with [ originalColor = BorderColor ] > 0 [ DetermineBordersProc ]  ; CALL A PROCEDURE
     ]
    [
      ask patches [   ; ELSE (if there is no input file):
                    set pcolor grey
                    set patchColor grey
                    set originalColor grey
                  ]
    ]

  if SatelliteFile != "No satellite image"
    [
      import-pcolors SatelliteFile
      ask patches [
                     set satelliteColor pcolor
                     set pcolor patchcolor
                  ]
    ]

  set WriteToFile false

  AnalyseProc   ; CALL A PROCEDURE
  ask patches [set pcolor grey ]
  ask patches with [  member? patchColor PatchColoursList = true] [ set pcolor patchColor ]
  if Lakes = true [ ask patches with [ patchColor = LakeColor ] [ set pcolor LakeColor ] ]
  if count patches with [ pcolor = LakeColor ] / (max-pxcor * max-pycor) > 0.5
    [ user-message "There are a lot of lakes on your map!" ]

end

;; ================================================================================================================================================================================

to DetermineBordersProc   ; RUN A PROCEDURE
  set TopBorder min [ pycor ] of patches with [(pycor > (max-pycor / 2)) and (originalColor = BorderColor)]      ; determine the borders of the world: top ..
  set BottomBorder max [ pycor ] of patches with [(pycor < (max-pycor / 2)) and (originalColor = BorderColor)]    ;   ...and bottom
  if TopBorder - BottomBorder <= 2
     [
       set TopBorder max-pycor + 1      ; i.e. outside the "world"
       set BottomBorder min-pycor - 1
     ]

  set RightBorder min [ pxcor ] of patches with [(pxcor > (max-pxcor / 2)) and (originalColor = BorderColor)]      ; determine the borders of the world: right ..
  set LeftBorder max [ pxcor ] of patches with [(pxcor < (max-pxcor / 2)) and (originalColor = BorderColor)]      ;   ...and left
  if RightBorder - LeftBorder <= 2
     [
       set RightBorder max-pxcor + 1   ; i.e. outside the "world"
       set LeftBorder min-pxcor - 1
     ]

  ask patches with [(pycor = TopBorder) or (pycor = BottomBorder) or (pxcor = LeftBorder) or (pxcor = RightBorder)]
    [ set originalColor BorderColor ]

  ask patches with [ originalColor = BorderColor ]  [ set pcolor BorderColor ]
end

;; ================================================================================================================================================================================

to ImportMapProc ;;loads the crop map, corrects colours    ; RUN A PROCEDURE
  import-pcolors InputFile
  ask patches
  [
    set originalColor pcolor  ;; the color of the (grid) patch in the original file
    set plabel-color black
    set flowerPatchID -1
    if remainder pcolor 10 < Black_th [ set pcolor black ]
    if remainder pcolor 10 > White_th [ set pcolor white ]
    if (pcolor > Red_min) and (pcolor <= Red_max) [ set pcolor red ]
    if (pcolor > Yellow_min) and (pcolor <= Yellow_max) [ set pcolor yellow ]
    if (pcolor > Green_min) and (pcolor <= Green_max) [ set pcolor green ]
    if (pcolor > Blue_min) and (pcolor <= Blue_max) [ set pcolor blue ] ; Rothamsted
    set patchColor pcolor  ;; saves the color after rounding
   ]
end

;; ================================================================================================================================================================================

to AnalyseProc  ;; determination of flower patches, calls procedures to create scout bees, and exploration of landscape   ; RUN A PROCEDURE
  clear-turtles
  let currentColor 0
  let currentPatchID -1
  let flowerPatchCounter 0
  set Repetitions round (MaxPatchRadius_m / Scaling)
    ; # of repetition depends on the scale of the landscape

  foreach sort patches
  [
    ask ?   ;; determines flowerpatches: coloured (r,b,y,g) (nlogo-)patches with ID -1 are searched, it gets a new ID and all connected (nlogo-)patches with the same colour get the same ID
    [
      if member? patchColor PatchColoursList
      [
        if flowerPatchID = -1 ;; if patch is not identified yet
        [
          set flowerPatchID (flowerPatchCounter)
          set currentColor pcolor ;; colour of the flower patch is the colour of the "firstPatchOfFlowerpatch"
          set firstPatchOfFlowerpatch true
          set currentPatchID flowerPatchID
          repeat Repetitions
          [
            ask patches  with [(pcolor = currentColor) and (flowerPatchID = currentPatchID)] ;; connected (nlogo-)patches around the firstPatchOfFlowerpatch are searched defined as part of this flower patch
            [
              ask neighbors with [(pcolor = currentColor) and (flowerPatchID = -1)]
                [ set flowerPatchID currentPatchID ]
             ]
          ]
          set flowerPatchCounter flowerPatchCounter + 1
          ]
        ]
        set pcolor lime  ; to show progress of map analysis
     ]
  ]
  set Npatches currentPatchID + 1  ; as 1st patch has id 0
  CreatePatchStatisticsProc ;; creates "patchStatistics" (turtles) to store data of the flower patches   ; CALL A PROCEDURE
  create-hives 1
    [
      set color 33
      set shape "square"
      set size 4
      setxy Col_X Col_Y
    ]
  CreateBeesProc   ;; creates scouts (turtles)   ; CALL A PROCEDURE
  DetermineSizeProc                              ; CALL A PROCEDURE
  if Lakes = true
    [
      ask patches with [patchColor = black ]
        [ set patchColor LakeColor ]
    ]

  if count patchstatistics > 0
  [
    if Plot1 > count patchstatistics - 1
      or Plot2 > count patchstatistics - 1
      or Plot3 > count patchstatistics - 1
      or Plot4 > count patchstatistics - 1
      [
        set Plot1 min [ who ] of patchstatistics
        set Plot2 round ((mean [ who ] of patchstatistics + min [ who ] of patchstatistics) / 2)
        set Plot3 round ((mean [ who ] of patchstatistics + max [ who ] of patchstatistics) / 2)
        set Plot4 max [ who ] of patchstatistics
      ]
  ]

end

;; ================================================================================================================================================================================


to CreatePatchStatisticsProc  ;; "patchStatistics": turtles, containing the relevant infos of the flowerPatches, slightly different colour, located within the flowerPatch   ; RUN A PROCEDURE
let currentXcor 0
let currentYcor 0
let currentWho 0
let currentColor 0
create-PatchStatistics Npatches
     [
       set size 1
       set shape "circle"
       set currentWho Who
       ask one-of patches with [ flowerPatchID = currentWho ]
         [
           set currentXcor pxcor
           set currentYcor pycor
           set currentColor patchcolor
         ]
       setxy currentXcor currentYcor
       set color currentColor - 1
       set label-color white
       ;if color = black [ set label-color white]
       set label who
       set modelledDetectionProb_per_s 0
       set modelledDetectionProb_per_trip 0
       set totalDetections 0
       set timeMaxDetectProb_s 0
       set detects1000List []
       set maxDetectProb_per_s 0
       if patchColor = red
        [
          set patchType PATCHTYPE_R
          set concentration Conc_R
          set nectarGathering_s t_Nectar_R
          set pollenGathering_s t_Pollen_R
          set startDay Start_R
          set stopDay Stop_R
        ]
       if patchColor = blue
        [
          set patchType PATCHTYPE_B
          set concentration Conc_B
          set nectarGathering_s t_Nectar_B
          set pollenGathering_s t_Pollen_B
          set startDay Start_B
          set stopDay Stop_B
        ]
       if patchColor = yellow
        [
          set patchType PATCHTYPE_Y
          set concentration Conc_Y
          set nectarGathering_s t_Nectar_Y
          set pollenGathering_s t_Pollen_Y
          set startDay Start_Y
          set stopDay Stop_Y
        ]
       if patchColor = green
        [
          set patchType PATCHTYPE_G
          set concentration Conc_G
          set nectarGathering_s t_Nectar_G
          set pollenGathering_s t_Pollen_G
          set startDay Start_G
          set stopDay Stop_G
        ]
     ]
end

;; ================================================================================================================================================================================

to-report CalculateDetectProbREP;; calculates the detection probability of a flower patch based on distance; see Simulation 2 in the BEESCOUT publication  ; RUN A PROCEDURE
   let lambda -0.00073 ; see BEESCOUT publication, Tab. 1, fit to "random location" search mode
   report e ^ (lambda * meanDistance_m)
end

;; ================================================================================================================================================================================


to DetermineSizeProc  ;; determines the size and distance to colony of the flower patches, writes data in short list and in output list   ; RUN A PROCEDURE
 let currentWho 0
 let shortList []
 ;;ask patchStatistics [
 foreach sort patchStatistics [ ask ? [
   let sumDistance 0
   let currentClosestDistanceM 999999 ;; Creation of LIST for OUTPUT file:
   set currentWho Who
   set areaPx (count patches with [flowerPatchID = currentWho])
   ask patches with [flowerPatchID = currentWho]
    [
      set sumDistance sumDistance + distancexy Col_X Col_Y
      if distancexy Col_X Col_Y < currentClosestDistanceM
         [ set currentClosestDistanceM distancexy Col_X Col_Y ]
    ]
   set areaSqm round(areaPx * Scaling * Scaling)

   if color = red - 1
        [
          set quantityNectar_l (Nectar_R / 1000) * areaSqm  ; (Nectar_R: ml)
          set quantityPollen_g Pollen_R * areaSqm
        ]
   if color = green - 1
        [
          set quantityNectar_l (Nectar_G / 1000) * areaSqm  ; (Nectar_G: ml)
          set quantityPollen_g Pollen_G * areaSqm
        ]
   if color = yellow - 1
        [
          set quantityNectar_l (Nectar_Y / 1000) * areaSqm  ; (Nectar_Y: ml)
          set quantityPollen_g Pollen_Y * areaSqm
        ]
   if color = blue - 1
        [
          set quantityNectar_l (Nectar_B / 1000) * areaSqm  ; (Nectar_B: ml)
          set quantityPollen_g Pollen_B * areaSqm
        ]

   set meanDistance_m precision((sumDistance / areaPx) * Scaling) 1  ;; [m]
   set closestDistance_m precision (currentClosestDistanceM * Scaling) 1  ;; [m]
   if closestDistance_m < 0.1 [ set closestDistance_m 0.1 ]  ; to avoid division by 0
   set calculatedDetectionProb_per_trip CalculateDetectProbREP

   ;; Headline (added in the "WriteFromListToFileProc"):  1. who 2. xcor 3. ycor 4. patchType 5. closestDistanceM 6. areaSqm 7. modelledDetectProb
   set shortList lput Who shortList               ;; 1st column (who)
   set shortList lput xcor shortList              ;; 2nd column (xcor)
   set shortList lput ycor shortList              ;; 3rd column (ycor)
   set shortList lput patchType shortList         ;; 4th column (patchType)
   set shortList lput closestDistance_m shortList  ;; 5th column (closestDistance)
   set shortList lput areaSqm shortList           ;; 6th column (area)
   ;;                                             ;; 7th column: modelledDetectionProb - is added in the "WriteFromListToFileProc"
   set OutputFileList lput shortList OutputFileList
   set shortList []
   ] ]
end

;; ================================================================================================================================================================================

to CreateBeesProc  ;; creates scouts to explore the landscape and to determine the detection probabilities of the flower patches   ; RUN A PROCEDURE
  ask bees [ die ]
  set-current-plot "Detection Probability per trip" clear-plot
  ask patches [ set visits 0 ]
  ask patchstatistics                     ; flower pacthes have to be reset
   [
     set modelledDetectionProb_per_s 0
     set modelledDetectionProb_per_trip 0
     ;set maxDetectProb_per_s 0
     set totalDetections 0
     set firstDetection_s 0
     set detects1000List []
     set maxDetectProb_per_s 0
     set timeMaxDetectProb_s 0
   ]
  create-bees N_Bees
    [
      setxy Col_X Col_Y
      set destinationList []
      set lastDestinationList list xcor ycor ; []
      set furthestLocationList list xcor ycor
      set furthestDistance_m 0
      set lastLocationList list xcor ycor
      set shape "bee_mb_1"
      set color white    ; bees start in the "search" mode (flightPhase 2) when they first leave the hive
      set flightPhase 2
      set size 5    ; beesize
      set knownPatchesList []
      set nTrips 1 ; 0
      set nowDetectedPatchesList []
    ]
  set MixedStratInd false
  set MixedStratCol false
end

;; ================================================================================================================================================================================


to Go  ;; Go Procedure movement of scouts, detection of flower patches   ; RUN A PROCEDURE
  tick
  if (ticks * Gap_s > ScoutingPeriod_hrs * 3600) or (min [ nTrips ] of bees > MaxTrips)
    [
      set OutputWordResult (word "Npatches Who areaSqm meanDistance_m modelledDetectionProb_per_s modelledDetectionProb_per_trip "  count patchStatistics)
      foreach sort patchStatistics
      [
       ask ?
        [ ; creates output for BehaviorSpace
          set OutputWordResult (word OutputWordResult " " who " " areaSqm " " meanDistance_m " " modelledDetectionProb_per_s " " modelledDetectionProb_per_trip)
        ]
      ]
      set OutputWordResult (word OutputWordResult " END")
      stop
    ]
  if remainder ticks 1000 = 0
    [  ask patchStatistics
      [ set detects1000List lput totalDetections detects1000List ]
    ]
  MoveBeesProc   ; CALL A PROCEDURE
  if count PatchStatistics > 0 [ DoDetectionPlotsProc ]   ; CALL A PROCEDURE
end



;; ================================================================================================================================================================================

to AnalyseOutfileProc     ;   (only) called by Analyse & Outfile button        ; RUN A PROCEDURE
  ; user-message ("Please wait while the button remains black. This may take a few minutes")
   Setup
   set WriteToFile true
   if is-string? NameOutfile  ;; We check to make sure we actually got a string just in case  the user hits the cancel button.
      [ if file-exists? nameOutfile   ;; If the file already exists, we begin by deleting it, otherwise new data would be appended to the old contents.
             [ file-delete nameOutfile ]
        file-open NameOutfile
       ]
   repeat ScoutingPeriod_hrs * 3600 / Gap_s
    [ Go ]
   WriteFromListToFileProc   ; CALL A PROCEDURE
   file-close
   set WriteToFile false
end


;; ================================================================================================================================================================================

to-report DisplacementREP ; [ time ]     ; RUN A PROCEDURE
let displ_m 0
 let result 0
 if BeeSpecies = "Honeybees"
   [ set displ_m 2.96 * Gap_s ]  ; 2.96 m/s times (3s) time step (derived from harmonic radar dataset Emma Wright (PhD thesis))
 if BeeSpecies = "Bumblebees"
   [ set displ_m 3.22 * Gap_s  ] ; 3.22 m/s times (3s) time step (derived from harmonic radar dataset, Osborne et al 2013)
 let displ_NLpatches displ_m / Scaling
 set result displ_NLpatches * DisplacementFactor    ; DisplacementFactor: slider (GUI), default: 1
 report result
end

;; ================================================================================================================================================================================

to-report TurningREP  ; reports rt, the angle a bee turns to its RIGHT ; RUN A PROCEDURE

 let turn 0

 if BeeSpecies = "Honeybees"
 [
   let randBar random 628 + 1 ; randomly chosen bar from the empirical turning angle histogram with a total of 628 turning angles (returns random number between [1..628]
   ; 18 bars, each 10 degrees, assuming symmetry of turning right and left  (derived from harmonic radar dataset Emma Wright (PhD thesis))

   if randBar >= 1 and randBar < 92 [ set turn 0 + random-float 10 ]
   if randBar >= 92 and randBar < 144 [ set turn 10 + random-float 10 ]
   if randBar >= 144 and randBar < 206 [ set turn 20 + random-float 10 ]
   if randBar >= 206 and randBar < 262 [ set turn 30 + random-float 10 ]
   if randBar >= 262 and randBar < 302 [ set turn 40 + random-float 10 ]
   if randBar >= 302 and randBar < 343 [ set turn 50 + random-float 10 ]
   if randBar >= 343 and randBar < 363 [ set turn 60 + random-float 10 ]
   if randBar >= 363 and randBar < 396 [ set turn 70 + random-float 10 ]
   if randBar >= 396 and randBar < 419 [ set turn 80 + random-float 10 ]
   if randBar >= 419 and randBar < 440 [ set turn 90 + random-float 10 ]
   if randBar >= 440 and randBar < 456 [ set turn 100 + random-float 10 ]
   if randBar >= 456 and randBar < 483 [ set turn 110 + random-float 10 ]
   if randBar >= 483 and randBar < 506 [ set turn 120 + random-float 10 ]
   if randBar >= 506 and randBar < 526 [ set turn 130 + random-float 10 ]
   if randBar >= 526 and randBar < 542 [ set turn 140 + random-float 10 ]
   if randBar >= 542 and randBar < 570 [ set turn 150 + random-float 10 ]
   if randBar >= 570 and randBar < 599 [ set turn 160 + random-float 10 ]
   if randBar >= 599 and randBar <= 628 [ set turn 170 + random-float 10 ]
 ]


 if BeeSpecies = "Bumblebees"
 [
   let randBar random 214 + 1 ; randomly chosen bar from the empirical turning angle histogram with a total of 214 turning angles (derived from harmonic radar dataset, Osborne et al 2013)

   if randBar >= 1 and randBar < 24 [ set turn 0 + random-float 10 ]
   if randBar >= 24 and randBar < 36 [ set turn 10 + random-float 10 ]
   if randBar >= 36 and randBar < 45 [ set turn 20 + random-float 10 ]
   if randBar >= 45 and randBar < 58 [ set turn 30 + random-float 10 ]
   if randBar >= 58 and randBar < 73 [ set turn 40 + random-float 10  ]
   if randBar >= 73 and randBar < 81 [ set turn 50 + random-float 10 ]
   if randBar >= 81 and randBar < 86 [ set turn 60 + random-float 10 ]
   if randBar >= 86 and randBar < 97 [ set turn 70 + random-float 10 ]
   if randBar >= 97 and randBar < 107 [ set turn 80 + random-float 10 ]
   if randBar >= 107 and randBar < 112 [ set turn 90 + random-float 10 ]
   if randBar >= 112 and randBar < 124 [ set turn 100 + random-float 10 ]
   if randBar >= 124 and randBar < 133 [ set turn 110 + random-float 10 ]
   if randBar >= 133 and randBar < 143 [ set turn 120 + random-float 10 ]
   if randBar >= 143 and randBar < 154 [ set turn 130 + random-float 10 ]
   if randBar >= 154 and randBar < 166 [ set turn 140 + random-float 10 ]
   if randBar >= 166 and randBar < 178 [ set turn 150 + random-float 10 ]
   if randBar >= 178 and randBar < 192 [ set turn 160 + random-float 10 ]
   if randBar >= 192 and randBar <= 214 [ set turn 170 + random-float 10 ]
 ]

 if random-float 1 > 0.5 [ set turn turn * -1 ]
 if RandomWalk = true [ set turn random-float 360 ] ; uncorrelated random walk
 set turn turn * LinearisationFactor
 if FixTurningAngle = true [ set turn FixRightTurn ]

 report turn
end

;; ================================================================================================================================================================================

to-report RelocationREP  ; defines new destination of scouts after a scouting round   ; RUN A PROCEDURE
  let result []
  if SearchMode = "colony"   ; as scouts are already at the colony and they immediately switch into search mode..
     [
       set result []    ;  .. hence destinationList remains empty!
     ]

  if SearchMode = "visited NLpatch (recruitment)" ; .. or at any location ever visited by at least one bee..
     [
       ifelse count patches with [ visits > 0 and patchColor != LakeColor ] > 0
         [
           ask one-of patches with [ visits > 0 and patchColor != LakeColor ]
            [ set result list pxcor pycor ]
         ]
         [ set result list Col_X Col_Y ]
     ]

  if SearchMode = "random location" ; .. or at any location within MaxForagingRange_m
     [
        ask one-of patches with [ patchColor != LakeColor and distancexy Col_X Col_Y * Scaling < MaxForagingRange_m and originalColor != BorderColor  ]
            [ set result list pxcor pycor ]

     ]

  if SearchMode = "known flowerpatch (individual)"  ; ..or at a flower patch, already known to that particular scout..
    [
       let memoKnownPatchesList []
       ifelse empty? knownPatchesList
         [ set result [] ]
         [
           set memoKnownPatchesList knownPatchesList
           let theChosenPatch one-of memoKnownPatchesList   ; bigger patches are more likely to be chosen
           ask one-of patches with [ flowerpatchid = theChosenPatch ]  ; (do NOT replace theChosenPatch by "one-of memoKnownPatchesList"!)
             [ set result list pxcor pycor ]
         ]
     ]

  if SearchMode = "known flowerpatch (recruitment)"   ; .. or at any one of all flower patches known to the whole colony..
    [
       let memoKnownPatchesList []
       ask patchstatistics
        [
          if totalDetections > 0  [ set memoKnownPatchesList fput who memoKnownPatchesList ]
        ]
       ifelse empty? memoKnownPatchesList
         [ set result [] ]
         [
            let theChosenPatch one-of memoKnownPatchesList              ; bigger patches are more likely to be chosen
            ask one-of patches with [ flowerpatchid = theChosenPatch ]  ; (do NOT replace theChosenPatch by "one-of memoKnownPatchesList"!)
              [ set result list pxcor pycor ]
         ]
     ]

  if SearchMode = "furthest location (individual)"  ; .. or at the furthest location this individual scout has ever been
     [ set result furthestLocationList ]

  if SearchMode = "last location (individual)"  ; .. or at the last location this individual scout has been before returning to the hive
     [ set result lastLocationList ]

  ifelse empty? result   ; new colour, depending if bees are in search mode or have a destination
    [
      set color white
      set flightPhase 2
    ]
    [
      set color green
      set flightPhase 1
    ]
  report result
end

;; ================================================================================================================================================================================

to MoveBeesProc  ; RUN A PROCEDURE
  let returningProb Gap_s / TripDuration_s ; if trip duration is randomly determined, this results in an average trip duration of TripDuration_s (only if ImmediateReturn = false)
  set TotalTrips 0
  ask bees
   [
     set time_s time_s + Gap_s               ; 3s between each recorded position in experimental radar data
     if SearchMode = "mixed strategy (individual)"
       [
         set MixedStratInd true
         set SearchMode one-of [ "colony" "known flowerpatch (individual)" "furthest location (individual)" "last location (individual)" ]
       ]

     if SearchMode = "mixed strategy (recruitment)"
       [
         set MixedStratCol true
         set SearchMode one-of [ "colony" "visited NLpatch (recruitment)" "known flowerpatch (recruitment)" "furthest location (individual)" "last location (individual)" ]
       ]

     if flightPhase = 2 [ set lastLocationList list xcor ycor ]     ; saves current location (only if bees are in search mode)

     ; if this is the END OF AN INDIVIDUAL'S SCOUTING ROUND:
     if (RandomTripDuration = false and remainder (ticks * Gap_s) (Gap_s * round (TripDuration_s / Gap_s)) = 0)   ; rounding: to make sure that trip duration can be divided by Gap_s
         or (RandomTripDuration = true and random-float 1 <= returningProb)
       [
         set destinationList list Col_x Col_y  ; then the new destination of a bee is the colony
         set color sky
         set flightPhase 3
       ]

     ; determine direction:
     ifelse destinationList = []             ; movement depends if or if not a bee has a destination
        [ TurnNoDestinationProc ]    ; CALL A PROCEDURE
        [ TurnDestinationProc ]     ; (remember: lakes do not affect bee movement, if it has a destination!)  ; CALL A PROCEDURE

     if (distancexy Col_x Col_y) * Scaling > MaxHiveDisplAllBees_m
        [ set MaxHiveDisplAllBees_m Scaling * distancexy Col_x Col_y ]  ; MaxHiveDisplAllBees_m: furthest distance of any individual from the colony ever

     if (distancexy Col_x Col_y) > DisplacementREP ; time_s    ; "visits": patches-own variable to show the distribution of bees in the landscape
        [ set visits visits + 1 ]              ; not recorded directly at the hive (i.e. within 1 Netlogo-Patch), as this would outshine visits elsewhere

     DetectionProc   ; to determine if a patch is detected   ; CALL A PROCEDURE

     if MixedStratInd = true
       [ set SearchMode "mixed strategy (individual)" ]
     if MixedStratCol = true
       [ set SearchMode "mixed strategy (recruitment)" ]

     ifelse nTrips <= MaxTrips
       [ set TotalTrips TotalTrips + nTrips ]
       [ set TotalTrips TotalTrips + (nTrips - 1) ] ; scouts are not allowed to perform more than MaxTrips trips!



    ]  ; end ask bees


 ; calculate DETECTION PROBABILITIES and save max. detect. prob. of a patch:
   ask patchStatistics
     [
       ifelse totalTrips > 0
         [
           set modelledDetectionProb_per_trip totalDetections / totalTrips
           if modelledDetectionProb_per_trip > 1 [ show "Warning! Detection Probability > 1!" ]
         ]
         [ set modelledDetectionProb_per_trip 0 ]

       set modelledDetectionProb_per_s totalDetections / (ticks * Gap_s * N_Bees)


       if modelledDetectionProb_per_s > maxDetectProb_per_s and ticks > TripDuration_s    ;  ticks > TripDuration_s: to avoid random peaks at beginning of simulation
          [
            set maxDetectProb_per_s modelledDetectionProb_per_s
            set timeMaxDetectProb_s ticks * Gap_s
          ]
     ]
   if remainder ticks 10 = 0 [ if any? patches with [ patchColor = -1 ] [ show "BUGALARM!" ask patches [ set pcolor pink ]]]

   ; actual MOVEMENT:
   ask bees with [ nTrips <= MaxTrips ] ;          when bees have finished the max. number of trips, they don't move anymore (i.e. they stay in the hive)
     [
       fd DisplacementREP ; time_s
       if (patchColor = LakeColor)                          ; LAKES & BORDERS:
         or (originalColor = BorderColor)                       ;       if a scout reaches a lake - or the borders of the world..
         or (pxcor = max-pxcor) or (pxcor = min-pxcor)
         or (pycor = max-pycor) or (pycor = min-pycor)
          [
            if flightPhase = 2 and destinationList = [] ; i.e. lakes only affect "white" bees in searching phase, not "orange" bees doing a loop
             [
               setxy item 0 lastLocationList item 1 lastLocationList   ;       .. it jumps back to its earlier position..
               set heading random-float 360                            ;       .. and chooses a new dircetion (only in searching phase!)
             ]
           ]
     ]

end


;; ================================================================================================================================================================================

to TurnDestinationProc  ; (CALLED BY INDIVIDUAL BEE) only called, if the bee has a destination   ; RUN A PROCEDURE

 facexy item 0 destinationList item 1 destinationList ; bee turns towards its destination
 if (distancexy Col_x Col_y < DisplacementREP) and (flightPhase = 2) ; if a (orange) bee loops around the colony: it retruns to ordinary search mode (white) once it reaches the colony
       [
         set destinationList []
         set color white
        ]
 if (distancexy Col_x Col_y < DisplacementREP) and (flightPhase = 3) ; if (blue) bee is back (i.e. within range of 1 step) at the colony, and flightPhase is 3 (i.e. blue, returning) (to avoid multiple relocation) their new destination is determined
       [
          let newXYlist RelocationREP
          if SearchMode = "last location (individual)" or MixedStratInd = true  or MixedStratCol = true
             [ set lastLocationList list xcor ycor ]
          set destinationList newXYlist
          set nowDetectedPatchesList [] ; a new scouting trip starts, hence list is empty
          if nTrips <= MaxTrips [ set nTrips nTrips + 1 ]
        ]

 if destinationList != []  ; only relevant, if bee has a destination (might not be the case if bees are about to leave the colony again, e.g. if SearchMode = "colony")
   [
     if (distancexy item 0 destinationList item 1 destinationList) < DisplacementREP and destinationList != list Col_x Col_y ; if this bee is at its field destination (within range of 1 step) (i.e. not at the colony)
       [
         set lastDestinationList destinationList
         set destinationList []               ; no more destination, i.e. bees switches into search behaviour
         set heading random-float 360
         set color white
         set flightPhase 2
       ]
   ]

end

;; ================================================================================================================================================================================


to TurnNoDestinationProc   ; (CALLED BY INDIVIDUAL BEE)   ; RUN A PROCEDURE
        rt TurningREP                              ; MOVEMENT if bee is in searching phase (i.e. without a destination):
        if random-float 1 <= TurnToDestinationProb
           [
            set destinationList lastDestinationList
            set color orange
           ]

        if SearchMode = "furthest location (individual)" or MixedStratInd = true or MixedStratCol = true
        [
          if (distancexy Col_X Col_Y) * Scaling > furthestDistance_m
          [
            set furthestLocationList list xcor ycor
            set furthestDistance_m (distancexy Col_X Col_Y) * Scaling
          ]
         ]

end

;; ================================================================================================================================================================================

to DetectionProc ; (CALLED BY INDIVIDUAL BEE) determines if a patch is detected   ; RUN A PROCEDURE
    ; possible DETECTION of a flower patch, if bee enters a red, blue, yellow or green NL-patch:

 if nTrips <= MaxTrips   ; patches can only dected, while MaxTrips is not exceeded!
  [
    if member? patchColor PatchColoursList  ; if a bee arrives at a patch

      [
        let patchNumber flowerPatchID
        if member? patchNumber nowDetectedPatchesList = false
        [
          set nowDetectedPatchesList fput patchNumber nowDetectedPatchesList
          ask PatchStatistic patchNumber
            [
              set totalDetections totalDetections + 1
              if firstDetection_s = 0
                [ set firstDetection_s ticks * Gap_s ]; time [s] when patch was detected for the first time
             ]
         ] ;; patch is added to the nowDetectedPatchesList if it wasn't detected earlier during this trip
        if member? patchNumber knownPatchesList = false
        [
          set knownPatchesList fput patchNumber knownPatchesList   ;; patch is added to the knownPatchesList if it wasn't detected ever
          if ImmediateReturn = true [
                                      set destinationList list Col_x Col_y
                                      set color sky
                                      set flightPhase 3
                                    ]
         ]
       ]
  ]
end

;; ================================================================================================================================================================================

to WriteFromListToFileProc   ;; called by: AnalyseProc   ; RUN A PROCEDURE
 let dayCounter 1
 let patchCounter 0
 file-print "day id oldPatchID patchType distance_m xcor ycor size_sqm quantityPollen_g concentration quantityNectar_l calculatedDetectionProb_per_trip modelledDetectionProb_per_trip nectarGathering_s pollenGathering_s "
 foreach sort patchStatistics
 [
   ask ?
   [
     if closestDistance_m <= MaxForagingRange_m
     [
       repeat 365
       [
           file-type dayCounter file-type " "                                                                ;;   1.) day
           file-type patchCounter file-type " "                                                              ;;   2.) ID (= who, if MaxForagingRange > closestDistance_m of furthest patch)
           file-type who file-type " "                                                                       ;;   3.) who = "old ID"
           file-type patchType file-type " "                                                                 ;;   4.) patchType
           file-type closestDistance_m file-type " "                                                         ;;   5.) closestDistanceM
           file-type round ((xcor - Col_X) * (ScaleDistance_m / (Scale_X2 - Scale_X1)))   file-type " "       ;;   6.) xcor relative to hive
           file-type round ((ycor - Col_Y) * (ScaleDistance_m / (Scale_X2 - Scale_X1)))  file-type " "        ;;   7.) ycor relative to hive
           file-type areaSqm file-type " "                                                                   ;;   8.) areaSqm
           ifelse dayCounter >= startDay and dayCounter <= stopDay                                           ;;   9.) quantityPollen_g
             [ file-type quantityPollen_g file-type " " ]
             [ file-type 0 file-type " " ]                                                                   ;;        ( no pollen, if patch is not in flower)
           file-type concentration file-type " "                                                             ;;   10.) concentration
           ifelse dayCounter >= startDay and dayCounter <= stopDay                                           ;;   11.) quantityNectar_l
             [ file-type quantityNectar_l file-type " " ]
             [ file-type 0 file-type " " ]                                                                   ;;        ( no nectar, if patch is not in flower)
           file-type calculatedDetectionProb_per_trip file-type " "                                          ;;   12.) calculatedDetectionProb_per_trip
           file-type modelledDetectionProb_per_trip file-type " "                                            ;;   13.) modelledDetectionProb_per_trip
           file-type nectarGathering_s file-type " "                                                         ;;   14.) nectarGathering_s
           file-type pollenGathering_s file-type " "                                                         ;;   15.) pollenGathering_s "
           file-print (" ")
           set dayCounter dayCounter + 1
           if dayCounter = 366 [ set dayCounter 1 ]
        ]
        set patchCounter patchCounter + 1
     ]
   ]
 ]
end

;; ================================================================================================================================================================================


to DoDetectionPlotsProc   ; RUN A PROCEDURE
 let detectProb1 [ modelledDetectionProb_per_trip ] of PatchStatistic Plot1
 let detectProb2 [ modelledDetectionProb_per_trip ] of PatchStatistic Plot2
 let detectProb3 [ modelledDetectionProb_per_trip ] of PatchStatistic Plot3
 let detectProb4 [ modelledDetectionProb_per_trip ] of PatchStatistic Plot4

  set-current-plot "Detection Probability per trip"
    set-current-plot-pen "plot1"
    plot (detectProb1)
    set-current-plot-pen "plot2"
    plot (detectProb2)
    set-current-plot-pen "plot3"
    plot (detectProb3)
    set-current-plot-pen "plot4"
    plot (detectProb4)
end


;; ================================================================================================================================================================================


to ReadForagingDataProc   ; RUN A PROCEDURE
   set ForagingFileList []
   file-open InputForagingFile
   let nPatchesInFile file-read
   let dustbin file-read-line  ;; to skip the headings of the txt file
   while [ not file-at-end? ] [ set ForagingFileList sentence ForagingFileList (list (list file-read file-read file-read file-read))]
   set ForagingFileList fput nPatchesInFile ForagingFileList
   file-close
   set NewTimestepForaging 1
end

;; ================================================================================================================================================================================


to ShowForagingDataProc   ; RUN A PROCEDURE
  let ForagingFileListToday []
  let nPatchesInFile item 0 ForagingFileList   ; first item (i.e. number 0) in list: # of patches, second item: first day, first patch data
  let itemNumber 1 + nPatchesInFile * (NewTimestepForaging - 1)   ; this is the first item with foraging data for today
  let itemCounter 0
  let columns length item 1 ForagingFileList ; number of data columns in the foraging input file

  repeat nPatchesInFile
   [
     set ForagingFileListToday sentence ForagingFileListToday (item (itemNumber + itemCounter) ForagingFileList)
     set itemCounter itemCounter + 1
   ]

  if item 0 ForagingFileListToday != NewTimestepForaging
    [ user-message "Error in ShowForagingDataProc - wrong day!" ]

  let currentItem 1  ; i.e. the first "who" in the list (item 0 is the day)
  let nextWho item currentItem ForagingFileListToday
  let maxItems length ForagingFileListToday

  foreach sort patchStatistics
  [
   ask ?
   [
     ifelse who = nextWho and currentItem + columns - 1 <= maxItems  ; if who is on the list and its not the end of the list yet, read data from list

     [
       set nNectarVisits item (currentItem + 1) ForagingFileListToday
       set nPollenVisits item (currentItem + 2) ForagingFileListToday
       set currentItem currentItem + columns  ; the position of the next "who" in the list
       if currentItem <= maxItems [ set nextWho item currentItem ForagingFileListToday ]

     ]
     [    ; ELSE: if patch is not on the list, it can't have had any visitors!
       set nNectarVisits 0
       set nPollenVisits 0
     ]

   ]
  ]

  ask PatchStatistics
  [
    if ForagingMap = "Nectar" [ set visitColor scale-color yellow nNectarVisits 0 MaxVisitsColour ]
    if ForagingMap = "Pollen" [ set visitColor scale-color orange nPollenVisits 0 MaxVisitsColour ]
    if ForagingMap = "All visits" [ set visitColor scale-color red (nNectarVisits + nPollenVisits) 0 MaxVisitsColour ]
    let memoWho who
    ask patches with [ flowerPatchID = memoWho ] [ set pcolor [ visitColor ] of patchStatistic memoWho ]
    if nNectarVisits + nPollenVisits > 0 and closestDistance_m > MaxForagingRange_m
      [ show "Warning! Foraging outside the max. foraging range!" ]
  ]



end

;; ================================================================================================================================================================================


to-report DateREP
  let month-names (list "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
  let days-in-months (list 31 28 31 30 31 30 31 31 30 31 30 31)


  let year floor (NewTimeStepForaging / 365.01) + 1
  let month 0
  let dayOfYear remainder NewTimeStepForaging 365
  if dayOfYear = 0 [ set dayOfYear 365 ]
  let dayOfMonth 0
  let sumDaysInMonths 0
  while [ sumDaysInMonths < dayOfYear ]
  [
    set month month + 1
    set sumDaysInMonths sumDaysInMonths + item (month - 1) days-in-months
    set dayOfMonth dayOfYear - sumDaysInMonths + item (month - 1) days-in-months
  ]

  report (word dayOfMonth "  " (item (month - 1) month-names) ) ; " " year )

end

;; =================================================================================================================================================================================================================

;---------------   BUTTONS   ---------------   BUTTONS   ---------------   BUTTONS   ---------------   BUTTONS   ---------------   BUTTONS   ---------------   BUTTONS   ---------------   BUTTONS   ---------------

;; =================================================================================================================================================================================================================


to Default_ButtonProc   ; RUN A PROCEDURE

;; NOT set by Default button:
  ;set  RandSeed  1
  ;set  Plot1  1
  ;set  Plot2  2
  ;set  Plot3  3
  ;set  Plot4  4

;; SET by Default button:
  set  BeeSpecies "Honeybees"
  set  Black_th  1
  set  Blue_max  110
  set  Blue_min  90
  set  BluePatches  TRUE
  set  BrushSize  3
  set  ByColour  0   ; 0: black
  set  Col_X   max-pxcor / 2 ; 160
  set  Col_Y  max-pycor / 2 ; 106
  set  Conc_B  1.5
  set  Conc_G  1.5
  set  Conc_R  1.5
  set  Conc_Y  1.5
  set  Day_x  1
  set  DisplacementFactor  1
  set  FixRightTurn  0.2
  set  FixTurningAngle  FALSE
  set  ForagingMap  "Nectar"
  set  Green_max  70
  set  Green_min  50
  set  GreenPatches TRUE ; false
  set  Gridsize 1000
  set  Highlight_Patch  0
  set  ImmediateReturn  FALSE
  set  InputFile "No input file"
  set  InputForagingFile  "Input_1-2_Foraging.txt"
  set  Lakes  FALSE
  set  LinearisationFactor  1
  set  MaxForagingRange_m  10000
  set  MaxPatchRadius_m  500
  set  MaxTrips 999999
  set  MaxVisitsColour  1000
  set  N_Bees  10000
  set  NameOutfile  "Input_2-1_FoodFlow.txt"
  set  Nectar_B  1
  set  Nectar_G  1
  set  Nectar_R  1
  set  Nectar_Y  1
  set  Patchtype_B  "\"BlueField\""
  set  Patchtype_G  "\"GreenField\""
  set  Patchtype_R  "\"RedField\""
  set  Patchtype_Y  "\"YellowField\""
  set  Pollen_B  1
  set  Pollen_G  1
  set  Pollen_R  1
  set  Pollen_Y  1
  set  RandomTripDuration  TRUE
  set  RandomWalk  FALSE
  set  Red_max  30
  set  Red_min  10
  set  RedPatches  TRUE
  set  ReplaceColour  85   ; 85: cyan ("lake" colour)
  set  SatelliteFile  "No satellite image"
  set  Scale_X1  1
  set  Scale_X2  max-pxcor
  set  ScaleDistance_m  3000 ; average foraging distance (i.e. radius) ca. 1500m; focus on efficient patches
  set  ScoutingPeriod_hrs  9
  set  SearchMode  "known flowerpatch (recruitment)"
  set  SetColour  "Yellow"
  set  SetDirection_deg  90
  set  SetDistanceToCentre_m  250
  set  SetRadius_m  50
  set  Start_B  271
  set  Start_G  181
  set  Start_R  1
  set  Start_Y  91
  set  Stop_B  360
  set  Stop_G  270
  set  Stop_R  90
  set  Stop_Y  180
  set  t_Nectar_B  1200
  set  t_Nectar_G  1200
  set  t_Nectar_R  1200
  set  t_Nectar_Y  1200
  set  t_Pollen_B  600
  set  t_Pollen_G  600
  set  t_Pollen_R  600
  set  t_Pollen_Y  600
  set  TripDuration_s  1020
  set  TurnToDestinationProb 0.02
  set  White_th  9
  set  Yellow_max  50
  set  Yellow_min  40
  set  YellowPatches  TRUE
end

;; ================================================================================================================================================================================

to ClearBeesProc
 if RandSeed > 0 [ random-seed RandSeed ]
 CreateBeesProc
 set MaxHiveDisplAllBees_m 0
 set TotalTrips 0
 set SCALING ScaleDistance_m / (Scale_X2 - Scale_X1)
 clear-drawing
 clear-all-plots
 clear-ticks
 RESET-TICKS
end

;; ================================================================================================================================================================================

to BumblebeesProc
 set BeeSpecies "Bumblebees"
 set SearchMode "known flowerpatch (individual)"
 set N_Bees 30
 set TripDuration_s 600
 set ScoutingPeriod_hrs 9
 set LinearisationFactor 1
 set DisplacementFactor 1
 set RandomWalk false
 set RandomTripDuration true
 set ImmediateReturn false
 set MaxTrips 999999
 set TurnToDestinationProb 0.02
end

;; ================================================================================================================================================================================

to HoneybeesProc
 set BeeSpecies "Honeybees"
 set SearchMode "known flowerpatch (recruitment)"
 set N_Bees 10000
 set TripDuration_s 1020 ; as in BEEHAVE model
 set ScoutingPeriod_hrs 9
 set LinearisationFactor 1
 set DisplacementFactor 1
 set RandomWalk false
 set RandomTripDuration true
 set ImmediateReturn false
 set MaxTrips 999999
 set TurnToDestinationProb 0.02
end



;; ===================================================================================================================================================================================================
;; ---------------      END      ---------------      END      ---------------      END      ---------------      END      ---------------      END      ---------------      END      ---------------
;; ===================================================================================================================================================================================================
@#$#@#$#@
GRAPHICS-WINDOW
174
10
1087
674
-1
-1
3.0
1
14
1
1
1
0
0
0
1
0
300
0
210
0
0
1
ticks
30.0

BUTTON
9
48
172
110
Setup
Setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
9
112
171
157
InputFile
InputFile
"No input file" "S6_RealLandscape.jpg" "S6_Map_Osborne.jpg" "S6_ConnectedPatch.png" "S6_UnconnectedPatch.png" "MapImageUpdate.png" "MapImage.png"
6

INPUTBOX
1096
210
1177
270
Col_X
150
1
0
Number

INPUTBOX
1178
210
1257
270
Col_Y
105
1
0
Number

INPUTBOX
1095
151
1176
211
Scale_X1
1
1
0
Number

INPUTBOX
1177
151
1256
211
Scale_X2
300
1
0
Number

MONITOR
1096
46
1256
91
Scaling [m/nlPatches]
SCALING
2
1
11

INPUTBOX
179
1067
333
1127
Patchtype_R
\"RedField\"
1
0
String

INPUTBOX
668
1067
821
1127
Patchtype_B
\"BlueField\"
1
0
String

INPUTBOX
342
1067
497
1127
Patchtype_Y
\"YellowField\"
1
0
String

BUTTON
1095
402
1181
435
Patches
  ;ask patches with [(patchColor = red) or (patchColor = yellow) or (patchColor = blue)] [ set pcolor patchColor ]\nask patches with [  member? patchColor PatchColoursList = true] \n  [ \n    ifelse pcolor != patchColor\n     [ set pcolor patchColor ] \n     [ set pcolor grey ]\n   ]\n  \nif Lakes = true \n[ \n  ask patches with [ patchColor = LakeColor ] \n   [\n     ifelse pcolor != LakeColor \n      [ set pcolor LakeColor ] \n      [ set pcolor grey ]\n   ]\n]\n;set satelliteView false\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1095
573
1181
606
Borders
ask patches with [originalColor = BorderColor]\n  [ \n    ifelse pcolor != BorderColor\n     [ set pcolor BorderColor ] \n     [ set pcolor grey ]\n   ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
475
167
546
Go
\nGo\n\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1280
435
1393
495
N_Bees
1000
1
0
Number

BUTTON
5
630
167
663
Clear Bees
ClearBeesProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1095
331
1180
364
Reset
ask patchStatistics \n[ \n  show-turtle \n  set label who \n]\nask bees [ show-turtle ]\nask hives [ show-turtle ]\n\n   \nask patches [\n  set plabel \"\"\n  set pcolor gray\n   ]   \nask patches with [  member? patchColor PatchColoursList = true] [ set pcolor patchColor ]\nif Lakes = true [ ask patches with [ patchColor = LakeColor ] [ set pcolor LakeColor ] ]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
595
167
628
Step
Go
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

INPUTBOX
1095
91
1256
151
ScaleDistance_m
3000
1
0
Number

PLOT
566
678
967
865
detection probability per trip
NIL
NIL
0.0
10.0
0.0
1.0E-5
true
true
"" ""
PENS
"plot1" 1.0 0 -16777216 true "" ""
"plot2" 1.0 0 -6459832 true "" ""
"plot3" 1.0 0 -10899396 true "" ""
"plot4" 1.0 0 -8630108 true "" ""

INPUTBOX
761
866
811
926
Plot1
0
1
0
Number

INPUTBOX
813
866
863
926
Plot2
0
1
0
Number

INPUTBOX
865
866
915
926
Plot3
0
1
0
Number

INPUTBOX
917
866
967
926
Plot4
0
1
0
Number

BUTTON
1095
642
1181
675
Pen
let penMemo true\nask one-of bees [ set penMemo pen-mode ] \nifelse penMemo = \"up\" \n  [ ask bees [pen-down]]\n  [ ask bees [pen-up]]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1183
642
1269
675
Erase
clear-drawing
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1183
573
1269
606
Satellite
\n;import-pcolors SatelliteFile\nask patches [ set pcolor satelliteColor ]\n;set satelliteView true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
7
830
171
863
Read foraging data
\nifelse file-exists? InputForagingFile \n[ \n  ReadForagingDataProc\n  ShowForagingDataProc\n  set NewTimestepForaging 0 ;Day\n ]\n[ user-message \"Foraging input file not found!\" ]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
232
759
344
804
ForagingMap
ForagingMap
"Nectar" "Pollen" "All visits"
0

BUTTON
399
725
454
758
+1d
set NewTimeStepForaging NewTimeStepForaging + 1\nif NewTimeStepForaging > 365 [ set NewTimeStepForaging 1 ]\nShowForagingDataProc\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
438
759
508
804
time step
NewTimeStepForaging
17
1
11

BUTTON
1095
538
1181
571
Bees
 ask bees [ ifelse hidden? = false \n                [ hide-turtle]\n                [ show-turtle ] ]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
345
725
400
758
-1d
set NewTimeStepForaging NewTimeStepForaging - 1\nif NewTimeStepForaging < 1\n  [ set NewTimeStepForaging 1 ]\nShowForagingDataProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
509
725
564
758
+30d
set NewTimeStepForaging NewTimeStepForaging + 30\nif NewTimeStepForaging > 365 [ set NewTimeStepForaging 1 ]\nShowForagingDataProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
233
725
288
758
-30d
set NewTimeStepForaging NewTimeStepForaging - 30\nif NewTimeStepForaging < 1\n  [ set NewTimeStepForaging 1 ]\nShowForagingDataProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
177
725
232
758
Day 1
ifelse file-exists? InputForagingFile\n   [ set NewTimeStepForaging 1\n     ShowForagingDataProc ]\n     [ user-message \"No 'Input_1-3_Foraging' file found!\" ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
176
759
231
804
Refresh
ifelse file-exists? InputForagingFile \n   [ ShowForagingDataProc ]\n     [ user-message \"No 'Input_1-3_Foraging' file found!\" ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
345
805
507
865
MaxVisitsColour
1000
1
0
Number

BUTTON
1396
591
1506
652
Highlight patch
ask patches with [ flowerPatchID = Highlight_Patch ] \n  [ ifelse pcolor != 95\n      [ set pcolor 95 ]\n      [ set pcolor white ]\n      \n      ]\n      \nask turtle Highlight_Patch\n[\n show-turtle\n set color color + 3\n repeat 50 [ set size size + 0.5 wait 0.02  ] \n wait 1.5\n repeat 50 [ set size size - 0.5 wait 0.02  ]\n set color color - 3\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1282
592
1398
652
Highlight_Patch
0
1
0
Number

BUTTON
453
725
508
758
+7d
set NewTimeStepForaging NewTimeStepForaging + 7\nif NewTimeStepForaging > 365 [ set NewTimeStepForaging 1 ]\nShowForagingDataProc\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
289
725
344
758
-7d
set NewTimeStepForaging NewTimeStepForaging - 7\nif NewTimeStepForaging < 1\n  [ set NewTimeStepForaging 1 ]\nShowForagingDataProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
7
783
171
828
InputForagingFile
InputForagingFile
"Input_1-2_Foraging.txt"
0

BUTTON
1183
401
1269
434
Labels
ask patchstatistics\n [ \n ifelse label = \"\"\n     [ \n       set label who \n       show-turtle\n     ]\n     [ set label \"\" ]\n ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
828
1070
982
1130
MaxPatchRadius_m
500
1
0
Number

BUTTON
1095
607
1181
640
Map
ask patches [ set pcolor patchColor ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1683
149
1760
182
Import world
import-world \"3_BEEHAVE_Landscape2014_world.csv\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1767
149
1845
182
Export world
export-world \"3_BEEHAVE_Landscape2014_world.csv\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1170
833
1259
878
minutes passed
floor ((3 * ticks) / 60)
0
1
11

MONITOR
1353
834
1504
879
furthest distance [m]
MaxHiveDisplAllBees_m
1
1
11

CHOOSER
9
160
171
205
SatelliteFile
SatelliteFile
"No satellite image"
0

CHOOSER
1280
387
1504
432
SearchMode
SearchMode
"colony" "known flowerpatch (individual)" "known flowerpatch (recruitment)" "random location" "visited NLpatch (recruitment)" "furthest location (individual)" "last location (individual)" "mixed strategy (individual)" "mixed strategy (recruitment)"
2

INPUTBOX
1280
496
1393
556
TripDuration_s
1020
1
0
Number

INPUTBOX
1277
108
1466
168
FixRightTurn
0.2
1
0
Number

SWITCH
1277
74
1466
107
FixTurningAngle
FixTurningAngle
1
1
-1000

INPUTBOX
1393
435
1505
495
RandSeed
0
1
0
Number

SLIDER
1277
38
1466
71
LinearisationFactor
LinearisationFactor
0
1
1
0.05
1
NIL
HORIZONTAL

SLIDER
1467
38
1657
71
DisplacementFactor
DisplacementFactor
0
10
1
0.1
1
NIL
HORIZONTAL

BUTTON
1095
436
1181
469
Patchstats
ask Patchstatistics\n [ \n        ifelse hidden? = false \n                [ hide-turtle]\n                [ \n                  ifelse TotalDetections > 0\n                    [ show-turtle ]\n                    [ hide-turtle ]\n                  if ticks = 0 [ show-turtle ] \n                ] \n   ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1467
74
1657
107
RandomWalk
RandomWalk
1
1
-1000

BUTTON
8
207
170
254
Default (Honeybees)
Default_ButtonProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
828
973
982
1006
Lakes
Lakes
1
1
-1000

BUTTON
1183
504
1269
537
Distribution
let maxVisits max [ visits ] of patches\nif maxVisits > 0\n[ \n  ask patches \n    ;[ set pcolor scale-color sky Visits 0 (maxVisits / 20) ]\n    [ set pcolor scale-color sky sqrt Visits 0 sqrt (maxVisits / 10) ]\n    \n    \n ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1260
833
1350
878
seconds passed
[time_s] of one-of bees
17
1
11

MONITOR
970
833
1082
878
detected patches
count patchstatistics with [ totalDetections > 0]
17
1
11

BUTTON
1183
435
1269
468
Detected
ask patches [ set pcolor black ]\nask patches with [ member? patchColor PatchColoursList = true]\n;ask patches with [ patchColor = red or patchColor = blue or patchColor = yellow ]\n [\n   ifelse [ TotalDetections ] of patchstatistic flowerpatchID > 0\n     [ set pcolor patchColor ]\n     [ set pcolor grey ]\n ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1183
608
1269
641
Original color
ask patches [ set pcolor originalColor ]\n\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
179
1006
256
1066
Red_min
10
1
0
Number

INPUTBOX
257
1006
334
1066
Red_max
30
1
0
Number

INPUTBOX
343
1006
420
1066
Yellow_min
40
1
0
Number

INPUTBOX
420
1006
497
1066
Yellow_max
50
1
0
Number

INPUTBOX
507
1005
584
1065
Green_min
50
1
0
Number

INPUTBOX
584
1005
659
1065
Green_max
70
1
0
Number

INPUTBOX
668
1006
745
1066
Blue_min
90
1
0
Number

INPUTBOX
745
1006
822
1066
Blue_max
110
1
0
Number

INPUTBOX
828
1008
905
1068
Black_th
1
1
0
Number

INPUTBOX
905
1008
982
1068
White_th
9
1
0
Number

INPUTBOX
1393
496
1504
556
ScoutingPeriod_hrs
9
1
0
Number

SWITCH
1468
108
1657
141
RandomTripDuration
RandomTripDuration
0
1
-1000

SWITCH
1468
142
1657
175
ImmediateReturn
ImmediateReturn
1
1
-1000

PLOT
970
679
1350
831
covered area [km^2]
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"area" 1.0 0 -16777216 true "" "plot count patches with [ visits > 0 ] * (SCALING / 1000) ^ 2"

BUTTON
1095
504
1181
537
History
\nlet maxTime max [ firstDetection_s ] of patchStatistics\nif maxTime > 0\n[\n  ask patches with [flowerPatchID != -1] \n      [ set mapDisplay [firstDetection_s] of patchstatistic flowerPatchID ] \n  ask patches with [ flowerPatchID != -1 and mapDisplay > 0 ] \n    [ set pcolor scale-color lime sqrt mapDisplay 0 sqrt maxTime ]\n    \n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1095
470
1181
503
model detProb
ask patches [ set pcolor gray ]\nlet maxPdetect max [ modelledDetectionProb_per_trip ] of patchStatistics\nif maxPdetect > 0\n[\n  ask patches with [flowerPatchID != -1] \n      [ set mapDisplay [modelledDetectionProb_per_trip] of patchstatistic flowerPatchID ] \n ; ask patches with [flowerPatchID != -1] [ set pcolor scale-color orange sqrt mapDisplay 0 sqrt (maxPdetect / 20)]\n  ask patches with [flowerPatchID != -1] [ set pcolor scale-color orange sqrt mapDisplay 0 0.2 ]  \n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
180
971
334
1004
RedPatches
RedPatches
0
1
-1000

SWITCH
507
971
659
1004
GreenPatches
GreenPatches
0
1
-1000

SWITCH
668
972
822
1005
BluePatches
BluePatches
0
1
-1000

SWITCH
344
971
497
1004
YellowPatches
YellowPatches
0
1
-1000

INPUTBOX
507
1066
659
1126
Patchtype_G
\"GreenField\"
1
0
String

BUTTON
1683
38
1844
83
Analyse & Outfile
AnalyseOutfileProc\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
33
1033
107
1066
Info colours
let yshift -7\nifelse ([ pcolor ] of patch 260 200 != 139)\n   and ([ pcolor ] of patch 35 80 != 51)\n[\nask patchStatistics [ hide-turtle ]\nask bees [ hide-turtle ]\nask hives [ hide-turtle ]\nask patches \n[\n set plabel \"\"\n set pcolor black\n set plabel-color black \n let x round ((pxcor / max-pxcor) * 10)\n let y round (((pycor + yshift) / max-pycor) * 14)\n set pcolor 10 * y + x\n if remainder pxcor (max-pxcor / 10) = 0\n    and remainder (pycor + yshift) (max-pycor / 14) = 0  \n    [ set plabel pcolor ] \n]\n\nuser-message \"NetLogo represents colors as a number in the range 0 to 140, with the exception of 140 itself. Black is represented by 0, 10 .. 130, white by 9.9, 19.9 ..139.9 (http://ccl.northwestern.edu/netlogo/faq.html)\"\n]\n\n; ELSE:\n[\nask patchStatistics [ show-turtle ]\nask bees [ show-turtle ]\nask hives [ show-turtle ]\nask patches [ \n              set pcolor black \n              set plabel \"\"\n             ]\nask patches with [  member? patchColor PatchColoursList = true] [ set pcolor patchColor ]\nif Lakes = true [ ask patches with [ patchColor = LakeColor ] [ set pcolor LakeColor ] ]\n\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
179
1129
256
1189
Start_R
1
1
0
Number

INPUTBOX
343
1129
419
1189
Start_Y
91
1
0
Number

INPUTBOX
507
1126
584
1186
Start_G
181
1
0
Number

INPUTBOX
668
1127
745
1187
Start_B
271
1
0
Number

INPUTBOX
257
1129
334
1189
Stop_R
90
1
0
Number

INPUTBOX
420
1129
497
1189
Stop_Y
180
1
0
Number

INPUTBOX
584
1126
659
1186
Stop_G
270
1
0
Number

INPUTBOX
746
1127
822
1187
Stop_B
360
1
0
Number

INPUTBOX
179
1190
255
1250
Nectar_R
1
1
0
Number

INPUTBOX
343
1191
420
1251
Nectar_Y
1
1
0
Number

INPUTBOX
507
1189
585
1249
Nectar_G
1
1
0
Number

INPUTBOX
669
1189
746
1249
Nectar_B
1
1
0
Number

INPUTBOX
256
1190
334
1250
Pollen_R
1
1
0
Number

INPUTBOX
421
1191
497
1251
Pollen_Y
1
1
0
Number

INPUTBOX
587
1189
660
1249
Pollen_G
1
1
0
Number

INPUTBOX
747
1189
823
1249
Pollen_B
1
1
0
Number

TEXTBOX
1521
309
1814
338
Artificial landscape creator:
19
0.0
1

INPUTBOX
179
1252
254
1312
t_Nectar_R
1200
1
0
Number

INPUTBOX
343
1252
420
1312
t_Nectar_Y
1200
1
0
Number

INPUTBOX
507
1251
585
1311
t_Nectar_G
1200
1
0
Number

INPUTBOX
668
1250
746
1310
t_Nectar_B
1200
1
0
Number

INPUTBOX
255
1252
334
1312
t_Pollen_R
600
1
0
Number

INPUTBOX
421
1252
498
1312
t_Pollen_Y
600
1
0
Number

INPUTBOX
586
1251
660
1311
t_Pollen_G
600
1
0
Number

INPUTBOX
747
1250
823
1310
t_Pollen_B
600
1
0
Number

BUTTON
507
1379
661
1412
Maize
set Patchtype_G \"\\\"Maize\\\"\"\nset Start_G 197\nset Stop_G 210\nset Nectar_G 0\nset Pollen_G 0.752\nset t_Nectar_G 0\nset t_Pollen_G 600 ; default BEEHAVE (no data available)\nset Conc_G 0\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
345
1379
423
1412
Oilseed rape
set Patchtype_Y \"\\\"Oilseed rape\\\"\"\nset Start_Y 114 \nset Stop_Y 136\nset Nectar_Y 0.3\nset Pollen_Y 0.13\nset t_Nectar_Y 320\nset t_Pollen_Y 221\nset Conc_Y 1.5\n\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
179
1378
333
1411
Field beans
set Patchtype_R \"\\\"Field beans\\\"\"\nset Start_R 153\nset Stop_R 182\nset Nectar_R 0.092 \nset Pollen_R 0.0642\nset t_Nectar_R 692\nset t_Pollen_R 298\nset Conc_R 1.28\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
668
1379
825
1412
White clover
set Patchtype_B \"\\\"White clover\\\"\"\nset Start_B 140\nset Stop_B 242\nset Nectar_B 0.049\nset Pollen_B 0.0094\nset t_Nectar_B 1630\nset t_Pollen_B 2574\nset Conc_B 1.49\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
1625
340
1762
385
SetColour
SetColour
"Red" "Yellow" "Green" "Blue" "Black" "Grey"
1

INPUTBOX
1625
386
1762
446
SetDistanceToCentre_m
250
1
0
Number

INPUTBOX
1626
482
1763
542
SetRadius_m
50
1
0
Number

BUTTON
1520
482
1625
587
Update map
set InputFile \"MapImageUpdate.png\"\nask PatchStatistics [ hide-turtle ]  ; hives, bees and patchstatistics have to be hidden, otherwise they become part of the landscape image\nask hives [ hide-turtle ]\nask bees [ hide-turtle ]\nask patches [ set plabel \"\" ]\nexport-view \"MapImageUpdate.png\" \nSetup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
510
675
565
708
Grid
 ask patches with [ remainder pxcor round (Gridsize / SCALING) = 0 ] [ set pcolor 124 ]\n ask patches with [ remainder pycor round (Gridsize / SCALING) = 0 ] [ set pcolor 124 ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1520
341
1624
400
Draw patches
let clickedXcor mouse-xcor\nlet clickedYcor mouse-ycor\n\nif mouse-down?\n  [ \n    ifelse BrushSize = 0\n      [ ask patch clickedXcor clickedYcor [ set pcolor read-from-string SetColour ] ]\n      [ \n        ask patches with [ (pxcor >= clickedXcor - (BrushSize / 2) and pxcor <= clickedXcor + (BrushSize / 2))\n                            and (pycor >= clickedYcor - (BrushSize / 2) and pycor <= clickedYcor + (BrushSize / 2)) ]\n                           [ set pcolor read-from-string SetColour ] \n       ]\n  ]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1520
401
1624
434
BrushSize
BrushSize
0
60
3
1
1
NIL
HORIZONTAL

BUTTON
1520
435
1624
481
Export map
ask PatchStatistics [ hide-turtle ]\nask hives [ hide-turtle ]\nask bees [ hide-turtle ]\nexport-view \"MapImage.png\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1765
340
1852
384
Clear all
ask turtles [ hide-turtle ]\nask patches [ set pcolor grey ]\nask hives [ show-turtle ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1626
448
1762
481
SetDirection_deg
SetDirection_deg
0
360
90
5
1
°
HORIZONTAL

INPUTBOX
179
1314
254
1374
Conc_R
1.5
1
0
Number

INPUTBOX
344
1314
420
1374
Conc_Y
1.5
1
0
Number

INPUTBOX
507
1313
585
1373
Conc_G
1.5
1
0
Number

INPUTBOX
668
1312
746
1372
Conc_B
1.5
1
0
Number

INPUTBOX
1684
187
1847
247
MaxForagingRange_m
10000
1
0
Number

INPUTBOX
1683
87
1844
147
NameOutfile
Input_2-1_FoodFlow.txt
1
0
String

MONITOR
1627
542
1763
587
Area of patch [km^2]
(Pi * (SetRadius_m ^ 2)) / 1000000
4
1
11

BUTTON
1684
247
1848
280
Show foraging range
;ask patches [ set pcolor gray ]\nask patches with [ member? patchColor PatchColoursList = true] [ set pcolor patchColor ]\nif Lakes = true [ ask patches with [ patchColor = LakeColor ] [ set pcolor LakeColor ] ]\nask patches with [ distancexy COL_X COL_Y * Scaling > MaxForagingRange_m ] \n[ \n  let newColour (10 * (floor (pcolor / 10))) + 2\n  if newColour < 1 [ set newColour 1 ] \n  if newColour > 139 [ set newColour 139 ] \n  set pcolor newColour\n]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
175
805
344
864
Slide Show
wait 0.25\nShowForagingDataProc\nset NewTimeStepForaging NewTimeStepForaging + 1\nif NewTimeStepForaging > 365 [ set NewTimeStepForaging 1 ]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1520
591
1625
651
Replace colour
ask patches with [ pcolor = ReplaceColour ]\n  [ set pcolor ByColour ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1626
591
1763
651
ReplaceColour
85
1
0
Color

INPUTBOX
1765
591
1853
651
ByColour
0
1
0
Color

TEXTBOX
182
931
473
952
Definition food patches:
19
0.0
1

TEXTBOX
1099
10
1240
35
Scaling & hive:
19
0.0
1

BUTTON
1183
366
1269
400
Color G/B/W
\n;if [ pcolor ] of patch 50 50 != black\n; or [ pcolor ] of patch 50 50 != white\n; or [ pcolor ] of patch 50 50 != grey\n; [ set BackgroundColor black] \n\n\nif [ pcolor ] of patch 50 50 = white\n   [ set BackgroundColor grey ]\nif [ pcolor ] of patch 50 50 = black\n   [ set BackgroundColor white ] \nif [ pcolor ] of patch 50 50 = grey\n   [ set BackgroundColor black ] \n \n   \nask patches [\n  set plabel \"\"\n  set pcolor BackgroundColor\n   ]   \n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1095
367
1180
400
Grey
ask patches [\n  set plabel \"\"\n  set pcolor gray\n   ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
21
14
140
37
Setup map:
19
0.0
1

TEXTBOX
11
445
144
481
Explore map:
19
0.0
1

TEXTBOX
1282
312
1432
335
Search options:
19
0.0
1

TEXTBOX
1100
306
1191
329
Display:
19
0.0
1

TEXTBOX
14
724
160
793
Show BEEHAVE\nforaging data:
19
0.0
1

TEXTBOX
1667
10
1871
39
Create BEEHAVE outfile:
19
0.0
1

BUTTON
1183
538
1269
571
Hive
 ask hives [ ifelse hidden? = false \n                [ hide-turtle]\n                [ show-turtle ] ]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1183
331
1268
364
Hide all
ask turtles [ hide-turtle]\nask patches [\n  set plabel \"\"\n  set pcolor gray\n   ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
509
805
564
865
Day_x
1
1
0
Number

BUTTON
509
759
564
804
Goto x
set Day_x round Day_x\nif Day_x < 1 [ set Day_x 1 ]\nwhile [ Day_x > 365 ]\n  [ set Day_x Day_x - 365 ]\nset NewTimeStepForaging Day_x\nShowForagingDataProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1096
270
1258
303
Update scaling & hive
Setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1278
176
1467
236
TurnToDestinationProb
0.02
1
0
Number

INPUTBOX
1468
176
1657
236
MaxTrips
999999
1
0
Number

MONITOR
1505
834
1619
879
min N trips
min [ nTrips ] of bees
17
1
11

MONITOR
1619
834
1733
879
max N trips
max [ nTrips ] of bees
17
1
11

CHOOSER
1280
340
1504
385
BeeSpecies
BeeSpecies
"Honeybees" "Bumblebees"
0

BUTTON
8
288
170
321
Bumblebees
BumblebeesProc\n\nset BeeSpecies \"Bumblebees\"\nset SearchMode \"known flowerpatch (individual)\"\nset N_Bees 30\nset TripDuration_s 600\n\nset ScoutingPeriod_hrs 9\nset LinearisationFactor 1\nset DisplacementFactor 1\nset RandomWalk false\nset RandomTripDuration true\nset ImmediateReturn false\nset MaxTrips 999999\nset TurnToDestinationProb 0.02\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
8
255
170
288
Honeybees
HoneybeesProc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1765
384
1852
417
Add sources
\nask patches\n[if random-float 1 < 0.01 \n   [set pcolor read-from-string SetColour] ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1352
679
1732
832
furthest distance [m]
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot MaxHiveDisplAllBees_m"

SLIDER
174
675
509
708
Gridsize
Gridsize
25
2000
1000
25
1
m
HORIZONTAL

TEXTBOX
1281
10
1431
33
Bee movement:
19
0.0
1

BUTTON
1183
469
1269
502
calc detProb
ask patches [ set pcolor gray ]\nlet maxPdetect max [ calculatedDetectionProb_per_trip ] of patchStatistics\nif maxPdetect > 0\n[\n  ask patches with [flowerPatchID != -1] \n      [ set mapDisplay [calculatedDetectionProb_per_trip] of patchstatistic flowerPatchID ] \n ; ask patches with [flowerPatchID != -1] [ set pcolor scale-color orange sqrt mapDisplay 0 sqrt (maxPdetect / 20)]\n  ask patches with [flowerPatchID != -1] [ set pcolor scale-color orange sqrt mapDisplay 0 0.2 ]  \n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
423
1379
499
1412
Sunflower
set Patchtype_Y \"\\\"Sunflower\\\"\"\nset Start_Y 237\nset Stop_Y 264\nset Nectar_Y 0.003\nset Pollen_Y 0.108\nset t_Nectar_Y 9256\nset t_Pollen_Y 79\nset Conc_Y 1.25\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
8
335
158
358
My scenarios:
19
0.0
1

BUTTON
7
362
62
395
HB
Default_ButtonProc\nHoneybeesProc\nset Scale_X1 184\nset Scale_X2 225\nset ScaleDistance_m 3000\nset InputFile \"S6_RealLandscape.jpg\"\nset Col_X 160\nset Col_Y 106\nset MaxPatchRadius_m 500\nset N_Bees 10000\nSetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
118
362
173
395
S1
;;; OUTCOMMENT this line (add a semicolon):\nuser-message \"Right click on this button and edit to setup your own, pre-defined scenario. Activate commands by deleting the semicolon before the relevant line and put a semicolon before this user-message!\"\n\n;HoneybeesProc OR BumblebeesProc ; choose either honey bees or bumble bees\n;set BluePatches true             ; if there are no blue patches, set to \"false\"!\n;set GreenPatches true            ; if there are no green patches, set to \"false\"!\n;set RedPatches true              ; if there are no red patches, set to \"false\"!\n;set YellowPatches true           ; if there are no yellow patches, set to \"false\"!\n;set Lakes false                  ; if there are lakes/obstacles patches, set to \"true\"!\n;set Scale_X1 1                   ; right click on the map to see the x-value of your first reference point\n;set Scale_X2 300                 ; right click on the map to see the x-value of your second reference point\n;set ScaleDistance_m              ; real x-distance [m] between your reference points \n;set InputFile \"MyInputFile.jpg\"  ; file name of your input map - add this also as an option to InputFile!          \n;set Col_X                        ; x-coordinate of your hive \n;set Col_Y                        ; y-coordinate of your hive\n;set MaxPatchRadius_m 500         ; approx. the max. radius a patch can have - larger areas are divided into sub-patches\n;Setup                            ; start the Setup procedure\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
7
395
62
429
S2
;;; OUTCOMMENT this line (add a semicolon):\nuser-message \"Right click on this button and edit to setup your own, pre-defined scenario. Activate commands by deleting the semicolon before the relevant line and put a semicolon before this user-message!\"\n\n;HoneybeesProc OR BumblebeesProc ; choose either honey bees or bumble bees\n;set BluePatches true             ; if there are no blue patches, set to \"false\"!\n;set GreenPatches true            ; if there are no green patches, set to \"false\"!\n;set RedPatches true              ; if there are no red patches, set to \"false\"!\n;set YellowPatches true           ; if there are no yellow patches, set to \"false\"!\n;set Lakes false                  ; if there are lakes/obstacles patches, set to \"true\"!\n;set Scale_X1 1                   ; right click on the map to see the x-value of your first reference point\n;set Scale_X2 300                 ; right click on the map to see the x-value of your second reference point\n;set ScaleDistance_m              ; real x-distance [m] between your reference points \n;set InputFile \"MyInputFile.jpg\"  ; file name of your input map - add this also as an option to InputFile!          \n;set Col_X                        ; x-coordinate of your hive \n;set Col_Y                        ; y-coordinate of your hive\n;set MaxPatchRadius_m 500         ; approx. the max. radius a patch can have - larger areas are divided into sub-patches\n;Setup                            ; start the Setup procedure\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
63
395
118
429
S3
;;; OUTCOMMENT this line (add a semicolon):\nuser-message \"Right click on this button and edit to setup your own, pre-defined scenario. Activate commands by deleting the semicolon before the relevant line and put a semicolon before this user-message!\"\n\n;HoneybeesProc OR BumblebeesProc ; choose either honey bees or bumble bees\n;set BluePatches true             ; if there are no blue patches, set to \"false\"!\n;set GreenPatches true            ; if there are no green patches, set to \"false\"!\n;set RedPatches true              ; if there are no red patches, set to \"false\"!\n;set YellowPatches true           ; if there are no yellow patches, set to \"false\"!\n;set Lakes false                  ; if there are lakes/obstacles patches, set to \"true\"!\n;set Scale_X1 1                   ; right click on the map to see the x-value of your first reference point\n;set Scale_X2 300                 ; right click on the map to see the x-value of your second reference point\n;set ScaleDistance_m              ; real x-distance [m] between your reference points \n;set InputFile \"MyInputFile.jpg\"  ; file name of your input map - add this also as an option to InputFile!          \n;set Col_X                        ; x-coordinate of your hive \n;set Col_Y                        ; y-coordinate of your hive\n;set MaxPatchRadius_m 500         ; approx. the max. radius a patch can have - larger areas are divided into sub-patches\n;Setup                            ; start the Setup procedure\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
63
362
118
395
BB
Default_ButtonProc\nBumblebeesProc\nset BluePatches true\nset GreenPatches false\nset RedPatches false\nset YellowPatches false\nset Lakes true\nset Scale_X1 70\nset Scale_X2 100\nset ScaleDistance_m 100\nset InputFile \"S6_Map_Osborne.jpg\"\nset Col_X 184\nset Col_Y 113\nset MaxPatchRadius_m 500\nSetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
117
396
172
429
S4
;;; OUTCOMMENT this line (add a semicolon):\nuser-message \"Right click on this button and edit to setup your own, pre-defined scenario. Activate commands by deleting the semicolon before the relevant line and put a semicolon before this user-message!\"\n\n;HoneybeesProc OR BumblebeesProc ; choose either honey bees or bumble bees\n;set BluePatches true             ; if there are no blue patches, set to \"false\"!\n;set GreenPatches true            ; if there are no green patches, set to \"false\"!\n;set RedPatches true              ; if there are no red patches, set to \"false\"!\n;set YellowPatches true           ; if there are no yellow patches, set to \"false\"!\n;set Lakes false                  ; if there are lakes/obstacles patches, set to \"true\"!\n;set Scale_X1 1                   ; right click on the map to see the x-value of your first reference point\n;set Scale_X2 300                 ; right click on the map to see the x-value of your second reference point\n;set ScaleDistance_m              ; real x-distance [m] between your reference points \n;set InputFile \"MyInputFile.jpg\"  ; file name of your input map - add this also as an option to InputFile!          \n;set Col_X                        ; x-coordinate of your hive \n;set Col_Y                        ; y-coordinate of your hive\n;set MaxPatchRadius_m 500         ; approx. the max. radius a patch can have - larger areas are divided into sub-patches\n;Setup                            ; start the Setup procedure\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1765
450
1853
541
Show
\nlet centre patch 0 0\nlet outside false\nif count hives = 0\n[\n  create-hives 1 \n    [ \n      set color 33 \n      set shape \"square\" \n      set size 4 \n      setxy Col_X Col_Y \n    ]\n]\n\n\nask one-of hives \n[ \n  ifelse patch-at-heading-and-distance SetDirection_deg (SetDistanceToCentre_m / Scaling) != nobody\n    [ set centre patch-at-heading-and-distance SetDirection_deg (SetDistanceToCentre_m / Scaling) ]\n    [ \n      user-message \"Patch outside of map!\"\n      set outside true \n    ]\n    \n]  \n\nif outside = false \n[\n\n  let centreX [ pxcor ] of centre\n  let centreY [ pycor ] of centre\n                 \n  ask patches with [distancexy centreX centreY <= (SetRadius_m / Scaling)] \n    [ \n      set pcolor read-from-string SetColour \n    ] \n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1765
542
1853
587
Delete
\nlet centre patch 0 0\nlet outside false\nif count hives = 0\n[\n  create-hives 1 \n    [ \n      set color 33 \n      set shape \"square\" \n      set size 4 \n      setxy Col_X Col_Y \n    ]\n]\n\n\nask one-of hives \n[ \n  ifelse patch-at-heading-and-distance SetDirection_deg (SetDistanceToCentre_m / Scaling) != nobody\n    [ set centre patch-at-heading-and-distance SetDirection_deg (SetDistanceToCentre_m / Scaling) ]\n    [ \n      user-message \"Patch outside of map!\"\n      set outside true \n    ]\n    \n]  \n\nif outside = false \n[\n\n  let centreX [ pxcor ] of centre\n  let centreY [ pycor ] of centre\n                 \n  ask patches with [distancexy centreX centreY <= (SetRadius_m / Scaling)] \n    [ \n      set pcolor gray\n    ] \n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
345
759
437
804
Date
DateREP
17
1
11

TEXTBOX
13
977
110
995
Switch on/off:\n
14
0.0
1

TEXTBOX
13
1011
163
1029
Colour corrections:
14
0.0
1

TEXTBOX
13
1079
89
1097
Crop type:
14
0.0
1

TEXTBOX
13
1135
172
1192
Phenology:\n(Day of year of flowering)\n
14
0.0
1

TEXTBOX
14
1200
132
1234
Nectar [ml/m^2]\n& Pollen [g/m^2]:\n
14
0.0
1

TEXTBOX
13
1266
135
1284
Handling times [s]:
14
0.0
1

TEXTBOX
13
1334
163
1352
Concentration [mol/l]:
14
0.0
1

TEXTBOX
13
1385
84
1403
Examples:
14
0.0
1

TEXTBOX
1287
576
1374
594
Find a patch:
9
0.0
1

TEXTBOX
1755
320
1877
338
(requires \"Update map\")
9
0.0
1

TEXTBOX
1135
34
1269
56
(requires \"Setup\" or \"Update\")
9
0.0
1

TEXTBOX
1419
393
1501
411
(requires \"Setup\")
9
0.0
1

TEXTBOX
1294
479
1393
501
(requires \"Clear bees\")
9
0.0
1

TEXTBOX
1425
479
1504
497
(requires \"Setup\")
9
0.0
1

TEXTBOX
389
939
468
957
(requires \"Setup\")
9
0.0
1

TEXTBOX
1422
19
1572
37
(changes applied instantly)
9
0.0
1

BUTTON
1765
417
1852
450
Add by colour
if any? patches with [ pcolor = ReplaceColour ]\n[\n ask patches with [ pcolor = ReplaceColour ]\n   [if random-float 1 < 0.01 \n       [set pcolor read-from-string SetColour] ]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1456
443
1505
461
(0: not set)
9
0.0
1

MONITOR
5
548
167
593
Progress of simulation run [%]
100 * [time_s] of one-of bees / (ScoutingPeriod_hrs * 3600)
2
1
11

MONITOR
1083
833
1169
878
hours passed
floor ((3 * ticks) / 3600)
17
1
11

@#$#@#$#@
# Terms of use of the software BEESCOUT

BEESCOUT is a landscape module for the honeybee model BEEHAVE.

BEESCOUT and BEEHAVE can be downloaded for free from http://beehave-model.net/.

Publications:
Becher, M. A., Grimm, V., Thorbek, P., Horn, J., Kennedy, P. J., & Osborne, J. L. (2014) BEEHAVE: a systems model of honeybee colony dynamics and foraging to explore multifactorial causes of colony failure. _Journal of Applied Ecology_, 51, 470-482.

Becher, M.A., Grimm, V., Knapp, J., Horn, J., Twiston-Davies, G., Osborne, J.L. (2016)
BEESCOUT: a model of bee scouting behaviour and a software tool for characterizing nectar/pollen landscapes for BEEHAVE. _Ecological Modelling_.


## Copyright and Licence Information:
© The University of Exeter, Matthias Becher 2016

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

A copy of the GNU General Public License can be found at http://www.gnu.org/licenses/gpl.html or write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.



## Recommendations when using BEEHAVE and BEESCOUT:

• Please refer to the BEEHAVE/BEESCOUT publication (Becher et al. 2014; Becher et al. 2016see above) and the BEEHAVE website (http://beehave-model.net/) when using BEEHAVE/BEESCOUT.

• We recommend that any publication or report based on using BEEHAVE or BEESCOUT shall include, in the Supplementary Material, the very NetLogo file that was used to produce the corresponding figure, table, or other kinds of results, as well as the "Experiments" in the BehaviorSpace and all necessary input files (see Supplementary Material of Becher et al. 2014 as example). If you changed the code, we recommend to document these changes in all detail and to provide a revised ODD model description in which the modified or added elements are highlighted.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee_mb_1
true
0
Circle -7500403 true true 110 75 80
Circle -7500403 true true 101 157 98
Circle -6459832 true false 107 124 86
Line -7500403 true 150 100 105 60
Line -7500403 true 150 100 195 60
Circle -7500403 true true 103 178 92
Circle -7500403 true true 117 227 62
Polygon -7500403 true true 120 150 60 225 60 240 75 255 105 255 120 240 135 165 120 150
Polygon -7500403 true true 180 150 240 225 240 240 225 255 195 255 180 240 165 165 180 150
Circle -16777216 true false 116 88 19
Circle -16777216 true false 163 86 19
Circle -16777216 true false 112 99 19
Circle -16777216 true false 168 97 19

beehive1
false
3
Rectangle -6459832 true true 15 135 285 270
Rectangle -7500403 true false 0 105 300 135
Line -16777216 false 15 240 285 240
Rectangle -16777216 true false 120 240 180 255

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="2014-10-23_MaxHiveDispl" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks * 3</metric>
    <metric>MaxHiveDisplAllBees_m</metric>
    <enumeratedValueSet variable="InputFile">
      <value value="&quot;No input file&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxTrips">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N_Bees">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandSeed">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
      <value value="17"/>
      <value value="18"/>
      <value value="19"/>
      <value value="20"/>
      <value value="21"/>
      <value value="22"/>
      <value value="23"/>
      <value value="24"/>
      <value value="25"/>
      <value value="26"/>
      <value value="27"/>
      <value value="28"/>
      <value value="29"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BeeSpecies">
      <value value="&quot;Honeybees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Black_th">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_max">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_min">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BluePatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BrushSize">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ByColour">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_X">
      <value value="160"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_Y">
      <value value="106"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_B">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_G">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_R">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_Y">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Day_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DisplacementFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixRightTurn">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixTurningAngle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ForagingMap">
      <value value="&quot;Nectar&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_max">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_min">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GreenPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Gridsize">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Highlight_Patch">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ImmediateReturn">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputForagingFile">
      <value value="&quot;Input_1-2_Foraging.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lakes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LinearisationFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxForagingRange_m">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxPatchRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxVisitsColour">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N_Bees">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NameOutfile">
      <value value="&quot;Input_2-1_FoodFlow.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_B">
      <value value="&quot;\&quot;BlueField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_G">
      <value value="&quot;\&quot;GreenField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_R">
      <value value="&quot;\&quot;RedField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_Y">
      <value value="&quot;\&quot;YellowField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot3">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot4">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomTripDuration">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomWalk">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_max">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_min">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RedPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ReplaceColour">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SatelliteFile">
      <value value="&quot;No satellite image&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X1">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X2">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScaleDistance_m">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScoutingPeriod_hrs">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SearchMode">
      <value value="&quot;known flowerpatch (recruitment)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetColour">
      <value value="&quot;Yellow&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetDirection_deg">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_B">
      <value value="181"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_G">
      <value value="271"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_Y">
      <value value="91"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_B">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_G">
      <value value="360"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_R">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_Y">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_B">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_G">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_R">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_Y">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_B">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_G">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_R">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_Y">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TripDuration_s">
      <value value="1020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TurnToDestinationProb">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="White_th">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_max">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_min">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="YellowPatches">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2015-02-11_JLO-Plos1-BB" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>OutputWordResult</metric>
    <enumeratedValueSet variable="BluePatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GreenPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RedPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="YellowPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lakes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X1">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X2">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScaleDistance_m">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputFile">
      <value value="&quot;S6_Map_Osborne.jpg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_X">
      <value value="184"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_Y">
      <value value="113"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxPatchRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N_Bees">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SearchMode">
      <value value="&quot;known flowerpatch (individual)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxTrips">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BeeSpecies">
      <value value="&quot;Bumblebees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandSeed">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
      <value value="17"/>
      <value value="18"/>
      <value value="19"/>
      <value value="20"/>
      <value value="21"/>
      <value value="22"/>
      <value value="23"/>
      <value value="24"/>
      <value value="25"/>
      <value value="26"/>
      <value value="27"/>
      <value value="28"/>
      <value value="29"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Black_th">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_max">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_min">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BrushSize">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ByColour">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_B">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_G">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_R">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_Y">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Day_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DisplacementFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixRightTurn">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixTurningAngle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ForagingMap">
      <value value="&quot;Nectar&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_max">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_min">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Gridsize">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Highlight_Patch">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ImmediateReturn">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputForagingFile">
      <value value="&quot;Input_1-2_Foraging.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LinearisationFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxForagingRange_m">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxVisitsColour">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NameOutfile">
      <value value="&quot;Input_2-1_FoodFlow.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_B">
      <value value="&quot;\&quot;BlueField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_G">
      <value value="&quot;\&quot;GreenField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_R">
      <value value="&quot;\&quot;RedField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_Y">
      <value value="&quot;\&quot;YellowField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot3">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot4">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomTripDuration">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomWalk">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_max">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_min">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ReplaceColour">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SatelliteFile">
      <value value="&quot;No satellite image&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScoutingPeriod_hrs">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetColour">
      <value value="&quot;Yellow&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetDirection_deg">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetDistanceToCentre_m">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_B">
      <value value="181"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_G">
      <value value="271"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_Y">
      <value value="91"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_B">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_G">
      <value value="360"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_R">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_Y">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_B">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_G">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_R">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_Y">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_B">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_G">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_R">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_Y">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TripDuration_s">
      <value value="1020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TurnToDestinationProb">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="White_th">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_max">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_min">
      <value value="40"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2015-02-17_RealLandscape_RBY_10000bees" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count patches with [ visits &gt; 0 ] * (SCALING / 1000) ^ 2</metric>
    <metric>MaxHiveDisplAllBees_m</metric>
    <metric>OutputWordResult</metric>
    <enumeratedValueSet variable="RedPatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BluePatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="YellowPatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X1">
      <value value="184"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X2">
      <value value="225"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScaleDistance_m">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputFile">
      <value value="&quot;S6_RealLandscape.jpg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_X">
      <value value="160"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_Y">
      <value value="106"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxPatchRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N_Bees">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SearchMode">
      <value value="&quot;colony&quot;"/>
      <value value="&quot;known flowerpatch (individual)&quot;"/>
      <value value="&quot;known flowerpatch (recruitment)&quot;"/>
      <value value="&quot;random location&quot;"/>
      <value value="&quot;visited NLpatch (recruitment)&quot;"/>
      <value value="&quot;furthest location (individual)&quot;"/>
      <value value="&quot;last location (individual)&quot;"/>
      <value value="&quot;mixed strategy (individual)&quot;"/>
      <value value="&quot;mixed strategy (recruitment)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandSeed">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
      <value value="17"/>
      <value value="18"/>
      <value value="19"/>
      <value value="20"/>
      <value value="21"/>
      <value value="22"/>
      <value value="23"/>
      <value value="24"/>
      <value value="25"/>
      <value value="26"/>
      <value value="27"/>
      <value value="28"/>
      <value value="29"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BeeSpecies">
      <value value="&quot;Honeybees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Black_th">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_max">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_min">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BrushSize">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ByColour">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_B">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_G">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_R">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_Y">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Day_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DisplacementFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixRightTurn">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixTurningAngle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ForagingMap">
      <value value="&quot;Nectar&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_max">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_min">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GreenPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Gridsize">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Highlight_Patch">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ImmediateReturn">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputForagingFile">
      <value value="&quot;Input_1-2_Foraging.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lakes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LinearisationFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxForagingRange_m">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxTrips">
      <value value="999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxVisitsColour">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NameOutfile">
      <value value="&quot;Input_2-1_FoodFlow.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_B">
      <value value="&quot;\&quot;BlueField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_G">
      <value value="&quot;\&quot;GreenField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_R">
      <value value="&quot;\&quot;RedField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_Y">
      <value value="&quot;\&quot;YellowField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot3">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot4">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomTripDuration">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomWalk">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_max">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_min">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ReplaceColour">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SatelliteFile">
      <value value="&quot;No satellite image&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScoutingPeriod_hrs">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetColour">
      <value value="&quot;Yellow&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetDirection_deg">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetDistanceToCentre_m">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_B">
      <value value="181"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_G">
      <value value="271"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_Y">
      <value value="91"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_B">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_G">
      <value value="360"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_R">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_Y">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_B">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_G">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_R">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_Y">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_B">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_G">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_R">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_Y">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TripDuration_s">
      <value value="1020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TurnToDestinationProb">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="White_th">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_max">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_min">
      <value value="40"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2014-11-03_UnConnected" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>ask bees [ hide-turtle]
ask patchstatistic 0 [ set xcor 220 set ycor 97 ]

let maxVisits max [ visits ] of patches
if maxVisits &gt; 0
[
  ask patches
    ;[ set pcolor scale-color sky Visits 0 (maxVisits / 20) ]
    [ set pcolor scale-color sky sqrt Visits 0 sqrt (maxVisits / 10) ]


 ]
export-view (word "Distribution_" InputFile)</final>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="InputFile">
      <value value="&quot;S6_ConnectedPatch.png&quot;"/>
      <value value="&quot;S6_UnconnectedPatch.png&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxTrips">
      <value value="999999"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BrushSize">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_Y">
      <value value="&quot;\&quot;YellowField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_B">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Black_th">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ByColour">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N_Bees">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GreenPatches">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxForagingRange_m">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X2">
      <value value="221"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="White_th">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_B">
      <value value="181"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_max">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="YellowPatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxVisitsColour">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_Y">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScaleDistance_m">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_B">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomWalk">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_X">
      <value value="154"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TurnToDestinationProb">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputForagingFile">
      <value value="&quot;Input_1-2_Foraging.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_Y">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot4">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_R">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_Y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot3">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_Y">
      <value value="91"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_min">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_G">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_G">
      <value value="360"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_max">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_min">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandSeed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixRightTurn">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Gridsize">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NameOutfile">
      <value value="&quot;Input_2-1_FoodFlow.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_G">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MaxPatchRadius_m">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Highlight_Patch">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_B">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lakes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nectar_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScoutingPeriod_hrs">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ScaleDistance_m">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Start_G">
      <value value="271"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ForagingMap">
      <value value="&quot;Nectar&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetDirection_deg">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetRadius_m">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_R">
      <value value="&quot;\&quot;RedField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RandomTripDuration">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TripDuration_s">
      <value value="1020"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Green_max">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_R">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RedPatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SetColour">
      <value value="&quot;Grey&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_B">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_Y">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SearchMode">
      <value value="&quot;known flowerpatch (recruitment)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Red_min">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_G">
      <value value="&quot;\&quot;GreenField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Day_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Col_Y">
      <value value="97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BluePatches">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LinearisationFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop_Y">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_G">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Nectar_G">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Conc_R">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Plot2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yellow_max">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_R">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scale_X1">
      <value value="154"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pollen_R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DisplacementFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="FixTurningAngle">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t_Pollen_B">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ReplaceColour">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patchtype_B">
      <value value="&quot;\&quot;BlueField\&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ImmediateReturn">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="BeeSpecies">
      <value value="&quot;Honeybees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SatelliteFile">
      <value value="&quot;No satellite image&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Blue_min">
      <value value="90"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
