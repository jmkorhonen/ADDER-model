;; ***************
;; * ADDER MODEL *
;; ***************
;;
;;
;; PRELIMINARIES
;;
;; *******************************************************************
breed [techs tech] ;; instead of turtles, let's call them technologies
;; breed [developers developer] 
;; *******************************************************************

;; *******************************************************************
techs-own [
 product ;; the "product" of the technology
 cost ;; the cost of the technology
 components ;; a list of components used
 operators ;; a list of operators used (this list is separate so that debugging is easier
 cost-of-components ;; cost of components used 
 contributor-tech-whos ;; a list of IDs - which unique techs are used by this tech
 tech-ID ;; the unique ID of the tech
 created ;; timestamp when developed
 last-updated ;; timestamp when updated last
 active-repertoire? ;; is tech in active use?
 debug ;; general debug variable
]
;; *******************************************************************

;; developers-own [
;; nothing here yet
;; ]

;; *******************************************************************
globals [
 next-tech-ID ;; a counter for tech IDs
 list-of-operators
 failed-techs ;;  counting how many techs failed, i.e. were killed because product < 2, if kill-negative-techs? = TRUE
 cumulative-replacements ;;  counting cumulative replacements
 list-of-replacements ;; a list where number of replacements is saved during update, to draw a histogram
 highest-tech-reached ;; the highest product reached so far 
 techlevels-found ;; a list of techlevels found
 size-of-active-repertoire ;; number of techs in active repertoire
 viable-techs-created-this-turn? ;; were there viable techs created during this turn? 
 last-techlevel-created ;; these are used to update proper technologies 
 last-tech-who-created ;; these are used to update proper technologies
 stop-called? ;; TRUE/FALSE, mostly for debugging
 obsoleted-techs ;; used in update techs procedure
 old-product ;; debugging variable...
]
;; *******************************************************************

;; *******************************************************************
to setup
 ;;
 ;; INITIALIZING THE WORLD
 ;;
 clear-all
 let seed-number 123456
 random-seed random-generator-seed ;; for setting the random seed
 set list-of-operators [1 -1] ;; if we want to alter the % distribution of + and - operators, we can alter this e.g. [1 1 -1]
 set list-of-replacements [] ;; setting up an empty list
 set-patch-size 5
 resize-world 0 50 0 50 ;; syntax: resize-world min-pxcor max-pxcor min-pycor max-pycor 
 set-default-shape techs "circle"
 print (" ")  
 print (word "----------- ADDER READY -----------")
 print (word "--- " date-and-time " ---")
 print (word "-----------------------------------")
 print (" ")
 print (word "Random seed: " random-generator-seed)
 print (word "Max components " max-components)
 print (word "Kill negative techs? " kill-negative-techs?)
 print (" ")

 ;;
 ;; CREATING A SET OF PRIMITIVES
 ;;
 create-techs 1 [
  set product 1
  set cost 1
  set components [] ;; sets up an empty list
  set cost-of-components [] ;; empty, not needed usually but here to avoid bugs
  set contributor-tech-whos [] ;; empty, not needed usually but here to avoid bugs
  set created 0 ;; primitives are gen 0
  set active-repertoire? TRUE
  set tech-ID "p"
  setxy 0 0
  set color green 
  ]
 set next-tech-ID 1
 set highest-tech-reached 1
 set size-of-active-repertoire 1 
 set techlevels-found []
 
 ;; creating developer agents
 ;; create-developers 1 [
  ;; set developer values here
  ;; though maybe "developers" could be patches???
  ;; could use patch x and y coordinates e.g.
  ;; x-coord = division to Standing reserve, Active repertoire, Cumulative replacements
  ;; y-coord = different agents
  ;; e.g. use 3x1 board for 1 agent, 3x3 for 3 agents
 ;; ]  
end
;; *******************************************************************

;; *******************************************************************
to go
 if ticks = stop-at-tick [stop]
 if stop-called? = TRUE [stop]
 tick
 if debugging-mode = TRUE [print (word "--- " ticks " ---")]
 set viable-techs-created-this-turn? FALSE ;; no techs created yet
 set obsoleted-techs [] ;; empties the list of obsoleted techs, just in case
 update-active-repertoire-simple
 create-next-tech
 if viable-techs-created-this-turn? = TRUE [ ;; Checks whether there is a need to update technologies
    if any? techs with [product = last-techlevel-created and cost > [cost] of tech last-tech-who-created] [ ;; ...and if there are no existing similar higher-cost techs, no need to update either
        set obsoleted-techs [who] of techs with [product = last-techlevel-created and cost > [cost] of tech last-tech-who-created]
        if debugging-mode = TRUE [
          print (word "techs " obsoleted-techs " obsoleted at turn " (ticks) " by tech " "(" ([product] of tech last-tech-who-created) ")" last-tech-who-created)
          ] ;; debugging
        while [not empty? obsoleted-techs] [
          update-techs-simple ;; calls a procedure to update technologies
        ]
    ]
 ]
 do-plotting
end
;; *******************************************************************

;; *******************************************************************
to update-active-repertoire-simple ;; simplest version of update
 ;; this needs to:
 ;; 1. determine current least-cost techs
 ;; 2. determine current closest-to-target techs (if not all numbers are targets)
 ;; 3. add these techs to agentset "active repertoire"
 ;; 4. add primitives to the above set
 ask techs with [created > 0] [set active-repertoire? FALSE] ;; AR cleared of all but primitives
 
 
 foreach remove-duplicates [product] of techs [
     ask min-one-of techs with [product = ?] [cost] [set active-repertoire? TRUE] ;; the least-cost tech is added to AR
   ]
 
 ask techs with [active-repertoire? = TRUE] [set color green]
 ask techs with [active-repertoire? = FALSE] [set color red]
 set size-of-active-repertoire count techs with [active-repertoire? = TRUE]
end
;; *******************************************************************

;; *******************************************************************
to create-next-tech
  ;; needs to:
  ;; 1. draw random components from the list of available components or zero
  ;; 2. assign + or - operators
  ;; 3. determine product 
  ;; 4. determine cost 
  ;; 5. name tech and assign values
  ;;
  ;; CREATING PROSPECTIVE TECHNOLOGY
  ;;
  create-techs 1 [
    ;;
    ;; PRELIMINARIES
    ;;
    set color black ;; eye candy
    set created ticks ;; assigns timestamp
    set tech-ID next-tech-ID
    set active-repertoire? FALSE ;; to avoid using this tech in the draw, maybe unnecessary by using <one-of other techs> syntax below?
    set components [] ;; sets up an empty list for components
    set operators [] ;; sets up an empty list for operators
    set cost-of-components [] ;; sets up an empty list for cost of components
    set contributor-tech-whos [] ;; sets up an empty list for "contributing" technologies"
    ;;
    ;; RANDOMIZING COMPONENTS FOR THE NEW TECHNOLOGY (up to max-components # of components)
    ;;
    repeat 2 [ 
    let target [who] of one-of other techs with [active-repertoire? = TRUE] ;; let's pick a random technology
    set operators lput one-of list-of-operators operators ;; ... a random operator
    set components lput [product] of tech target components ;; ... and add it as the first and second components
    set cost-of-components lput [cost] of tech target cost-of-components ;; saves the cost of this tech, if added component is other than 0
    set contributor-tech-whos lput target contributor-tech-whos ;; adds the [who] of "contributor" technology to the end of the list
    ]
    ;; Now we pick components 3, ... , n
    repeat max-components - 2 [ ;; -2 here to ensure that all new technologies are combinations - not just duplicates with all the rest as 0!
      let target [who] of one-of other techs with [active-repertoire? = TRUE] ;; let's pick a random technology
      let op one-of list-of-operators ;; ... and a random operator from the list of operators 
      let component-added one-of (list 0 ([product] of tech target)) ;; draws either 0 or target component 
      set operators lput op operators
      set components lput component-added components ;; adds next component to the end of list of components
      ;;
      ;; SAVING THE COST DATA
      ;; 
      ifelse component-added != 0 [
        set cost-of-components lput [cost] of tech target cost-of-components ;; saves the cost of this tech, if added component is other than 0
        set contributor-tech-whos lput target contributor-tech-whos ;; adds the [who] of "contributor" technology to the end of the list
      ] [  ;; ELSE block - if a zero is added, the cost is zero as well
      set cost-of-components lput 0 cost-of-components
      set contributor-tech-whos lput "na" contributor-tech-whos ;; but we need to add a "na" (0 is primitive) in order to keep proper positions for update sequence
      ]
    ]
    ;;
    ;; DETERMINING THE PRODUCT
    ;;
    calculate-product ;; subfunction that adds up the components to determine the "product" 
    ;;
    ;; CHECKING WHETHER THE CREATED TECH IS VIABLE
    ;;
    ifelse kill-negative-techs? = TRUE [
      ifelse [product] of self < 2 [ ;; 2 is here to prevent techs like [1 0 0 0]
        die ;; if negative, zero, and 1 techs aren't allowed, kills off the tech; 
        ;; another way would be to simply active-repertoire? = FALSE for negative techs, but this may 
        ;; save some space by killing off unnecessary techs...decisions, decisions!
        set failed-techs failed-techs + 1 ;; tallies the statistics
      ] [ ;; ELSE block for ifelse [product] of self < 2 begins
      set viable-techs-created-this-turn? TRUE ;; ELSE - unless tech is killed, a viable tech will be created
      set last-techlevel-created [product] of self ;; 
      set last-tech-who-created [who] of self ;; 
      ] 
    ] [ ;; ELSE block for ifelse kill-negative-techs? = TRUE begins; this is done if negative techs are allowed
    set viable-techs-created-this-turn? TRUE ;; ELSE - if kill-negative-techs? = FALSE, every turn creates viable technologies  
    set last-techlevel-created [product] of self ;;
    set last-tech-who-created [who] of self ;; 
    ]
    ;;
    ;; COUNTING THE COST
    ;;
    set cost sum [cost-of-components] of self
    ;; 
    ;; CHECKING IF TECHNOLOGY FRONTIER ADVANCES
    ;;
    if [product] of self > highest-tech-reached [ 
      set highest-tech-reached [product] of self ;; updates highest-tech-reached
      set techlevels-found lput [product] of self techlevels-found ;; adds the tech to the list of techs found
      if debugging-mode = TRUE [print (word "At tick " ticks ", " techlevels-found)] ;; debugging...
    ]
    ;;
    ;; ADVANCING TECH ID COUNTER
    ;;
    if viable-techs-created-this-turn? = TRUE [set next-tech-ID next-tech-ID + 1]
    ;;
    ;; EYE CANDY (VISUALIZING)
    ;;   
    setxy (ln [product] of self) (ln [cost] of self)
  ]
end
;; *******************************************************************


;; *******************************************************************
to update-techs-simple
  ;;
  ;; this here should update the techs with the most efficient version
  ;;
  ;; LOGIC: 
  ;; 1. If viable techs are created this turn ->
  ;;  2. if there exists another tech with same product and the new tech is cheaper ->
  ;;   go through all technologies, starting from simplest
  ;;   if any is found ->
  ;;    replace old contributor-tech-ID with new one
  ;;    replace cost of old components with the cost of new ones
  ;;    recalculate cost
  ;;    put ID of changed technologies to a list of changed technologies
  ;;   once this 1st order update is done, go to 2. for as long as the list of changed technologies is not empty
  if empty? obsoleted-techs = FALSE  [ 
    foreach obsoleted-techs [
      let updates 0 ;; how many techs will replace the current obsolete tech with a new one
      let possible-replacements techs with [product = [product] of tech ? and who != ?] ;; let's pick an agentset of possible replacement, so that the obsolete one is not used
      let replacement [who] of min-one-of possible-replacements [cost] ;; ... aaaand we have a winner!
      if debugging-mode = TRUE [
        print(word "Possible replacements for " ? ": " ([who] of possible-replacements) ". The replacement is " replacement)
      ]
      ask techs [
        if member? ? [contributor-tech-whos] of self = TRUE [
          set updates updates + 1
          let old-cost [cost] of self
          let updated-component position ? [contributor-tech-whos] of self
          if debugging-mode = TRUE [ ;; debugging
            set old-product sum components
            set components replace-item updated-component components [product] of tech replacement
            ]
          set contributor-tech-whos replace-item updated-component contributor-tech-whos replacement 
          set cost-of-components replace-item updated-component cost-of-components [cost] of tech replacement  
          set cost sum [cost-of-components] of self
          let cost-reduction old-cost - [cost] of self
          set last-updated ticks
          setxy (ln [product] of self) (ln [cost] of self)
          if debugging-mode = TRUE [
            print (word "tech (" ([product] of self) ")" ([who] of self) " at tick " (ticks) " replaces tech " "(" ([product] of tech ?) ")" ? ", pos " updated-component ", with tech " "(" ([product] of tech replacement) ")" replacement ", cost reduced by " cost-reduction) ;; debugging 
            if sum components != old-product [print (word "ERROR! " self " old prod " old-product ", new " (sum components))]
            ;; set stop-called? TRUE ;; debugging
            print (word "Self " self ", Product of self " ([product] of self) ", count " (count techs with [product = [product] of myself]))
          ]
          
          if count techs with [product = [product] of myself] > 1 [ ;; this is bit clumsy - the idea is that for a tech to be added to the obsolescent list, there has to be at least 2 techs with same product!
            if cost-reduction > 0 [
              let old-obsoleted obsoleted-techs ;; for debugging
              set obsoleted-techs lput [who] of self obsoleted-techs ;; 
              if debugging-mode = TRUE [
                print (word "tech (" ([product] of self) ")" ([who] of self) " cascades, old obsoletes " old-obsoleted " new " obsoleted-techs) ;; debugging
              ] 
            ]
          ]
        ] 
      ]
      set obsoleted-techs remove ? obsoleted-techs ;; removes a tech once all the agents have gone through it
      if updates > 0 [set list-of-replacements lput updates list-of-replacements]
      if debugging-mode = TRUE [
        if updates > 0 [print (word "no more users of " ? "; it's removed from the list of obsolete techs. " obsoleted-techs " remain on the list.")]
        if updates = 0 [print (word "no users found for " ? "; it's removed from the list of obsolete techs. " obsoleted-techs " remain on the list.")]
        ] ;; for debugging   
    ]
  ]           
end
;; *******************************************************************

;; *******************************************************************
to do-plotting
  set-current-plot "Replacements"
  histogram list-of-replacements
  set-current-plot "Cumulative replacements"
  plot sum list-of-replacements
end
;; *******************************************************************

to calculate-product ;; small function for calculating product from a separate list of operators and components
  let operator-pos 0 ;; starts from the first operator
  foreach [operators] of self [
    set product product + ((item operator-pos components) * ?)
    set operator-pos operator-pos + 1
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
295
10
560
296
-1
-1
5.0
1
10
1
1
1
0
1
1
1
0
50
0
50
0
0
1
ticks

BUTTON
10
10
95
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
10
50
95
83
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
100
50
280
83
max-components
max-components
1
20
3
1
1
NIL
HORIZONTAL

SWITCH
100
10
280
43
kill-negative-techs?
kill-negative-techs?
0
1
-1000

MONITOR
10
190
159
235
NIL
highest-tech-reached
17
1
11

MONITOR
10
350
177
395
Cumulative replacements
sum list-of-replacements
17
1
11

MONITOR
10
295
177
340
NIL
size-of-active-repertoire
17
1
11

BUTTON
10
90
95
123
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

MONITOR
10
240
122
285
Number of techs
count techs
17
1
11

SWITCH
100
90
280
123
debugging-mode
debugging-mode
1
1
-1000

PLOT
10
415
265
635
Replacements
Cascade size
Cascade frequency
1.0
10.0
0.0
10.0
true
false
PENS
"Replacements" 1.0 1 -16777216 true

INPUTBOX
100
125
252
185
stop-at-tick
10000
1
0
Number

PLOT
280
415
580
635
Cumulative replacements
NIL
NIL
0.0
100.0
0.0
50.0
true
false
PENS
"cumulative-repls" 1.0 0 -16777216 true

INPUTBOX
130
230
282
290
random-generator-seed
123456
1
0
Number

@#$#@#$#@
WHAT IS IT?
-----------
This section could give a general understanding of what the model is trying to show or explain.


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.


THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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
NetLogo 4.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
