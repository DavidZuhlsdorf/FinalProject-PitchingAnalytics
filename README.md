# Umpire Accuracy in the 2025 World Series


<link href="README_files/libs/htmltools-fill-0.5.9/fill.css" rel="stylesheet" />
<script src="README_files/libs/htmlwidgets-1.6.4/htmlwidgets.js"></script>
<script src="README_files/libs/plotly-binding-4.12.0/plotly.js"></script>
<script src="README_files/libs/setprototypeof-0.1/setprototypeof.js"></script>
<script src="README_files/libs/typedarray-0.1/typedarray.min.js"></script>
<script src="README_files/libs/jquery-3.5.1/jquery.min.js"></script>
<link href="README_files/libs/crosstalk-1.2.2/css/crosstalk.min.css" rel="stylesheet" />
<script src="README_files/libs/crosstalk-1.2.2/js/crosstalk.min.js"></script>
<link href="README_files/libs/plotly-htmlwidgets-css-2.25.2/plotly-htmlwidgets.css" rel="stylesheet" />
<script src="README_files/libs/plotly-main-2.25.2/plotly-latest.min.js"></script>

``` r
library(tidyverse)
```

    Warning: package 'purrr' was built under R version 4.5.3

    Warning: package 'dplyr' was built under R version 4.5.3

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ✔ purrr     1.2.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(plotly)
```


    Attaching package: 'plotly'

    The following object is masked from 'package:ggplot2':

        last_plot

    The following object is masked from 'package:stats':

        filter

    The following object is masked from 'package:graphics':

        layout

``` r
library(here)
```

    here() starts at C:/Users/023dz/OneDrive - St. Lawrence University/Stat334/Stat334/FinalProject-PitchingAnalytics

## Overview

This project analyzes MLB umpire accuracy on called balls and strikes
during the 2025 World Series using Statcast data from Baseball Savant.
The goal is to evaluate how accurate umpires are in high-stakes
situations and identify where missed calls are most likely to occur.

## Research Question

**How accurate are MLB umpires in the biggest moments?**

Specifically, this project examines:

- Overall umpire accuracy  
- Accuracy by game  
- Accuracy by pitch count  
- Missed call locations  
- Differences between high-leverage and non-high-leverage situations

## Data

The dataset contains pitch-level Statcast data from the 2025 World
Series.

``` r
data <- read_excel(here::here("data/2025WS_data.xlsx"))
```

Key variables used in this analysis include:

- `plate_x` and `plate_z`: pitch location  
- `sz_top` and `sz_bot`: batter-specific strike zone boundaries  
- `balls` and `strikes`: count  
- `inning`, `home_score`, and `away_score`: game situation  
- `des`: umpire call description  
- `abs_call`,`ump_call`,`correct_call`: Created variables

The data was filtered to only include taken pitches that were called
either a ball or a called strike. I then created a simplified automated
strike zone based on pitch location. A pitch was classified as a strike
if it crossed within the horizontal strike zone and fell between the
batter’s individual strike zone height.

``` r
data$plate_x <- as.numeric(data$plate_x)
data$plate_z <- as.numeric(data$plate_z)

clean_data <- data %>%
  filter(des %in% c("Ball", "Called Strike")) %>%
  mutate(
    ump_call = if_else(des == "Called Strike", "Strike", "Ball"),
    abs_call = if_else(
      plate_x >= -0.83 & plate_x <= 0.83 &
        plate_z >= sz_bot & plate_z <= sz_top,
      "Strike",
      "Ball"
    ),
    correct_call = if_else(ump_call == abs_call, "Yes", "No"),
    z_norm = (plate_z - sz_bot) / (sz_top - sz_bot),
    count = paste(balls, strikes, sep = "-"),
    two_strike = ifelse(strikes == 2, "Two strikes", "Fewer than two strikes"),
    full_count = ifelse(balls == 3 & strikes == 2, "Yes", "No"),
    late_inning = ifelse(inning >= 7, "Yes", "No"),
    run_diff = abs(home_score - away_score),
    close_game = ifelse(run_diff <= 3, "Yes", "No"),
    high_lev = ifelse(
      late_inning == "Yes" & close_game == "Yes",
      "High leverage situation",
      "Not high leverage situation"
    )
  )
```

For this project, a high-leverage situation was defined as a pitch in
the 7th inning or later with a run differential of 3 or fewer runs. This
is a simplified definition, but it helps compare normal situations to
moments where missed calls may feel more important.

## Visualizations and Results

### Accuracy by Game

``` r
accuracy_data <- clean_data %>%
  group_by(game_date) %>%
  count(correct_call) %>%
  mutate(
    game_date = as.character(game_date),
    accuracy = (n / sum(n)) * 100
  ) %>%
  filter(correct_call == "Yes")

barplot <- ggplot(
  accuracy_data,
  aes(x = game_date, y = accuracy, fill = accuracy, label = round(accuracy, 2))
) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_c() +
  geom_hline(
    aes(yintercept = 94.15, linetype = "2025 Average Accuracy: 94.15%")
  ) +
  labs(
    title = "Umpire Accuracy by World Series Game",
    x = "Game Date",
    y = "Accuracy (%)",
    linetype = ""
  )

ggplotly(barplot, tooltip = "label")
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-73d56994614dfafc6b1e" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-73d56994614dfafc6b1e">{"x":{"data":[{"orientation":"v","width":0.90000000000000013,"base":0,"x":[2],"y":[85.714285714285708],"text":"round(accuracy, 2): 85.71","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(68,1,84,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[4],"y":[91.666666666666657],"text":"round(accuracy, 2): 91.67","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(55,173,126,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[3],"y":[92.828685258964143],"text":"round(accuracy, 2): 92.83","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(107,199,96,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[5],"y":[92.907801418439718],"text":"round(accuracy, 2): 92.91","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(110,200,94,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[7],"y":[93.442622950819683],"text":"round(accuracy, 2): 93.44","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(134,211,79,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.89999999999999991,"base":0,"x":[1],"y":[93.793103448275858],"text":"round(accuracy, 2): 93.79","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(160,215,73,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[6],"y":[95.199999999999989],"text":"round(accuracy, 2): 95.20","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(253,231,37,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996],"y":[94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006],"text":"","type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","name":"2025 Average Accuracy: 94.15%","legendgroup":"2025 Average Accuracy: 94.15%","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1],"y":[0],"name":"adfcf30a975c90b20a9417eb4fec8b3f","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"skip","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#440154"],[0.0033444816053508999,"#440355"],[0.0066889632107017998,"#440456"],[0.010033444816054199,"#440656"],[0.013377926421405098,"#450857"],[0.016722408026755998,"#450958"],[0.020066889632106899,"#450B59"],[0.023411371237457797,"#450D5A"],[0.026755852842808698,"#450E5B"],[0.030100334448161098,"#45105B"],[0.033444816053511996,"#45115C"],[0.036789297658862893,"#45135D"],[0.040133779264213798,"#45145E"],[0.043478260869564696,"#45155F"],[0.046822742474917092,"#451760"],[0.050167224080267997,"#451860"],[0.053511705685618895,"#451961"],[0.056856187290969792,"#461A62"],[0.060200668896320697,"#461B63"],[0.063545150501671588,"#461D64"],[0.066889632107023991,"#461E65"],[0.070234113712374896,"#461F65"],[0.073578595317725787,"#462066"],[0.076923076923076691,"#462167"],[0.080267558528427596,"#462268"],[0.083612040133779986,"#462369"],[0.08695652173913089,"#46246A"],[0.090301003344481795,"#46256B"],[0.093645484949832686,"#46266B"],[0.09698996655518359,"#46276C"],[0.1003344481605345,"#46286D"],[0.10367892976588688,"#46296E"],[0.10702341137123779,"#462A6F"],[0.11036789297658869,"#452B70"],[0.11371237458193958,"#452C70"],[0.11705685618729049,"#452D71"],[0.12040133779264289,"#452E72"],[0.12374581939799378,"#452F73"],[0.12709030100334467,"#453074"],[0.13043478260869559,"#453175"],[0.13377926421404648,"#453276"],[0.13712374581939737,"#453377"],[0.14046822742474979,"#453477"],[0.14381270903010068,"#443578"],[0.14715719063545157,"#443679"],[0.15050167224080249,"#44367A"],[0.15384615384615338,"#44377B"],[0.15719063545150427,"#44387C"],[0.16053511705685669,"#44397D"],[0.16387959866220758,"#443A7D"],[0.16722408026755847,"#433B7E"],[0.17056856187290939,"#433C7F"],[0.17391304347826028,"#433D80"],[0.17725752508361267,"#433E81"],[0.18060200668896359,"#433F82"],[0.18394648829431448,"#424083"],[0.18729096989966537,"#424184"],[0.19063545150501629,"#424185"],[0.19397993311036718,"#424285"],[0.19732441471571957,"#414386"],[0.20066889632107049,"#414487"],[0.20401337792642138,"#414587"],[0.20735785953177227,"#414687"],[0.21070234113712319,"#414787"],[0.21404682274247558,"#414888"],[0.21739130434782647,"#404988"],[0.22073578595317739,"#404A88"],[0.22408026755852828,"#404A88"],[0.22742474916387917,"#404B88"],[0.23076923076923009,"#404C88"],[0.23411371237458248,"#404D88"],[0.23745819397993337,"#404E88"],[0.24080267558528429,"#3F4F89"],[0.24414715719063518,"#3F5089"],[0.24749163879598607,"#3F5189"],[0.25083612040133846,"#3F5289"],[0.25418060200668935,"#3F5289"],[0.2575250836120403,"#3E5389"],[0.26086956521739119,"#3E5489"],[0.26421404682274208,"#3E5589"],[0.26755852842809297,"#3E568A"],[0.27090301003344536,"#3D578A"],[0.27424749163879625,"#3D588A"],[0.27759197324414719,"#3D598A"],[0.28093645484949809,"#3D598A"],[0.28428093645484898,"#3C5A8A"],[0.28762541806020137,"#3C5B8A"],[0.29096989966555226,"#3C5C8A"],[0.29431438127090315,"#3B5D8A"],[0.29765886287625409,"#3B5E8B"],[0.30100334448160498,"#3B5F8B"],[0.30434782608695587,"#3A5F8B"],[0.30769230769230826,"#3A608B"],[0.31103678929765916,"#3A618B"],[0.31438127090301005,"#39628B"],[0.31772575250836099,"#39638B"],[0.32107023411371188,"#38648B"],[0.32441471571906427,"#38658C"],[0.32775919732441516,"#38658C"],[0.33110367892976605,"#37668C"],[0.33444816053511695,"#37678C"],[0.33779264214046789,"#36688C"],[0.34113712374581878,"#36698C"],[0.34448160535117117,"#356A8C"],[0.34782608695652206,"#356B8C"],[0.35117056856187295,"#346B8C"],[0.35451505016722384,"#346C8D"],[0.35785953177257479,"#336D8D"],[0.36120401337792568,"#326E8D"],[0.36454849498327807,"#326F8D"],[0.36789297658862896,"#31708D"],[0.37123745819397985,"#31718D"],[0.37458193979933074,"#30718D"],[0.37792642140468169,"#2F728D"],[0.38127090301003408,"#2F738D"],[0.38461538461538497,"#2E748E"],[0.38795986622073586,"#2D758E"],[0.39130434782608675,"#2C768E"],[0.39464882943143764,"#2B778E"],[0.39799331103679003,"#2B778E"],[0.40133779264214098,"#2A788E"],[0.40468227424749187,"#2A798E"],[0.40802675585284276,"#2A7A8E"],[0.41137123745819365,"#2B7B8E"],[0.41471571906354454,"#2B7B8D"],[0.41806020066889693,"#2B7C8D"],[0.42140468227424788,"#2B7D8D"],[0.42474916387959877,"#2B7E8D"],[0.42809364548494966,"#2B7F8D"],[0.43143812709030055,"#2B7F8D"],[0.43478260869565144,"#2B808D"],[0.43812709030100383,"#2B818C"],[0.44147157190635478,"#2B828C"],[0.44481605351170567,"#2B838C"],[0.44816053511705656,"#2C838C"],[0.45150501672240745,"#2C848C"],[0.45484949832775984,"#2C858C"],[0.45819397993311073,"#2C868B"],[0.46153846153846168,"#2C878B"],[0.46488294314381257,"#2C878B"],[0.46822742474916346,"#2C888B"],[0.47157190635451585,"#2C898B"],[0.47491638795986674,"#2C8A8B"],[0.47826086956521763,"#2C8B8B"],[0.48160535117056857,"#2B8B8A"],[0.48494983277591946,"#2B8C8A"],[0.48829431438127036,"#2B8D8A"],[0.49163879598662275,"#2B8E8A"],[0.49498327759197364,"#2B8F8A"],[0.49832775919732453,"#2B8F8A"],[0.50167224080267547,"#2B9089"],[0.50501672240802631,"#2B9189"],[0.50836120401337725,"#2B9289"],[0.51170568561872964,"#2B9389"],[0.51505016722408059,"#2A9389"],[0.51839464882943143,"#2A9489"],[0.52173913043478237,"#2A9588"],[0.52508361204013321,"#2A9688"],[0.5284280936454856,"#2A9788"],[0.53177257525083654,"#2A9788"],[0.53511705685618749,"#299888"],[0.53846153846153832,"#299987"],[0.54180602006688927,"#299A87"],[0.54515050167224011,"#299B87"],[0.5484949832775925,"#289B87"],[0.55183946488294344,"#289C87"],[0.55518394648829439,"#289D87"],[0.55852842809364522,"#279E86"],[0.56187290969899617,"#279F86"],[0.56521739130434856,"#27A086"],[0.5685618729096994,"#26A086"],[0.57190635451505034,"#26A186"],[0.57525083612040129,"#26A285"],[0.57859531772575212,"#25A385"],[0.58193979933110307,"#25A485"],[0.58528428093645546,"#24A485"],[0.58862876254180629,"#24A585"],[0.59197324414715724,"#23A684"],[0.59531772575250819,"#23A784"],[0.59866220735785902,"#22A884"],[0.60200668896321141,"#24A884"],[0.60535117056856236,"#27A983"],[0.60869565217391319,"#2AAA82"],[0.61204013377926414,"#2DAA81"],[0.61538461538461509,"#2FAB81"],[0.61872909698996592,"#32AC80"],[0.62207357859531831,"#34AC7F"],[0.62541806020066926,"#36AD7E"],[0.62876254180602009,"#38AE7E"],[0.63210702341137104,"#3AAE7D"],[0.63545150501672198,"#3CAF7C"],[0.63879598662207282,"#3EB07B"],[0.64214046822742521,"#40B07B"],[0.64548494983277616,"#42B17A"],[0.64882943143812699,"#43B279"],[0.65217391304347794,"#45B278"],[0.65551839464882888,"#47B378"],[0.65886287625418127,"#48B477"],[0.66220735785953211,"#4AB476"],[0.66555183946488305,"#4BB575"],[0.66889632107023389,"#4DB675"],[0.67224080267558484,"#4EB674"],[0.67558528428093723,"#50B773"],[0.67892976588628817,"#51B872"],[0.68227424749163901,"#53B971"],[0.68561872909698995,"#54B971"],[0.68896321070234079,"#55BA70"],[0.69230769230769174,"#57BB6F"],[0.69565217391304413,"#58BB6E"],[0.69899665551839507,"#59BC6D"],[0.70234113712374591,"#5BBD6C"],[0.70568561872909685,"#5CBD6C"],[0.70903010033444769,"#5DBE6B"],[0.71237458193979863,"#5EBF6A"],[0.71571906354515102,"#5FBF69"],[0.71906354515050197,"#61C068"],[0.72240802675585281,"#62C167"],[0.72575250836120375,"#63C166"],[0.72909698996655459,"#64C266"],[0.73244147157190698,"#65C365"],[0.73578595317725792,"#66C464"],[0.73913043478260887,"#67C463"],[0.7424749163879597,"#69C562"],[0.74581939799331065,"#6AC661"],[0.74916387959866304,"#6BC660"],[0.75250836120401388,"#6CC75F"],[0.75585284280936482,"#6DC85E"],[0.75919732441471577,"#6EC85D"],[0.7625418060200666,"#6FC95C"],[0.76588628762541755,"#70CA5B"],[0.76923076923076994,"#71CB5A"],[0.77257525083612077,"#72CB59"],[0.77591973244147172,"#73CC58"],[0.77926421404682267,"#74CD57"],[0.7826086956521735,"#75CD56"],[0.78595317725752445,"#76CE55"],[0.78929765886287684,"#77CF54"],[0.79264214046822767,"#78CF53"],[0.79598662207357862,"#79D052"],[0.79933110367892957,"#7AD151"],[0.8026755852842804,"#7CD151"],[0.80602006688963279,"#7FD250"],[0.80936454849498374,"#81D250"],[0.81270903010033457,"#84D34F"],[0.81605351170568552,"#87D34F"],[0.81939799331103647,"#89D34E"],[0.8227424749163873,"#8CD44D"],[0.82608695652173969,"#8ED44D"],[0.82943143812709064,"#91D54C"],[0.83277591973244147,"#93D54C"],[0.83612040133779242,"#95D54B"],[0.83946488294314336,"#98D64B"],[0.8428093645484942,"#9AD64A"],[0.84615384615384659,"#9DD74A"],[0.84949832775919754,"#9FD749"],[0.85284280936454837,"#A1D748"],[0.85618729096989932,"#A3D848"],[0.85953177257525026,"#A6D847"],[0.86287625418060265,"#A8D947"],[0.86622073578595349,"#AAD946"],[0.86956521739130443,"#ACD946"],[0.87290969899665527,"#AFDA45"],[0.87625418060200622,"#B1DA44"],[0.87959866220735861,"#B3DB44"],[0.88294314381270955,"#B5DB43"],[0.88628762541806039,"#B7DB42"],[0.88963210702341133,"#BADC42"],[0.89297658862876217,"#BCDC41"],[0.89632107023411312,"#BEDC40"],[0.8996655518394655,"#C0DD40"],[0.90301003344481645,"#C2DD3F"],[0.90635451505016729,"#C4DE3E"],[0.90969899665551823,"#C6DE3E"],[0.91304347826086907,"#C8DE3D"],[0.91638795986622001,"#CBDF3C"],[0.9197324414715724,"#CDDF3B"],[0.92307692307692335,"#CFDF3B"],[0.92642140468227419,"#D1E03A"],[0.92976588628762513,"#D3E039"],[0.93311036789297597,"#D5E038"],[0.93645484949832836,"#D7E138"],[0.9397993311036793,"#D9E137"],[0.94314381270903025,"#DBE136"],[0.94648829431438108,"#DDE235"],[0.94983277591973203,"#DFE234"],[0.95317725752508442,"#E1E233"],[0.95652173913043526,"#E3E333"],[0.9598662207357862,"#E5E332"],[0.96321070234113715,"#E7E331"],[0.96655518394648798,"#E9E430"],[0.96989966555183893,"#EBE42F"],[0.97324414715719132,"#EDE42E"],[0.97658862876254215,"#EFE52D"],[0.9799331103678931,"#F1E52C"],[0.98327759197324405,"#F3E52B"],[0.98662207357859488,"#F5E62A"],[0.98996655518394583,"#F7E629"],[0.99331103678929822,"#F9E627"],[0.99665551839464905,"#FBE726"],[1,"#FDE725"]],"colorbar":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"thickness":23.039999999999996,"title":"accuracy","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"tickmode":"array","ticktext":["87.5","90.0","92.5","95.0"],"tickvals":[0.18929216867469956,0.45196787148594464,0.71464357429718972,0.97731927710843491],"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"ticklen":2,"len":0.5,"yanchor":"top","y":1}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":40.840182648401829,"r":7.3059360730593621,"b":37.260273972602747,"l":37.260273972602747},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Umpire Accuracy by World Series Game","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,7.5999999999999996],"tickmode":"array","ticktext":["2025-10-24","2025-10-25","2025-10-27","2025-10-28","2025-10-29","2025-10-31","2025-11-01"],"tickvals":[1,2,3,4.0000000000000009,5,6,7],"categoryorder":"array","categoryarray":["2025-10-24","2025-10-25","2025-10-27","2025-10-28","2025-10-29","2025-10-31","2025-11-01"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"Game Date","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4.7599999999999998,99.959999999999994],"tickmode":"array","ticktext":["0","25","50","75"],"tickvals":[0,25,49.999999999999993,75],"categoryorder":"array","categoryarray":["0","25","50","75"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"Accuracy (%)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"y":0.5,"yanchor":"top","title":{"text":"<br />accuracy","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"32b09be78ae":{"x":{},"y":{},"fill":{},"label":{},"type":"bar"},"32b06e3b4ec":{"yintercept":{},"linetype":{}}},"cur_data":"32b09be78ae","visdat":{"32b09be78ae":["function (y) ","x"],"32b06e3b4ec":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

Umpire accuracy across World Series games remains consistently high and
close to the MLB average of 94.15%. We can see that game 2 had a
significant drop in accuracy, but the other 6 games were relatively
consistent. This suggests that World Series umpires perform at a level
similar to the regular-season average, but sometimes can see a drop.

### Pitch Location: Correct vs. Incorrect Calls

``` r
ggplot(clean_data, aes(x = plate_x, y = z_norm, color = correct_call)) +
  geom_point(position = "jitter") +
  geom_rect(
    xmin = -0.83, xmax = 0.83,
    ymin = 0, ymax = 1,
    fill = NA, color = "black", linewidth = 1
  ) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(
    title = "Correct and Incorrect Ball/Strike Calls",
    x = "Horizontal Pitch Location",
    y = "Normalized Strike Zone Height",
    color = "Correct Call"
  ) +
  facet_wrap(~high_lev)
```

![](README_files/figure-commonmark/unnamed-chunk-5-1.png)

This plot shows where umpires made correct and incorrect calls. Most
incorrect calls occur near the edges of the strike zone. Pitches clearly
inside or outside the zone are usually called correctly, while
borderline pitches are more prone to error. The high-leverage and
non-high-leverage plots show similar patterns.

### Missed Call Density

``` r
missed_calls <- clean_data %>%
  filter(correct_call == "No")

ggplot(missed_calls, aes(x = plate_x, y = z_norm)) +
  stat_density_2d(
    aes(fill = after_stat(density)),
    geom = "raster",
    contour = FALSE
  ) +
  geom_rect(
    xmin = -0.83, xmax = 0.83,
    ymin = 0, ymax = 1,
    fill = NA, color = "white", linewidth = 1
  ) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Density of Missed Ball/Strike Calls in 2025 World Series",
    x = "Horizontal Pitch Location",
    y = "Normalized Strike Zone Height",
    fill = "Density"
  )
```

![](README_files/figure-commonmark/unnamed-chunk-6-1.png)

The heatmap highlights where missed calls occurred most often. The
highest density of missed calls appears near the top, bottom, and
corners of the strike zone. This supports the idea that missed calls are
not random. Instead, they happen in areas where the call is most
difficult.

### Accuracy by Count

``` r
accuracy_by_count <- clean_data %>%
  group_by(count) %>%
  summarise(
    total_pitches = n(),
    correct_calls = sum(correct_call == "Yes"),
    accuracy = (correct_calls / total_pitches)*100
  )
count_plot <- ggplot(accuracy_by_count, aes(x = reorder(count, accuracy), y = accuracy, label = accuracy)) +
  geom_segment(aes(xend = count, y = 0, yend = accuracy), color = "steelblue") +
  geom_point(color = "red", size = 3) +
  coord_flip() +
  theme_minimal(base_size = 14) + 
  labs(
    title = "Umpire Accuracy by Count",
    x = "Count",
    y = "Accuracy (%)"
  ) 

ggplotly(count_plot, tooltip = "label")
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-0231970d50e3d9337bc2" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-0231970d50e3d9337bc2">{"x":{"data":[{"x":[0,91.21447028423772,null,0,91.489361702127653,null,0,98.360655737704917,null,0,93.181818181818173,null,0,95.833333333333343,null,0,98.76543209876543,null,0,92.307692307692307,null,0,81.132075471698116,null,0,97.916666666666657,null,0,88.235294117647058,null,0,80],"y":[4,4,null,5,5,null,10,10,null,7,7,null,8,8,null,11,11,null,6,6,null,2,2,null,9,9,null,3,3,null,1,1],"text":["accuracy: 91.21447","accuracy: 91.21447",null,"accuracy: 91.48936","accuracy: 91.48936",null,"accuracy: 98.36066","accuracy: 98.36066",null,"accuracy: 93.18182","accuracy: 93.18182",null,"accuracy: 95.83333","accuracy: 95.83333",null,"accuracy: 98.76543","accuracy: 98.76543",null,"accuracy: 92.30769","accuracy: 92.30769",null,"accuracy: 81.13208","accuracy: 81.13208",null,"accuracy: 97.91667","accuracy: 97.91667",null,"accuracy: 88.23529","accuracy: 88.23529",null,"accuracy: 80.00000","accuracy: 80.00000"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(70,130,180,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[91.21447028423772,91.489361702127653,98.360655737704917,93.181818181818173,95.833333333333343,98.76543209876543,92.307692307692307,81.132075471698116,97.916666666666657,88.235294117647058,80],"y":[4,5,10,7,8,11,6,2,9,3,1],"text":["accuracy: 91.21447","accuracy: 91.48936","accuracy: 98.36066","accuracy: 93.18182","accuracy: 95.83333","accuracy: 98.76543","accuracy: 92.30769","accuracy: 81.13208","accuracy: 97.91667","accuracy: 88.23529","accuracy: 80.00000"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,0,0,1)","opacity":1,"size":11.338582677165356,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":47.61477791614778,"r":9.2984640929846396,"b":47.422166874221666,"l":54.860938148609378},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"title":{"text":"Umpire Accuracy by Count","font":{"color":"rgba(0,0,0,1)","family":"","size":22.316313823163139},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4.9382716049382722,103.7037037037037],"tickmode":"array","ticktext":["0","25","50","75","100"],"tickvals":[0,25,50.000000000000007,75,100],"categoryorder":"array","categoryarray":["0","25","50","75","100"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":14.877542548775427},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.8453149175440583,"zeroline":false,"anchor":"y","title":{"text":"Accuracy (%)","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,11.6],"tickmode":"array","ticktext":["3-1","2-1","3-0","0-0","0-1","2-0","1-0","1-1","2-2","0-2","1-2"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11],"categoryorder":"array","categoryarray":["3-1","2-1","3-0","0-0","0-1","2-0","1-0","1-1","2-2","0-2","1-2"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":14.87754254877543},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.8453149175440583,"zeroline":false,"anchor":"x","title":{"text":"Count","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}},"hoverformat":".2f"},"shapes":[],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.877542548775427}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"32b06678331a":{"x":{},"y":{},"label":{},"xend":{},"yend":{},"type":"scatter"},"32b076844944":{"x":{},"y":{},"label":{}}},"cur_data":"32b06678331a","visdat":{"32b06678331a":["function (y) ","x"],"32b076844944":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

Accuracy remains relatively consistent across pitch counts, but we do
see a drop when the count is “hitter-friendly”. This could suggest
umpires expand the zone slightly when the count is in favor of the
hitter. An observation worth noting is the accuracy on 2 strike counts.
The accuracy is very high in 2 strike counts, suggesting that umpires
are more focused and less likely to make an error when the batter is on
the verge of striking out.

## Key Findings

- Umpire accuracy in the 2025 World Series was generally high.  
- Most missed calls occurred near the edges of the strike zone.  
- High-leverage situations did not appear to create a major drop in
  accuracy.  
- Accuracy changed depending on the count, specifically 2 strike counts,
  and “hitter-friendly” counts

## Limitations

- This is an approximation of ABS, not official ABS data.
- Strike zone height for visuals is normalized because hitters vary in
  size.
- One World Series is a small sample.
- This project does not fully simulate how game outcomes would change.
- Future work could include full-season data or run expectancy.

## Conclusion

This project found that MLB umpire accuracy in the 2025 World Series was
generally strong, but missed calls occurred in specific areas of the
strike zone. The main pattern was that missed calls clustered near the
edges of the zone, especially around the top, bottom, and corners. The
count also influenced accuracy. We can see that 2 strike counts were
very high in accuracy while “hitter-friendly” counts such as 3-0 and 3-1
were much lower. This suggests that umpires may expand their zone when
the count is in the hitters favor while keeping the zone the same when
there are 2 strikes.
