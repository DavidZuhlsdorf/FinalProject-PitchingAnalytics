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
- `description`: umpire call description  
- `abs_call`,`ump_call`,`correct_call`: Created variables

The data was filtered to only include taken pitches that were called
either a ball or a called strike. I then created a simplified automated
strike zone based on pitch location. A pitch was classified as a strike
if it crossed within the horizontal strike zone and fell between the
batter’s individual strike zone height.

``` r
# Need to make the strike zone and establish what is a strike or ball
data$plate_x <- as.numeric(data$plate_x)
data$plate_z <- as.numeric(data$plate_z)
clean_data <- data %>%
  filter(description %in% c("ball","called_strike")) %>%
  mutate(ump_call = if_else(description == "called_strike","Strike","Ball"),
         abs_call = if_else(plate_x >= -0.83 & plate_x <= 0.83 &
                              plate_z >= sz_bot & plate_z <= sz_top,
                            "Strike",
                            "Ball"),
         correct_call = if_else(ump_call == abs_call,"Yes","No"),
         z_norm = (plate_z - sz_bot) / (sz_top - sz_bot),
         count = paste(balls,strikes,sep = "-"),
         two_strike = ifelse(strikes == 2, "Two strikes","Fewer than two strikes"),
         full_count = ifelse(balls == 3 & strikes == 2, "Yes","No"),
         late_inning = ifelse(inning >= 7,"Yes","No"),
         run_diff = abs(home_score-away_score),
         close_game = ifelse(run_diff <= 3, "Yes","No"),
         high_lev = ifelse(late_inning =="Yes" & close_game == "Yes","High leverage situation","Not high leverage situation")
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

<div class="plotly html-widget html-fill-item" id="htmlwidget-bff4db9c16c4bca9e3b7" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-bff4db9c16c4bca9e3b7">{"x":{"data":[{"orientation":"v","width":0.90000000000000013,"base":0,"x":[2],"y":[85.454545454545453],"text":"round(accuracy, 2): 85.45","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(68,1,84,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[4],"y":[91.472868217054256],"text":"round(accuracy, 2): 91.47","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(90,189,109,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[7],"y":[92.708333333333343],"text":"round(accuracy, 2): 92.71","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(156,215,74,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[3],"y":[93.3085501858736],"text":"round(accuracy, 2): 93.31","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(202,223,60,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[5],"y":[93.333333333333329],"text":"round(accuracy, 2): 93.33","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(204,223,60,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.89999999999999991,"base":0,"x":[1],"y":[93.589743589743591],"text":"round(accuracy, 2): 93.59","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(222,226,53,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[6],"y":[94.029850746268664],"text":"round(accuracy, 2): 94.03","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(253,231,37,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,0.40000000000000002,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996,7.5999999999999996],"y":[94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006,94.150000000000006],"text":"","type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","name":"2025 Average Accuracy: 94.15%","legendgroup":"2025 Average Accuracy: 94.15%","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1],"y":[0],"name":"4db8b0c6cd49f9b74557cec6f7c0e67c","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"skip","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#440154"],[0.0033444816053511371,"#440355"],[0.0066889632107022742,"#440456"],[0.010033444816053411,"#440656"],[0.013377926421404548,"#450857"],[0.016722408026755686,"#450958"],[0.020066889632106823,"#450B59"],[0.02341137123745796,"#450D5A"],[0.026755852842809097,"#450E5B"],[0.030100334448160238,"#45105B"],[0.033444816053511371,"#45115C"],[0.036789297658862512,"#45135D"],[0.040133779264213645,"#45145E"],[0.043478260869564786,"#45155F"],[0.04682274247491592,"#451760"],[0.05016722408026706,"#451860"],[0.053511705685618194,"#451961"],[0.056856187290969334,"#461A62"],[0.060200668896320475,"#461B63"],[0.063545150501671616,"#461D64"],[0.066889632107022742,"#461E65"],[0.070234113712373883,"#461F65"],[0.073578595317725023,"#462066"],[0.076923076923076164,"#462167"],[0.080267558528427291,"#462268"],[0.083612040133778431,"#462369"],[0.086956521739131223,"#46246A"],[0.090301003344482364,"#46256B"],[0.093645484949833505,"#46266B"],[0.096989966555184645,"#46276C"],[0.10033444816053577,"#46286D"],[0.10367892976588691,"#46296E"],[0.10702341137123805,"#462A6F"],[0.11036789297658919,"#452B70"],[0.11371237458194032,"#452C70"],[0.11705685618729146,"#452D71"],[0.1204013377926426,"#452E72"],[0.12374581939799374,"#452F73"],[0.12709030100334487,"#453074"],[0.13043478260869601,"#453175"],[0.13377926421404715,"#453276"],[0.13712374581939829,"#453377"],[0.14046822742474943,"#453477"],[0.14381270903010057,"#443578"],[0.14715719063545171,"#443679"],[0.15050167224080283,"#44367A"],[0.15384615384615397,"#44377B"],[0.15719063545150511,"#44387C"],[0.16053511705685625,"#44397D"],[0.16387959866220739,"#443A7D"],[0.16722408026755853,"#433B7E"],[0.17056856187290967,"#433C7F"],[0.17391304347826081,"#433D80"],[0.17725752508361192,"#433E81"],[0.18060200668896306,"#433F82"],[0.1839464882943142,"#424083"],[0.18729096989966534,"#424184"],[0.19063545150501648,"#424185"],[0.19397993311036762,"#424285"],[0.19732441471571877,"#414386"],[0.20066889632106991,"#414487"],[0.20401337792642105,"#414587"],[0.20735785953177216,"#414687"],[0.2107023411371233,"#414787"],[0.21404682274247444,"#414888"],[0.21739130434782558,"#404988"],[0.22073578595317672,"#404A88"],[0.22408026755852786,"#404A88"],[0.227424749163879,"#404B88"],[0.23076923076923014,"#404C88"],[0.23411371237458126,"#404D88"],[0.2374581939799324,"#404E88"],[0.24080267558528354,"#3F4F89"],[0.24414715719063468,"#3F5089"],[0.24749163879598582,"#3F5189"],[0.25083612040133862,"#3F5289"],[0.25418060200668974,"#3F5289"],[0.25752508361204091,"#3E5389"],[0.26086956521739202,"#3E5489"],[0.26421404682274319,"#3E5589"],[0.2675585284280943,"#3E568A"],[0.27090301003344541,"#3D578A"],[0.27424749163879658,"#3D588A"],[0.27759197324414769,"#3D598A"],[0.28093645484949886,"#3D598A"],[0.28428093645484998,"#3C5A8A"],[0.28762541806020114,"#3C5B8A"],[0.29096989966555226,"#3C5C8A"],[0.29431438127090342,"#3B5D8A"],[0.29765886287625454,"#3B5E8B"],[0.30100334448160565,"#3B5F8B"],[0.30434782608695682,"#3A5F8B"],[0.30769230769230793,"#3A608B"],[0.3110367892976591,"#3A618B"],[0.31438127090301021,"#39628B"],[0.31772575250836138,"#39638B"],[0.32107023411371249,"#38648B"],[0.32441471571906361,"#38658C"],[0.32775919732441477,"#38658C"],[0.33110367892976589,"#37668C"],[0.33444816053511706,"#37678C"],[0.33779264214046817,"#36688C"],[0.34113712374581934,"#36698C"],[0.34448160535117045,"#356A8C"],[0.34782608695652162,"#356B8C"],[0.35117056856187273,"#346B8C"],[0.35451505016722384,"#346C8D"],[0.35785953177257501,"#336D8D"],[0.36120401337792613,"#326E8D"],[0.36454849498327729,"#326F8D"],[0.36789297658862841,"#31708D"],[0.37123745819397957,"#31718D"],[0.37458193979933069,"#30718D"],[0.37792642140468186,"#2F728D"],[0.38127090301003297,"#2F738D"],[0.38461538461538408,"#2E748E"],[0.38795986622073525,"#2D758E"],[0.39130434782608636,"#2C768E"],[0.39464882943143753,"#2B778E"],[0.39799331103678864,"#2B778E"],[0.40133779264213981,"#2A788E"],[0.40468227424749093,"#2A798E"],[0.40802675585284209,"#2A7A8E"],[0.41137123745819321,"#2B7B8E"],[0.41471571906354432,"#2B7B8D"],[0.41806020066889549,"#2B7C8D"],[0.42140468227424827,"#2B7D8D"],[0.42474916387959943,"#2B7E8D"],[0.42809364548495055,"#2B7F8D"],[0.43143812709030166,"#2B7F8D"],[0.43478260869565283,"#2B808D"],[0.43812709030100394,"#2B818C"],[0.44147157190635511,"#2B828C"],[0.44481605351170622,"#2B838C"],[0.44816053511705739,"#2C838C"],[0.4515050167224085,"#2C848C"],[0.45484949832775967,"#2C858C"],[0.45819397993311078,"#2C868B"],[0.4615384615384619,"#2C878B"],[0.46488294314381307,"#2C878B"],[0.46822742474916418,"#2C888B"],[0.47157190635451535,"#2C898B"],[0.47491638795986646,"#2C8A8B"],[0.47826086956521763,"#2C8B8B"],[0.48160535117056874,"#2B8B8A"],[0.48494983277591991,"#2B8C8A"],[0.48829431438127102,"#2B8D8A"],[0.49163879598662213,"#2B8E8A"],[0.4949832775919733,"#2B8F8A"],[0.49832775919732442,"#2B8F8A"],[0.50167224080267558,"#2B9089"],[0.50501672240802675,"#2B9189"],[0.50836120401337781,"#2B9289"],[0.51170568561872898,"#2B9389"],[0.51505016722408015,"#2A9389"],[0.5183946488294312,"#2A9489"],[0.52173913043478237,"#2A9588"],[0.52508361204013354,"#2A9688"],[0.52842809364548471,"#2A9788"],[0.53177257525083577,"#2A9788"],[0.53511705685618693,"#299888"],[0.5384615384615381,"#299987"],[0.54180602006688927,"#299A87"],[0.54515050167224033,"#299B87"],[0.5484949832775915,"#289B87"],[0.55183946488294267,"#289C87"],[0.55518394648829372,"#289D87"],[0.55852842809364489,"#279E86"],[0.56187290969899606,"#279F86"],[0.56521739130434723,"#27A086"],[0.56856187290969828,"#26A086"],[0.57190635451504945,"#26A186"],[0.57525083612040062,"#26A285"],[0.57859531772575168,"#25A385"],[0.58193979933110285,"#25A485"],[0.58528428093645568,"#24A485"],[0.58862876254180518,"#24A585"],[0.59197324414715791,"#23A684"],[0.59531772575250907,"#23A784"],[0.59866220735786024,"#22A884"],[0.6020066889632113,"#24A884"],[0.60535117056856247,"#27A983"],[0.60869565217391364,"#2AAA82"],[0.61204013377926481,"#2DAA81"],[0.61538461538461586,"#2FAB81"],[0.61872909698996703,"#32AC80"],[0.6220735785953182,"#34AC7F"],[0.62541806020066926,"#36AD7E"],[0.62876254180602043,"#38AE7E"],[0.63210702341137159,"#3AAE7D"],[0.63545150501672276,"#3CAF7C"],[0.63879598662207382,"#3EB07B"],[0.64214046822742499,"#40B07B"],[0.64548494983277616,"#42B17A"],[0.64882943143812721,"#43B279"],[0.65217391304347838,"#45B278"],[0.65551839464882955,"#47B378"],[0.65886287625418072,"#48B477"],[0.66220735785953178,"#4AB476"],[0.66555183946488294,"#4BB575"],[0.66889632107023411,"#4DB675"],[0.67224080267558528,"#4EB674"],[0.67558528428093634,"#50B773"],[0.67892976588628751,"#51B872"],[0.68227424749163867,"#53B971"],[0.68561872909698973,"#54B971"],[0.6889632107023409,"#55BA70"],[0.69230769230769207,"#57BB6F"],[0.69565217391304324,"#58BB6E"],[0.69899665551839429,"#59BC6D"],[0.70234113712374546,"#5BBD6C"],[0.70568561872909663,"#5CBD6C"],[0.70903010033444769,"#5DBE6B"],[0.71237458193979886,"#5EBF6A"],[0.71571906354515002,"#5FBF69"],[0.71906354515050119,"#61C068"],[0.72240802675585225,"#62C167"],[0.72575250836120342,"#63C166"],[0.72909698996655459,"#64C266"],[0.73244147157190576,"#65C365"],[0.73578595317725681,"#66C464"],[0.73913043478260798,"#67C463"],[0.74247491638795915,"#69C562"],[0.74581939799331021,"#6AC661"],[0.74916387959866304,"#6BC660"],[0.75250836120401254,"#6CC75F"],[0.75585284280936527,"#6DC85E"],[0.75919732441471643,"#6EC85D"],[0.7625418060200676,"#6FC95C"],[0.76588628762541877,"#70CA5B"],[0.76923076923076983,"#71CB5A"],[0.772575250836121,"#72CB59"],[0.77591973244147217,"#73CC58"],[0.77926421404682333,"#74CD57"],[0.78260869565217439,"#75CD56"],[0.78595317725752556,"#76CE55"],[0.78929765886287673,"#77CF54"],[0.79264214046822778,"#78CF53"],[0.79598662207357895,"#79D052"],[0.79933110367893012,"#7AD151"],[0.80267558528428129,"#7CD151"],[0.80602006688963235,"#7FD250"],[0.80936454849498352,"#81D250"],[0.81270903010033468,"#84D34F"],[0.81605351170568574,"#87D34F"],[0.81939799331103691,"#89D34E"],[0.82274247491638808,"#8CD44D"],[0.82608695652173925,"#8ED44D"],[0.8294314381270903,"#91D54C"],[0.83277591973244147,"#93D54C"],[0.83612040133779264,"#95D54B"],[0.8394648829431437,"#98D64B"],[0.84280936454849487,"#9AD64A"],[0.84615384615384603,"#9DD74A"],[0.8494983277591972,"#9FD749"],[0.85284280936454826,"#A1D748"],[0.85618729096989943,"#A3D848"],[0.8595317725752506,"#A6D847"],[0.86287625418060176,"#A8D947"],[0.86622073578595282,"#AAD946"],[0.86956521739130399,"#ACD946"],[0.87290969899665516,"#AFDA45"],[0.87625418060200622,"#B1DA44"],[0.87959866220735738,"#B3DB44"],[0.88294314381270855,"#B5DB43"],[0.88628762541805972,"#B7DB42"],[0.88963210702341078,"#BADC42"],[0.89297658862876195,"#BCDC41"],[0.89632107023411312,"#BEDC40"],[0.89966555183946417,"#C0DD40"],[0.90301003344481534,"#C2DD3F"],[0.90635451505016651,"#C4DE3E"],[0.90969899665551768,"#C6DE3E"],[0.91304347826086873,"#C8DE3D"],[0.9163879598662199,"#CBDF3C"],[0.91973244147157274,"#CDDF3B"],[0.92307692307692224,"#CFDF3B"],[0.92642140468227496,"#D1E03A"],[0.92976588628762613,"#D3E039"],[0.9331103678929773,"#D5E038"],[0.93645484949832836,"#D7E138"],[0.93979933110367952,"#D9E137"],[0.94314381270903069,"#DBE136"],[0.94648829431438175,"#DDE235"],[0.94983277591973292,"#DFE234"],[0.95317725752508409,"#E1E233"],[0.95652173913043526,"#E3E333"],[0.95986622073578631,"#E5E332"],[0.96321070234113748,"#E7E331"],[0.96655518394648865,"#E9E430"],[0.96989966555183982,"#EBE42F"],[0.97324414715719088,"#EDE42E"],[0.97658862876254204,"#EFE52D"],[0.97993311036789321,"#F1E52C"],[0.98327759197324427,"#F3E52B"],[0.98662207357859544,"#F5E62A"],[0.98996655518394661,"#F7E629"],[0.99331103678929777,"#F9E627"],[0.99665551839464883,"#FBE726"],[1,"#FDE725"]],"colorbar":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"thickness":23.039999999999996,"title":"accuracy","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"tickmode":"array","ticktext":["86","88","90","92","94"],"tickvals":[0.065062236286919914,0.29751265822784795,0.52996308016877602,0.76241350210970404,0.99486392405063195],"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"ticklen":2,"len":0.5,"yanchor":"top","y":1}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":40.840182648401829,"r":7.3059360730593621,"b":37.260273972602747,"l":37.260273972602747},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Umpire Accuracy by World Series Game","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,7.5999999999999996],"tickmode":"array","ticktext":["2025-10-24","2025-10-25","2025-10-27","2025-10-28","2025-10-29","2025-10-31","2025-11-01"],"tickvals":[1,2,3,4.0000000000000009,5,6,7],"categoryorder":"array","categoryarray":["2025-10-24","2025-10-25","2025-10-27","2025-10-28","2025-10-29","2025-10-31","2025-11-01"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"Game Date","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4.7075000000000005,98.857500000000002],"tickmode":"array","ticktext":["0","25","50","75"],"tickvals":[0,25.000000000000004,49.999999999999993,75],"categoryorder":"array","categoryarray":["0","25","50","75"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"Accuracy (%)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"y":0.5,"yanchor":"top","title":{"text":"<br />accuracy","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2a402ea0221b":{"x":{},"y":{},"fill":{},"label":{},"type":"bar"},"2a40483218c9":{"yintercept":{},"linetype":{}}},"cur_data":"2a402ea0221b","visdat":{"2a402ea0221b":["function (y) ","x"],"2a40483218c9":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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

<div class="plotly html-widget html-fill-item" id="htmlwidget-0318f7a47dae6423facd" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-0318f7a47dae6423facd">{"x":{"data":[{"x":[0,91.21447028423772,null,0,91.489361702127653,null,0,96.92307692307692,null,0,93.233082706766908,null,0,95.833333333333343,null,0,98.82352941176471,null,0,92.307692307692307,null,0,81.132075471698116,null,0,93.103448275862064,null,0,91.304347826086953,null,0,86.666666666666671,null,0,93.333333333333329],"y":[3,3,null,5,5,null,11,11,null,8,8,null,10,10,null,12,12,null,6,6,null,1,1,null,7,7,null,4,4,null,2,2,null,9,9],"text":["accuracy: 91.21447","accuracy: 91.21447",null,"accuracy: 91.48936","accuracy: 91.48936",null,"accuracy: 96.92308","accuracy: 96.92308",null,"accuracy: 93.23308","accuracy: 93.23308",null,"accuracy: 95.83333","accuracy: 95.83333",null,"accuracy: 98.82353","accuracy: 98.82353",null,"accuracy: 92.30769","accuracy: 92.30769",null,"accuracy: 81.13208","accuracy: 81.13208",null,"accuracy: 93.10345","accuracy: 93.10345",null,"accuracy: 91.30435","accuracy: 91.30435",null,"accuracy: 86.66667","accuracy: 86.66667",null,"accuracy: 93.33333","accuracy: 93.33333"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(70,130,180,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[91.21447028423772,91.489361702127653,96.92307692307692,93.233082706766908,95.833333333333343,98.82352941176471,92.307692307692307,81.132075471698116,93.103448275862064,91.304347826086953,86.666666666666671,93.333333333333329],"y":[3,5,11,8,10,12,6,1,7,4,2,9],"text":["accuracy: 91.21447","accuracy: 91.48936","accuracy: 96.92308","accuracy: 93.23308","accuracy: 95.83333","accuracy: 98.82353","accuracy: 92.30769","accuracy: 81.13208","accuracy: 93.10345","accuracy: 91.30435","accuracy: 86.66667","accuracy: 93.33333"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,0,0,1)","opacity":1,"size":11.338582677165356,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":47.61477791614778,"r":9.2984640929846396,"b":47.422166874221666,"l":54.860938148609378},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"title":{"text":"Umpire Accuracy by Count","font":{"color":"rgba(0,0,0,1)","family":"","size":22.316313823163139},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4.9411764705882355,103.76470588235294],"tickmode":"array","ticktext":["0","25","50","75","100"],"tickvals":[0,24.999999999999996,50,75,100],"categoryorder":"array","categoryarray":["0","25","50","75","100"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":14.877542548775427},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.8453149175440583,"zeroline":false,"anchor":"y","title":{"text":"Accuracy (%)","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,12.6],"tickmode":"array","ticktext":["2-1","3-1","0-0","3-0","0-1","2-0","2-2","1-0","3-2","1-1","0-2","1-2"],"tickvals":[1,2,3,4.0000000000000009,5,6,7.0000000000000009,8,9,10,11,12],"categoryorder":"array","categoryarray":["2-1","3-1","0-0","3-0","0-1","2-0","2-2","1-0","3-2","1-1","0-2","1-2"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":14.87754254877543},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.8453149175440583,"zeroline":false,"anchor":"x","title":{"text":"Count","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}},"hoverformat":".2f"},"shapes":[],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.877542548775427}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2a4029b797a":{"x":{},"y":{},"label":{},"xend":{},"yend":{},"type":"scatter"},"2a40d904621":{"x":{},"y":{},"label":{}}},"cur_data":"2a4029b797a","visdat":{"2a4029b797a":["function (y) ","x"],"2a40d904621":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

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
