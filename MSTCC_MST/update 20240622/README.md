[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MSTvsHC_sp500** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'MST crypto'

Published in: 'Quantinar'

Description: 'Show the evolution of minimum spanning tree using varying time window of 10 cryptocurrencies.'

Submitted:  '23 June 2024'

Keywords: 
- 'Minimum Spaning Tree'
- 'Cryptocurrency'

Author: 
- 'Zijin Wang'
- 'Wolfgang Karl Härdle'
- 'Rui Ren'

```

- Step 1: Run "MSTCC_MST.R" to generate the dynamic minimum spanning trees 

- Step 2: RUN "timewindow_1_0_0.R" to generate dynamic subtitles for the windows spannings "FromTo.txt"

- Step 3: Run "write_subtitle.ipynb" to generate the suitable subtitles formats for the videos, "timewindow.srt"

- Step 4: Run `ffmpeg -i background.png -r 10 -i MSTCC_MST_%02d.png -filter_complex "[0]scale=770:350, overlay=120:10" my2_1.mp4 -y` in Terminal to generate the video with the dynamic minimum spanning trees, "my2_1.mp4"

- Step 5: Run `ffmpeg -i my2_1.mp4 -vf "subtitles=timewindow.srt" -y my2_2.mp4` in Terminal to generate the final video with the subtitles, "my2_2.mp4"   