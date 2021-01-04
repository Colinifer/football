README
================

## Setup/Installation

Most of this repo relies on a personal package I created and
nflfastR(‘<https://github.com/mrcaseb/nflfastR>’) but some simple
setup and most of the scripts with automatically render visualizations
from NFL data.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

    #> Loading required package: usethis
    #> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
    #> ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    #> ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    #> ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    #> ✓ readr   1.4.0     ✓ forcats 0.5.0
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> x dplyr::filter() masks stats::filter()
    #> x dplyr::lag()    masks stats::lag()
    #> 
    #> Attaching package: 'arrow'
    #> The following object is masked from 'package:utils':
    #> 
    #>     timestamp
    #> 
    #> Attaching package: 'pander'
    #> The following object is masked from 'package:shiny':
    #> 
    #>     p
    #> Loading required package: future
    #> 
    #> Attaching package: 'glue'
    #> The following object is masked from 'package:dplyr':
    #> 
    #>     collapse
    #> 
    #> Attaching package: 'jsonlite'
    #> The following object is masked from 'package:shiny':
    #> 
    #>     validate
    #> The following object is masked from 'package:purrr':
    #> 
    #>     flatten
    #> 
    #> Attaching package: 'DT'
    #> The following objects are masked from 'package:shiny':
    #> 
    #>     dataTableOutput, renderDataTable
    #> 
    #> Attaching package: 'ggpmisc'
    #> The following object is masked from 'package:ggplot2':
    #> 
    #>     annotate
    #> 
    #> Attaching package: 'cowplot'
    #> The following object is masked from 'package:ggthemes':
    #> 
    #>     theme_map
    #> The following object is masked from 'package:ggimage':
    #> 
    #>     theme_nothing
    #> 
    #> Attaching package: 'gridExtra'
    #> The following object is masked from 'package:dplyr':
    #> 
    #>     combine
    #> Registering fonts with R
    #> Loading required package: viridisLite
    #> 
    #> Attaching package: 'RCurl'
    #> The following object is masked from 'package:tidyr':
    #> 
    #>     complete
    #> 
    #> Attaching package: 'pracma'
    #> The following objects are masked from 'package:tictoc':
    #> 
    #>     tic, toc
    #> The following object is masked from 'package:arrow':
    #> 
    #>     null
    #> The following object is masked from 'package:purrr':
    #> 
    #>     cross
    #> [[1]]
    #> [1] "devtools"  "usethis"   "stats"     "graphics"  "grDevices" "utils"    
    #> [7] "datasets"  "methods"   "base"     
    #> 
    #> [[2]]
    #>  [1] "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"    
    #>  [7] "tibble"    "ggplot2"   "tidyverse" "devtools"  "usethis"   "stats"    
    #> [13] "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
    #> 
    #> [[3]]
    #>  [1] "nflfastR"  "forcats"   "stringr"   "dplyr"     "purrr"     "readr"    
    #>  [7] "tidyr"     "tibble"    "ggplot2"   "tidyverse" "devtools"  "usethis"  
    #> [13] "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"  
    #> [19] "base"     
    #> 
    #> [[4]]
    #>  [1] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #>  [6] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [11] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [16] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[5]]
    #>  [1] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #>  [6] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [11] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [16] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [21] "base"       
    #> 
    #> [[6]]
    #>  [1] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #>  [6] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [11] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [16] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [21] "methods"     "base"       
    #> 
    #> [[7]]
    #>  [1] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #>  [6] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [11] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [16] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [21] "datasets"    "methods"     "base"       
    #> 
    #> [[8]]
    #>  [1] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #>  [6] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [11] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [16] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [21] "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[9]]
    #>  [1] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #>  [6] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [11] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [16] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [21] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[10]]
    #>  [1] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #>  [6] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [11] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [16] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [21] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [26] "base"       
    #> 
    #> [[11]]
    #>  [1] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #>  [6] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [11] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [16] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [21] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [26] "methods"     "base"       
    #> 
    #> [[12]]
    #>  [1] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #>  [6] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [11] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [16] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [21] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [26] "datasets"    "methods"     "base"       
    #> 
    #> [[13]]
    #>  [1] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #>  [6] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [11] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [16] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [21] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [26] "datasets"    "methods"     "base"       
    #> 
    #> [[14]]
    #>  [1] "pander"      "httr"        "distill"     "shiny"       "arrow"      
    #>  [6] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #> [11] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [16] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [21] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [26] "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[15]]
    #>  [1] "furrr"       "future"      "pander"      "httr"        "distill"    
    #>  [6] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #> [11] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [16] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [21] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [26] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [31] "base"       
    #> 
    #> [[16]]
    #>  [1] "na.tools"    "furrr"       "future"      "pander"      "httr"       
    #>  [6] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #> [11] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [16] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [21] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [26] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [31] "methods"     "base"       
    #> 
    #> [[17]]
    #>  [1] "ggimage"     "na.tools"    "furrr"       "future"      "pander"     
    #>  [6] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #> [11] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [16] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [21] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [26] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [31] "datasets"    "methods"     "base"       
    #> 
    #> [[18]]
    #>  [1] "teamcolors"  "ggimage"     "na.tools"    "furrr"       "future"     
    #>  [6] "pander"      "httr"        "distill"     "shiny"       "arrow"      
    #> [11] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #> [16] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [21] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [26] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [31] "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[19]]
    #>  [1] "glue"        "teamcolors"  "ggimage"     "na.tools"    "furrr"      
    #>  [6] "future"      "pander"      "httr"        "distill"     "shiny"      
    #> [11] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #> [16] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [21] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [26] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [31] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[20]]
    #>  [1] "glue"        "teamcolors"  "ggimage"     "na.tools"    "furrr"      
    #>  [6] "future"      "pander"      "httr"        "distill"     "shiny"      
    #> [11] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #> [16] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [21] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [26] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [31] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[21]]
    #>  [1] "jsonlite"    "glue"        "teamcolors"  "ggimage"     "na.tools"   
    #>  [6] "furrr"       "future"      "pander"      "httr"        "distill"    
    #> [11] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #> [16] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [21] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [26] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [31] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [36] "base"       
    #> 
    #> [[22]]
    #>  [1] "tictoc"      "jsonlite"    "glue"        "teamcolors"  "ggimage"    
    #>  [6] "na.tools"    "furrr"       "future"      "pander"      "httr"       
    #> [11] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #> [16] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [21] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [26] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [31] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [36] "methods"     "base"       
    #> 
    #> [[23]]
    #>  [1] "animation"   "tictoc"      "jsonlite"    "glue"        "teamcolors" 
    #>  [6] "ggimage"     "na.tools"    "furrr"       "future"      "pander"     
    #> [11] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #> [16] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [21] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [26] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [31] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [36] "datasets"    "methods"     "base"       
    #> 
    #> [[24]]
    #>  [1] "gt"          "animation"   "tictoc"      "jsonlite"    "glue"       
    #>  [6] "teamcolors"  "ggimage"     "na.tools"    "furrr"       "future"     
    #> [11] "pander"      "httr"        "distill"     "shiny"       "arrow"      
    #> [16] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #> [21] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [26] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [31] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [36] "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[25]]
    #>  [1] "reactable"   "gt"          "animation"   "tictoc"      "jsonlite"   
    #>  [6] "glue"        "teamcolors"  "ggimage"     "na.tools"    "furrr"      
    #> [11] "future"      "pander"      "httr"        "distill"     "shiny"      
    #> [16] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #> [21] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [26] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [31] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [36] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[26]]
    #>  [1] "png"         "reactable"   "gt"          "animation"   "tictoc"     
    #>  [6] "jsonlite"    "glue"        "teamcolors"  "ggimage"     "na.tools"   
    #> [11] "furrr"       "future"      "pander"      "httr"        "distill"    
    #> [16] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #> [21] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [26] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [31] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [36] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [41] "base"       
    #> 
    #> [[27]]
    #>  [1] "DT"          "png"         "reactable"   "gt"          "animation"  
    #>  [6] "tictoc"      "jsonlite"    "glue"        "teamcolors"  "ggimage"    
    #> [11] "na.tools"    "furrr"       "future"      "pander"      "httr"       
    #> [16] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #> [21] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [26] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [31] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [36] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [41] "methods"     "base"       
    #> 
    #> [[28]]
    #>  [1] "ggthemes"    "DT"          "png"         "reactable"   "gt"         
    #>  [6] "animation"   "tictoc"      "jsonlite"    "glue"        "teamcolors" 
    #> [11] "ggimage"     "na.tools"    "furrr"       "future"      "pander"     
    #> [16] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #> [21] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [26] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [31] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [36] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [41] "datasets"    "methods"     "base"       
    #> 
    #> [[29]]
    #>  [1] "ggforce"     "ggthemes"    "DT"          "png"         "reactable"  
    #>  [6] "gt"          "animation"   "tictoc"      "jsonlite"    "glue"       
    #> [11] "teamcolors"  "ggimage"     "na.tools"    "furrr"       "future"     
    #> [16] "pander"      "httr"        "distill"     "shiny"       "arrow"      
    #> [21] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #> [26] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [31] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [36] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [41] "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[30]]
    #>  [1] "ggridges"    "ggforce"     "ggthemes"    "DT"          "png"        
    #>  [6] "reactable"   "gt"          "animation"   "tictoc"      "jsonlite"   
    #> [11] "glue"        "teamcolors"  "ggimage"     "na.tools"    "furrr"      
    #> [16] "future"      "pander"      "httr"        "distill"     "shiny"      
    #> [21] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #> [26] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [31] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [36] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [41] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[31]]
    #>  [1] "ggrepel"     "ggridges"    "ggforce"     "ggthemes"    "DT"         
    #>  [6] "png"         "reactable"   "gt"          "animation"   "tictoc"     
    #> [11] "jsonlite"    "glue"        "teamcolors"  "ggimage"     "na.tools"   
    #> [16] "furrr"       "future"      "pander"      "httr"        "distill"    
    #> [21] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #> [26] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [31] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [36] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [41] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [46] "base"       
    #> 
    #> [[32]]
    #>  [1] "ggpmisc"     "ggrepel"     "ggridges"    "ggforce"     "ggthemes"   
    #>  [6] "DT"          "png"         "reactable"   "gt"          "animation"  
    #> [11] "tictoc"      "jsonlite"    "glue"        "teamcolors"  "ggimage"    
    #> [16] "na.tools"    "furrr"       "future"      "pander"      "httr"       
    #> [21] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #> [26] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [31] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [36] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [41] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [46] "methods"     "base"       
    #> 
    #> [[33]]
    #>  [1] "ggbeeswarm"  "ggpmisc"     "ggrepel"     "ggridges"    "ggforce"    
    #>  [6] "ggthemes"    "DT"          "png"         "reactable"   "gt"         
    #> [11] "animation"   "tictoc"      "jsonlite"    "glue"        "teamcolors" 
    #> [16] "ggimage"     "na.tools"    "furrr"       "future"      "pander"     
    #> [21] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #> [26] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [31] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [36] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [41] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [46] "datasets"    "methods"     "base"       
    #> 
    #> [[34]]
    #>  [1] "cowplot"     "ggbeeswarm"  "ggpmisc"     "ggrepel"     "ggridges"   
    #>  [6] "ggforce"     "ggthemes"    "DT"          "png"         "reactable"  
    #> [11] "gt"          "animation"   "tictoc"      "jsonlite"    "glue"       
    #> [16] "teamcolors"  "ggimage"     "na.tools"    "furrr"       "future"     
    #> [21] "pander"      "httr"        "distill"     "shiny"       "arrow"      
    #> [26] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #> [31] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [36] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [41] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [46] "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[35]]
    #>  [1] "gridExtra"   "cowplot"     "ggbeeswarm"  "ggpmisc"     "ggrepel"    
    #>  [6] "ggridges"    "ggforce"     "ggthemes"    "DT"          "png"        
    #> [11] "reactable"   "gt"          "animation"   "tictoc"      "jsonlite"   
    #> [16] "glue"        "teamcolors"  "ggimage"     "na.tools"    "furrr"      
    #> [21] "future"      "pander"      "httr"        "distill"     "shiny"      
    #> [26] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #> [31] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [36] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [41] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [46] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[36]]
    #>  [1] "grid"        "gridExtra"   "cowplot"     "ggbeeswarm"  "ggpmisc"    
    #>  [6] "ggrepel"     "ggridges"    "ggforce"     "ggthemes"    "DT"         
    #> [11] "png"         "reactable"   "gt"          "animation"   "tictoc"     
    #> [16] "jsonlite"    "glue"        "teamcolors"  "ggimage"     "na.tools"   
    #> [21] "furrr"       "future"      "pander"      "httr"        "distill"    
    #> [26] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #> [31] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [36] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [41] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [46] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [51] "base"       
    #> 
    #> [[37]]
    #>  [1] "extrafont"   "grid"        "gridExtra"   "cowplot"     "ggbeeswarm" 
    #>  [6] "ggpmisc"     "ggrepel"     "ggridges"    "ggforce"     "ggthemes"   
    #> [11] "DT"          "png"         "reactable"   "gt"          "animation"  
    #> [16] "tictoc"      "jsonlite"    "glue"        "teamcolors"  "ggimage"    
    #> [21] "na.tools"    "furrr"       "future"      "pander"      "httr"       
    #> [26] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #> [31] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [36] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [41] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [46] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [51] "methods"     "base"       
    #> 
    #> [[38]]
    #>  [1] "shadowtext"  "extrafont"   "grid"        "gridExtra"   "cowplot"    
    #>  [6] "ggbeeswarm"  "ggpmisc"     "ggrepel"     "ggridges"    "ggforce"    
    #> [11] "ggthemes"    "DT"          "png"         "reactable"   "gt"         
    #> [16] "animation"   "tictoc"      "jsonlite"    "glue"        "teamcolors" 
    #> [21] "ggimage"     "na.tools"    "furrr"       "future"      "pander"     
    #> [26] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #> [31] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [36] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [41] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [46] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [51] "datasets"    "methods"     "base"       
    #> 
    #> [[39]]
    #>  [1] "viridis"     "viridisLite" "shadowtext"  "extrafont"   "grid"       
    #>  [6] "gridExtra"   "cowplot"     "ggbeeswarm"  "ggpmisc"     "ggrepel"    
    #> [11] "ggridges"    "ggforce"     "ggthemes"    "DT"          "png"        
    #> [16] "reactable"   "gt"          "animation"   "tictoc"      "jsonlite"   
    #> [21] "glue"        "teamcolors"  "ggimage"     "na.tools"    "furrr"      
    #> [26] "future"      "pander"      "httr"        "distill"     "shiny"      
    #> [31] "arrow"       "RMariaDB"    "odbc"        "DBI"         "espnscrapeR"
    #> [36] "gsisdecoder" "nflfastR"    "forcats"     "stringr"     "dplyr"      
    #> [41] "purrr"       "readr"       "tidyr"       "tibble"      "ggplot2"    
    #> [46] "tidyverse"   "devtools"    "usethis"     "stats"       "graphics"   
    #> [51] "grDevices"   "utils"       "datasets"    "methods"     "base"       
    #> 
    #> [[40]]
    #>  [1] "tidytext"    "viridis"     "viridisLite" "shadowtext"  "extrafont"  
    #>  [6] "grid"        "gridExtra"   "cowplot"     "ggbeeswarm"  "ggpmisc"    
    #> [11] "ggrepel"     "ggridges"    "ggforce"     "ggthemes"    "DT"         
    #> [16] "png"         "reactable"   "gt"          "animation"   "tictoc"     
    #> [21] "jsonlite"    "glue"        "teamcolors"  "ggimage"     "na.tools"   
    #> [26] "furrr"       "future"      "pander"      "httr"        "distill"    
    #> [31] "shiny"       "arrow"       "RMariaDB"    "odbc"        "DBI"        
    #> [36] "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"     "stringr"    
    #> [41] "dplyr"       "purrr"       "readr"       "tidyr"       "tibble"     
    #> [46] "ggplot2"     "tidyverse"   "devtools"    "usethis"     "stats"      
    #> [51] "graphics"    "grDevices"   "utils"       "datasets"    "methods"    
    #> [56] "base"       
    #> 
    #> [[41]]
    #>  [1] "RCurl"       "tidytext"    "viridis"     "viridisLite" "shadowtext" 
    #>  [6] "extrafont"   "grid"        "gridExtra"   "cowplot"     "ggbeeswarm" 
    #> [11] "ggpmisc"     "ggrepel"     "ggridges"    "ggforce"     "ggthemes"   
    #> [16] "DT"          "png"         "reactable"   "gt"          "animation"  
    #> [21] "tictoc"      "jsonlite"    "glue"        "teamcolors"  "ggimage"    
    #> [26] "na.tools"    "furrr"       "future"      "pander"      "httr"       
    #> [31] "distill"     "shiny"       "arrow"       "RMariaDB"    "odbc"       
    #> [36] "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"    "forcats"    
    #> [41] "stringr"     "dplyr"       "purrr"       "readr"       "tidyr"      
    #> [46] "tibble"      "ggplot2"     "tidyverse"   "devtools"    "usethis"    
    #> [51] "stats"       "graphics"    "grDevices"   "utils"       "datasets"   
    #> [56] "methods"     "base"       
    #> 
    #> [[42]]
    #>  [1] "pracma"      "RCurl"       "tidytext"    "viridis"     "viridisLite"
    #>  [6] "shadowtext"  "extrafont"   "grid"        "gridExtra"   "cowplot"    
    #> [11] "ggbeeswarm"  "ggpmisc"     "ggrepel"     "ggridges"    "ggforce"    
    #> [16] "ggthemes"    "DT"          "png"         "reactable"   "gt"         
    #> [21] "animation"   "tictoc"      "jsonlite"    "glue"        "teamcolors" 
    #> [26] "ggimage"     "na.tools"    "furrr"       "future"      "pander"     
    #> [31] "httr"        "distill"     "shiny"       "arrow"       "RMariaDB"   
    #> [36] "odbc"        "DBI"         "espnscrapeR" "gsisdecoder" "nflfastR"   
    #> [41] "forcats"     "stringr"     "dplyr"       "purrr"       "readr"      
    #> [46] "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "devtools"   
    #> [51] "usethis"     "stats"       "graphics"    "grDevices"   "utils"      
    #> [56] "datasets"    "methods"     "base"       
    #> 
    #> [[43]]
    #>  [1] "initR"       "pracma"      "RCurl"       "tidytext"    "viridis"    
    #>  [6] "viridisLite" "shadowtext"  "extrafont"   "grid"        "gridExtra"  
    #> [11] "cowplot"     "ggbeeswarm"  "ggpmisc"     "ggrepel"     "ggridges"   
    #> [16] "ggforce"     "ggthemes"    "DT"          "png"         "reactable"  
    #> [21] "gt"          "animation"   "tictoc"      "jsonlite"    "glue"       
    #> [26] "teamcolors"  "ggimage"     "na.tools"    "furrr"       "future"     
    #> [31] "pander"      "httr"        "distill"     "shiny"       "arrow"      
    #> [36] "RMariaDB"    "odbc"        "DBI"         "espnscrapeR" "gsisdecoder"
    #> [41] "nflfastR"    "forcats"     "stringr"     "dplyr"       "purrr"      
    #> [46] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"  
    #> [51] "devtools"    "usethis"     "stats"       "graphics"    "grDevices"  
    #> [56] "utils"       "datasets"    "methods"     "base"

## Create standard session objects/variables

The functions below help to load standard data and create standard
environment variables used in other scripts.

``` r
fx.setdir(proj_name)
#> [1] "Goose (iMac) is ready for some football"

year <- fx.get_year()

fx.get_sleeper_api_players()
# source("fantasy_football/ff_init.R")
fx.get_espn_players()

# nflfastR data
roster_df <-
  readRDS(
    url(
      'https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true'
    )
  ) %>%
  as_tibble()

schedule_df <- fast_scraper_schedules(seasons = year, pp = TRUE)
#> ℹ You have passed only 1 season(s) to parallel processing.
#>   Please note that the initiating process takes a few seconds
#>   and consider using `pp = FALSE` for a small number of seasons.

schedule_df %>% 
  saveRDS(glue('data/schedules/sched_{year}.rds'))
  
matchup_df <- schedule_df %>% 
  mutate(posteam = home_team,
         oppteam = away_team) %>%
  select(
    game_id,
    season,
    game_type,
    week,
    gameday,
    weekday,
    gametime,
    posteam,
    oppteam,
    away_team,
    home_team,
    away_score,
    home_score,
    home_result,
    stadium,
    location,
    roof,
    surface,
    old_game_id
  ) %>% rbind(
    schedule_df %>%
      mutate(posteam = away_team,
             oppteam = home_team) %>%
      select(
        game_id,
        season,
        game_type,
        week,
        gameday,
        weekday,
        gametime,
        posteam,
        oppteam,
        away_team,
        home_team,
        away_score,
        home_score,
        home_result,
        stadium,
        location,
        roof,
        surface,
        old_game_id
      )
  ) %>% arrange(old_game_id)

sr_games_df <- readRDS('data/schedules/sportradar/games_2020.rds')

# source('data/master_sr_pbp.R')
pbp_df <- readRDS(glue('data/pbp/play_by_play_{year}.rds'))
pbp_df %>% select(game_date) %>% arrange(game_date) %>%  unique() %>%  tail()
#> # A tibble: 6 x 1
#>   game_date 
#>   <chr>     
#> 1 2020-12-20
#> 2 2020-12-21
#> 3 2020-12-25
#> 4 2020-12-26
#> 5 2020-12-27
#> 6 2020-12-28
pbp_df %>% select(game_id) %>% unique() %>% tail()
#> # A tibble: 6 x 1
#>   game_id        
#>   <chr>          
#> 1 2020_16_IND_PIT
#> 2 2020_16_LA_SEA 
#> 3 2020_16_NYG_BAL
#> 4 2020_16_PHI_DAL
#> 5 2020_16_TEN_GB 
#> 6 2020_16_BUF_NE
```

Loading parquet/arrow files is fastest for multiple seasons of data.

``` r
part_ds <- open_dataset('data/part/sportradar', partitioning = 'year')
pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')
xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')
sr_pbp_df <- readRDS('data/pbp/sportradar/sr_pbp_2020.rds')
```

Load the custom/person GG theme for plots

    #> Linking to ImageMagick 6.9.11.32
    #> Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
    #> Disabled features: fftw, ghostscript, x11
    #> 
    #> Attaching package: 'scales'
    #> The following object is masked from 'package:viridis':
    #> 
    #>     viridis_pal
    #> The following object is masked from 'package:purrr':
    #> 
    #>     discard
    #> The following object is masked from 'package:readr':
    #> 
    #>     col_factor
    #> 
    #> Attaching package: 'magrittr'
    #> The following objects are masked from 'package:pracma':
    #> 
    #>     and, mod, or
    #> The following object is masked from 'package:purrr':
    #> 
    #>     set_names
    #> The following object is masked from 'package:tidyr':
    #> 
    #>     extract
    #> Loading required package: sysfonts
    #> Loading required package: showtextdb
    #> 
    #> Attaching package: 'showtextdb'
    #> The following object is masked from 'package:extrafont':
    #> 
    #>     font_install
    #> 
    #> Attaching package: 'curl'
    #> The following object is masked from 'package:httr':
    #> 
    #>     handle_reset
    #> The following object is masked from 'package:readr':
    #> 
    #>     parse_date

## Plots

Team wins over/under expected
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/team_wins/wins_above_expected_scatter_2020.png" width="100%" style="display: block; margin: auto;" />
