# Marine Heatwave Size Redistributes U.S. Pelagic Fishing Fleets

**Nima Farchadi, Heather Welch, Camrin D. Braun, Andrew J. Allyn, Steven J. Bograd, Stephanie Brodie, Elliott L. Hazen, Alex Kerney, Nerea Lezama-Ochoa, Katherine E. Mills, Dylan Pugh, Riley Young-Morse, Rebecca L. Lewison**

# Overview

This repository contains code to:

1. Download publicly available historical sea surface temperature data from OISST and characterize Marine Heatwave (MHW) properties (intensity, size, duration).

2. Developing vessel distribution models (downloading environmental data from HYCOM, fitting boosted regression trees, model validation, and prediction) and classifying core fishing grounds

3. Measure fleet responses to marine heatwaves: change in core fishing area and fleet displacement.

Majority of the analysis is performed separately for the U.S. Atlantic longline fishery in the northwest Atlantic (NWA_PLL) and the U.S. Pacific troll fishery in the northeast Pacific (NEP_TROLL). For example, for each analysis there will be separate folder for each fleet. However, fleet response analysis is performed together. 


**This repository is organized as follows:**

- `scripts` contains all code to download, wrangle, and analyze the data. Scripts are separated into subfolders that correspond to the analysis they were used for. Subfolders are numbered in the order the analysis should be performed. Also contains code to reproduce plots. 

- `functions` contains homemade functions called in `scripts`. Each `scripts` subfolder has its own designated `functions` folder.

- `data` is the directory where all of the data used in the analysis will live. This includes raw and processed AIS data, models and validations, model predictions, all environmental data, and summary metrics. Due to limited storage, large files (e.g. environmental data, AIS data, spatial predictions) are not version controlled and thus many of the diectroies for these data are not shown.
    - *Available data for users:*
        - MHW property and fleet response metrics in `mgmt_area_metrics` folder. This contains all the data needed to replicate fleet response analysis. 
        - `shapefiles` of each fishing fleets management areas.
        - Boosted Regresssion Trees models (i.e. vessel distribution models; VDM) and validation metrics for each fishing fleet in the `VDM_and_val` folder.

- `Plots` contains .png files of all the figures in the manuscript. 


# Related Products

This research was part of the **Fisheries and Climate Toolkit (FaCeT)** which is a NASA-funded project that is developing an online paltform for fisheries stakeholders to vizualize and explore how climate change will impact highly migratory species and fisheries. FaCeT bridges the gap between fisheries and climate science to support climate resilient and sustainable fisheries. 

To learn more about FaCeT and to interact with developed tools on the dashboard (such as the results from this study) explore the two links below: 
- [FaCeT project website](https://fisheriesclimatetoolkit.sdsu.edu/)
- [FaCeT dashboard website](https://facet.research.gmri.io/)

# Questions and Feedback

If you encounter with any problems or error, please create an issue here. Starting an issue will help other trying to use this code. With any other questions please contact the lead other, N. Farchadi, at nfarchadi@sdsu.edu