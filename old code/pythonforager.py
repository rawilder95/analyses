 ### PYTHON FORAGER NOTES ### 
# Need to cite the following  
   # Kumar, A.A., Apsel, M., Zhang, L., Xing, N., Jones. M.N. (2023). forager: A Python package and web interface for modeling mental search. 
    # Hills, T. T, Jones, M. N, & Todd, P. M (2012). Optimal foraging in semantic memory. Psychological Review, 119(2), 431–440. 
    # Kumar, A. A, Lundin, N. B, & Jones, M. N (2022). Mouse-mole-vole: The inconspicuous benefit of phonology during retrieval from semantic memory. Proceedings of the Annual Meeting of the Cognitive Science Society. 
    # Troyer A. K, Moscovitch M., Winocur G. (1997). Clustering and switching as two components of verbal fluency: evidence from younger and older healthy adults. Neuropsychology. Jan;11(1):138-46. 
    #  
# 1. data : The --data flag requires you to specify the path to the fluency list file that you would like to execute foraging methods on 
# 2. model: The --model flag requires you to pass one of the following arguments, to run corresponding model(s) you would like to execute. 
# a. static 
# b. dynamic 
# c. pstatic 
# d. pdynamic 
# e. all 
#  
# 3. switch: The --switch flag requires you to pass one of the following arguments to utilize corresponding switch method(s) in the model selected 
# a. troyer 
# b. simdrop 
# c. multimodal 
# d. delta 
# e. all 
# install relevant packages 

# a.  Sample execution with single model and all switches: 
# data:  
# python run_foraging.py  
# data/fluency_lists/data-psyrev.txt 
# # Set directory  
# setwd("/Users/rebeccawilder/Downloads/jatos_mac_java/analyses/forager-master") 
# # install.packages("reticulate") 
library(reticulate)
py_code <- " python run_foraging.py --data data/fluency_lists/data-psyrev.txt --model dynamic --switch all"
py_run_string(py_code)




python
import argparse
from scipy.optimize import fmin
from forager.foraging import forage
from forager.switch import switch_delta, switch_multimodal, switch_simdrop, switch_troyer
from forager.cues import create_history_variables
from forager.utils import prepareData
import pandas as pd
import numpy as np
from scipy.optimize import fmin
import os, sys
from tqdm import tqdm



b. Sample execution with all models and single switch:
  ```
python run_foraging.py --data data/fluency_lists/data-psyrev.txt --model all --switch simdrop
```

c. Running all models and switches
```
python run_foraging.py --data data/fluency_lists/data-psyrev.txt --model all --switch all
```
