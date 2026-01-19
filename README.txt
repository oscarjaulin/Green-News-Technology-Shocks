============================================================
Replication Package for: Becoming Green: Decomposing the Macroeconomic Effects of Green Technology News Shocks
Authors: Jaulín-Méndez, Oscar; Ramos, Andrey
Date: [January 2026]
============================================================

1. Overview
-----------
This replication package contains all the necessary files to reproduce the results presented in the paper 
"Becoming Green: Decomposing the Macroeconomic Effects of Green Technology News Shocks". 
The package includes data and code files. All the calculations were computed in R.

2. Contents
-----------
The package includes the following folders and files:

- Quarter_dat_Long.xlsx 	→ Raw and processed datasets used in the analysis

- MAIN.r, SOURCE_BVAR.r,        → Scripts for estimation, and plotting
  SOURCE_LP.R, Figure_.r
  Figure_2.r, Figure_9.r,
  Figure_A1.r, Figure_gfvsngf.r 
            
- Names.xlsx              	→ Long names of variables in the estimation
- README.txt          		→ This file

3. Requirements
---------------
These estimations were performed using:

- R 4.3.2
- Required packages:
  	- readxl
	- BVAR
	- plm
	- zoo
	- ggplot2
	- lpirfs
	- seasonal
	- sandwich
	- lmtest
	- boot
- Operating system: Windows 11.

4. Instructions
---------------
To replicate the results:

1. Open the MAIN.r script
2. Run the script sequentially. It will:
   - Load and process the data
   - Estimate the BVAR and LPs.
   - Generate figures.

5. Notes
--------
- All data used are publicly available.
- If you encounter issues, check that all required packages are installed.
- For questions or feedback, contact: ojaulime@banrep.gov.co, andrey.ramos@bde.es

6. Citation
-----------
If you use this code or data, please cite the paper as:

Jaulin, O., & Ramos, A. (2025). Becoming Green: Decomposing the Macroeconomic Effects of Green Technology News Shocks. arXiv preprint arXiv:2507.18386.

============================================================