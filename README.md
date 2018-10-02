# robustrank

These files were used in the Monte Carlo studies in Fong, Huang, Lemos, McElrath (2018). The file named superscript is run to perform Monte Carlo studies in a slurm distributed computing environment. superscript calls the files named runscript, which runs partially_matched_test_MC.R with proper arguments. The Monte Carlo results are saved in files organized into directories. After Monte Carlo is done, open an R console and run partially_matched_test_analysis_letter.R to summary simulation results.


References:

Fong, Huang, Lemos, McElrath (2018) Response to Guo et al.'s letter to the editor. Biostatistics, accepted.
