# robustrank

The file superscript is run to perform Monte Carlo studies in a slurm distributed computing environment. superscript calls the runscript file, which runs partially_matched_test_MC.R using with proper arguments. The Monte Carlo results are saved in files organized into directories. After Monte Carlo is done, open an R console and run partially_matched_test_analysis_letter.R to summary simulation results.
