# Case study 03: Fairness of Assessed Value for Hamden, CT
# Benedikt Farag, Elisa Sommer and Steve Ward
# September 2025

# Files not in the repo (from sharepoint): Both to go under rawdata/
# 1) `Connecticut_CAMA_and_Parcel_Layer_3895368049124948111.csv`: property data 
#    for CT from the link: 
#    https://yaleedu-my.sharepoint.com/:u:/g/personal/brian_macdonald_yale_edu/EVAE5NykXsdLlqI4ufT6nBwBA0_oJh1jqjPioEMQ_G7Fmg?e=qoYauD
# 2) Folder `Hamden_Sept2025`: Downloaded from the provided zip file: 
#    https://yaleedu-my.sharepoint.com/:u:/g/personal/brian_macdonald_yale_edu/ERDOKwhS_s1Bn2f3GFAg5gABBrtaf-g-bAsOetpW44j7yA?e=QHhBRf


print("Running main script...")

system("quarto render scrape_html.qmd")
system("quarto render visualizations.qmd")


# delete the html files and folders created
file.remove("scrape_html.html")
file.remove("visualizations.html")
unlink("scrape_html_files", recursive = TRUE)
unlink("visualizations_files", recursive = TRUE)

