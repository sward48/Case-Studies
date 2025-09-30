# Case study 03: Fairness of Assessed Value for Hamden, CT
# Benedikt Farag, Elisa Sommer and Steve Ward
# September 2025

print("Running main script...")

system("quarto render scrape_html.qmd")
system("quarto render visualizations.qmd")


# delete the html files and folders created
file.remove("scrape_html.html")
file.remove("visualizations.html")
unlink("scrape_html_files", recursive = TRUE)
unlink("visualizations_files", recursive = TRUE)

