.PHONY : usage test upload

usage:
	@echo "Usage: test|upload"

test:
	R -e "shiny::runApp('.')"

upload:
	scp ui.R ryanhope@glimmer.rstudio.com:~/ShinyApps/hops/
	scp server.R ryanhope@glimmer.rstudio.com:~/ShinyApps/hops/
	scp hops-comp-wide.csv ryanhope@glimmer.rstudio.com:~/ShinyApps/hops/
