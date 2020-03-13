TARGET_FIGS := fig/coverage fig/outcomes
TARGETS := $(addsuffix .png, $(TARGET_FIGS)) \
   	$(addsuffix .pdf, $(TARGET_FIGS)) \
   	output/report.txt

all: $(TARGETS)

clean:
	rm -f cache/* fig/* output/*

cache/alpha0.txt: data/raja-data.tsv estimate-alpha0.R util.R
	./estimate-alpha0.R

output/parameters.tsv: cache/alpha0.txt data/raja-data.tsv estimate-parameters.R util.R
	./estimate-parameters.R

cache/estimates.rds cache/variates.rds: output/parameters.tsv draw-variates.R util.R model.R
	./draw-variates.R

fig/coverage.png fig/coverage.pdf: cache/estimates.rds analyze-coverage.R util.R model.R
	./analyze-coverage.R

output/sensitivity.tsv: cache/estimates.rds analyze-sensitivity.R util.R model.R
	./analyze-sensitivity.R

output/outcomes-cis.tsv output/exclusion-counts.tsv fig/outcomes.pdf fig/outcomes.png: cache/variates.rds analyze-bootstraps.R util.R model.R
	./analyze-bootstraps.R

output/report.txt: output/parameters.tsv output/sensitivity.tsv output/outcomes-cis.tsv write-report.R
	./write-report.R
