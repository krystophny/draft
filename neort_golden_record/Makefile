all: CODE/NEO-RT

CODE/NEO-RT:
	if [ ! -d CODE/NEO-RT ]; then \
		git clone https://github.com/itpplasma/NEO-RT CODE/NEO-RT; \
	else \
		cd CODE/NEO-RT && git pull; \
	fi
