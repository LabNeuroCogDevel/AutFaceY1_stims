all: 1D

1D: timing.txt
	./02_write1Ds.R

timing.txt: getdata
	./01_timing.bash

getdata:
	./00_getData.bash

clean:
	rm -r Y1
