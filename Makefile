all: deps
	(cd src;$(MAKE))

deps:
	cp ../elog/ebin/* ebin
	cp ../elog/include/* include 

clean:
	(cd src;$(MAKE) clean)

