all: mbtiles

.PHONY:mbtiles
mbtiles:
	jbuilder build src/mbtile_viewer/mbtile_viewer.exe
	_build/default/src/mbtile_viewer/mbtile_viewer.exe

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
