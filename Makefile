all: mbtiles

.PHONY:mbtiles
mbtiles:
	jbuilder build src/mbtiles_viewer/mbtiles_viewer.exe
	_build/default/src/mbtiles_viewer/mbtiles_viewer.exe

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
