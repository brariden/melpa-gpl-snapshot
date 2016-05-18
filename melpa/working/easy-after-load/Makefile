CARTON ?= carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: win script

win:
	${CARTON} exec ${ECUKES} features

script:
	${CARTON} exec ${ECUKES} --script features

carton:
	${CARTON} install

travis: carton win script
