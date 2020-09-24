packages := avahi bash calf catgen cmake cuda cups emacs env freecad git gnupg \
            hwloc isync java jupyter keras mu nltk picom qt ssh steam tex \
            vdirsyncer xdg xsettingsd

.PHONY: install uninstall
install:
	stow $(packages)

uninstall:
	stow -D $(packages)
