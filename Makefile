packages := avahi bash calf catgen cmake cuda cups emacs env freecad git gnome \
            gnupg hwloc isync java jupyter keras mu nltk qt ssh steam tex \
            vdirsyncer xdg xsettingsd

.PHONY: install uninstall
install:
	stow $(packages)

uninstall:
	stow -D $(packages)
