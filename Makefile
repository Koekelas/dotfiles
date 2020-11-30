packages := avahi bash blueman calf catgen cmake cuda cups emacs env firefox \
            freecad git gnupg hwloc isync java jupyter keras leiningen mu nltk \
            picom qt ssh steam tex vdirsyncer xdg xsettingsd

.PHONY: install uninstall
install:
	stow $(packages)

uninstall:
	stow -D $(packages)
