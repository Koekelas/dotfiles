packages := avahi bash blueman calf catgen cmake cuda cups emacs env firefox \
            freecad git gnupg hwloc isync java jupyter keras leiningen mu nltk \
            picom qt qtox ssh steam tex vdirsyncer wacom xdg xsettingsd

.PHONY: install uninstall
install:
	stow $(packages)

uninstall:
	stow -D $(packages)
